{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module PsEl.SExpOptimize where

import Control.Lens (itoListOf)
import Data.Functor.Foldable (Recursive (para), cata, cataA)
import Data.Generics.Sum (_Ctor)
import Data.Generics.Wrapped (_Unwrapped)
import Language.PureScript qualified as PS
import PsEl.SExp
import PsEl.SExpPattern qualified as P
import PsEl.SExpTraverse (Index (..), freeVars)
import PsEl.Transpile (globalVar, localUnusedVar)
import RIO
import RIO.Lens (_1)
import RIO.Map qualified as Map
import RIO.State (State, evalState, get, put)

newtype OptimizeM a = OptimizeM {runOptimize :: State Int a}
    deriving (Functor, Applicative, Monad)

-- PSが $ prefixの変数を使わない,という前提
mkUniqVar :: Text -> OptimizeM Symbol
mkUniqVar base = OptimizeM $ do
    i <- get
    put $ i + 1
    pure $ UnsafeSymbol $ "$" <> base <> "-" <> textDisplay i

optimize :: Feature -> Feature
optimize f@Feature{defVars} =
    f
        { defVars =
            map
                (\dv -> dv{definition = optimizeDefVar dv})
                defVars
        }

optimizeDefVar :: DefVar -> SExp
optimizeDefVar DefVar{name, definition} = flip evalState 0 . runOptimize $ do
    pure (applyRewrite definition)
        >>= applyTopLevelTCO name
        >>= applyTCO
  where
    applyRewrite :: SExp -> SExp
    applyRewrite =
        cata
            ( magicDo
                . directlyCallFns
                . removeDataFunctions
                . removeRebindOnlyPcase
                . replacePcaseToIf
                . SExp
            )

    applyTopLevelTCO :: Symbol -> SExp -> OptimizeM SExp
    applyTopLevelTCO name sexp =
        fromMaybe (pure sexp) $ applyTCOToName name sexp

    -- top-downに適用する必要あり
    applyTCO :: SExp -> OptimizeM SExp
    applyTCO = para applyTCO'

    -- only letrec names are possibily self-resursive
    applyTCO' :: SExpF (SExp, OptimizeM SExp) -> OptimizeM SExp
    applyTCO' (Let LetRec binds body) = do
        binds' <- for binds $ \(name, (orig, sexp)) ->
            (name,) <$> case applyTCOToName name orig of
                Nothing -> sexp
                Just om -> applyTCO =<< om
        body' <- snd body
        pure . SExp $ Let LetRec binds' body'
    applyTCO' sexp =
        SExp <$> mapM snd sexp

    applyTCOToName :: Symbol -> SExp -> Maybe (OptimizeM SExp)
    applyTCOToName name sexp =
        case toLambdas sexp of
            ([], _) ->
                Nothing
            lambdas ->
                fmap (fmap fromLambdas) (selfRecursiveTCO name lambdas)

    toLambdas :: SExp -> ([Symbol], SExp)
    toLambdas (SExp (Lambda1 sym sexp)) = over _1 (sym :) $ toLambdas sexp
    toLambdas sexp = ([], sexp)

    fromLambdas :: ([Symbol], SExp) -> SExp
    fromLambdas (args, body) = foldr lambda1 body args

-- PSでif文を使うとcorefnではcaseに変換される。素直にそのままコード生成すると例
-- えばのようになる。
--
-- (pcase b ((pred identity) 1) (_ 0))
--
-- 上記ケースは (if b 1 0) に変換できる。ただpaseマクロが次まで展開してくれる。
--
-- (if (identity b) (let nil 1) (let nil 0))
--
-- そのため速度的な改善はそれほどでもない。
replacePcaseToIf :: SExp -> SExp
replacePcaseToIf
    ( P.Pcase
            [e]
            [PcaseAlt [PPredBool b] Nothing thenE, PcaseAlt [PAny] Nothing elseE]
        ) = if' (bool (funcallNative "not" [e]) e b) thenE elseE
replacePcaseToIf s = s

-- 変数のbindingのみ行なってる単一Altのpcase式の除去。
-- 例えば:
--
--   selfRec1 :: Int -> Int -> Int
--   selfRec1 i to
--     | eqInt i to = 1
--     | true = selfRec1 (succInt i) to
--
-- はそのままコード変換すると次のようになる。
--
--   (defvar Test.PsEl.TCO.selfRec1
--     (lambda (i)
--       (lambda (to)
--         (pcase (list i to)
--           (`(,i1 ,to1)
--            (cond ((funcall (funcall Test.Util....
--
-- pcaseは単に変数を再束縛しているだけであり,変数名を置き換えをすれば除去可能である。
-- PSレベルで行なえる変換に思えるが,CoreFnデータ型ににcond(ifの連結)に相当するものがないからかな？
removeRebindOnlyPcase :: SExp -> SExp
removeRebindOnlyPcase (P.Pcase exps [PcaseAlt pats Nothing code])
    | Just expsSyms <- traverse (^? _Unwrapped . _Ctor @"Symbol") exps
      , Just patsSyms <- traverse (^? _Ctor @"PBind") pats =
        code
            & over freeVars (symbolsRewrite (zip patsSyms expsSyms))
removeRebindOnlyPcase s = s

-- 不要な ($), (&) の除去
-- ほぼ意味がなく結合優先度や適用順序を調整するために使われる。
-- prelude の Data.Functinモジュールの apply($) 及び applyFlipped(&)である。
--
--   apply :: forall a b. (a -> b) -> a -> b
--   apply f x = f x
--
-- (funcall apply <a->b>) -> <a->b>
--
--   applyFlipped :: forall a b. a -> (a -> b) -> b
--   applyFlipped x f = f x
--
-- (funcall (funcall applyFlipped <a>) <a->b>)  ->  (funcall <a->b> <a>)
--
-- ??? ModuleNameやIdentはLanguage.PureScript.Constants以下で定義されている？
removeDataFunctions :: SExp -> SExp
removeDataFunctions (P.FunCall1 (P.Symbol f) a)
    | f == globalVar moduleDataFunction (PS.Ident "apply") = a
removeDataFunctions (P.FunCall1 (P.FunCall1 (P.Symbol f) a) ab)
    | f == globalVar moduleDataFunction (PS.Ident "applyFlipped") = funcall1 ab a
removeDataFunctions s = s

moduleDataFunction :: PS.ModuleName
moduleDataFunction = PS.ModuleName "Data.Function"

-- MagicDo
--
-- 参考までに pskt での実装方法
-- https://github.com/csicar/pskt/blob/aa0d5df52e9579abd38061c6ab891489ebf295c4/src/CodeGen/MagicDo.hs#L45
-- fuse0 で (funcall0 (lambda0 ..) を除去しているので(多分)bottomupに適用しても問題ないはず。
--
--   bind :: forall a b. m a -> (a -> m b) -> m b
--
-- (funcall (funcall (funcall Control.Bind.bind Effect.bindEffect) <m a>) (lambda (arg) <m b>))
--
-- if arg is unused, then:
--
-- (lambda ()
--   (funcall <m a>)
--   (funcall <m b>))
--
-- or else:
--
-- (lambda ()
--   (let* ((arg (funcall <m a>)))
--     (funcall <m b>)))
--
-- 捨てられる場合
--
--  discard :: forall f b. Bind f => f a -> (a -> f b) -> f b
--
-- (funcall (funcall (funcall (funcall Control.Bind.discard Control.Bind.discardUnit) Effect.bindEffect) <m a>) (lambda (arg) <m b>))
-- ->
-- (lambda ()
--   (funcall <m a>)
--   (funcall <m b>))
--
-- pure
--
--   pure :: forall a. a -> f a
--
-- (funcall (funcall Control.Applicative.pure Effect.applicativeEffect) <v>)
-- ->
-- (lambda () v)
--
-- NOTE: psktと同様 while や untilもやるべきかな。
-- あともう少しパターンマッチにしやすいよう Symbolの型変えるか？
magicDo :: SExp -> SExp
magicDo (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.Symbol bind) (P.Symbol bindEffect)) ma) (P.Lambda1 arg mb))
    | bind == identBind && bindEffect == identBindEffect =
        if arg == localUnusedVar
            then lambda0 $ progn [fuse0 (funcall0 ma), fuse0 (funcall0 mb)]
            else lambda0 $ letStar [(arg, fuse0 (funcall0 ma))] (fuse0 (funcall0 mb))
magicDo (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.Symbol discard) (P.Symbol discardUnit)) (P.Symbol bindEffect)) ma) (P.Lambda1 _ mb))
    | discard == identDiscard && discardUnit == identDiscardUnit && bindEffect == identBindEffect =
        lambda0 $ progn [fuse0 (funcall0 ma), fuse0 (funcall0 mb)]
magicDo (P.FunCall1 (P.FunCall1 (P.Symbol pure) (P.Symbol applicativeEffect)) v)
    | pure == identPure && applicativeEffect == identApplicativeEffect =
        lambda0 v
magicDo s = s

-- NOTE: (lambda0 (funcall0 ..)) もなくせるかと思うが多分そのようなケースは発生しない
-- NOTE: 良く考えたら上記のケースは (lambda0 (funcall 0 (let ((..)) x))) のケースが
-- あるから多分fuseは駄目な気がする
-- magicDo が cata で bottomup に適用されているので再帰除去は不要。
fuse0 :: SExp -> SExp
fuse0 (P.FunCall0 (P.Lambda0 a)) = a
fuse0 s = s

identBind = globalVar moduleControlBind (PS.Ident "bind")
identPure = globalVar moduleControlApplicative (PS.Ident "pure")
identDiscard = globalVar moduleControlBind (PS.Ident "discard")
identDiscardUnit = globalVar moduleControlBind (PS.Ident "discardUnit")
identBindEffect = globalVar moduleEffect (PS.Ident "bindEffect")
identApplicativeEffect = globalVar moduleEffect (PS.Ident "applicativeEffect")
moduleEffect = PS.ModuleName "Effect"
moduleControlApplicative = PS.ModuleName "Control.Applicative"
moduleControlBind = PS.ModuleName "Control.Bind"

-- 引数が全部与えられていた場合, FnN{0,..,10} を直接呼ぶ

directlyCallFns :: SExp -> SExp
directlyCallFns (P.FunCall1 (P.Symbol runFn0) f)
    | runFn0 == identRunFn 0 = funcallN f []
directlyCallFns (P.FunCall1 (P.FunCall1 (P.Symbol runFn1) f) a0)
    | runFn1 == identRunFn 1 = funcallN f [a0]
directlyCallFns (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.Symbol runFn2) f) a0) a1)
    | runFn2 == identRunFn 2 = funcallN f [a0, a1]
directlyCallFns (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.Symbol runFn3) f) a0) a1) a2)
    | runFn3 == identRunFn 3 = funcallN f [a0, a1, a2]
directlyCallFns (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.Symbol runFn4) f) a0) a1) a2) a3)
    | runFn4 == identRunFn 4 = funcallN f [a0, a1, a2, a3]
directlyCallFns (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.Symbol runFn5) f) a0) a1) a2) a3) a4)
    | runFn5 == identRunFn 5 = funcallN f [a0, a1, a2, a3, a4]
directlyCallFns (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.Symbol runFn6) f) a0) a1) a2) a3) a4) a5)
    | runFn6 == identRunFn 6 = funcallN f [a0, a1, a2, a3, a4, a5]
directlyCallFns (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.Symbol runFn7) f) a0) a1) a2) a3) a4) a5) a6)
    | runFn7 == identRunFn 7 = funcallN f [a0, a1, a2, a3, a4, a5, a6]
directlyCallFns (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.Symbol runFn8) f) a0) a1) a2) a3) a4) a5) a6) a7)
    | runFn8 == identRunFn 8 = funcallN f [a0, a1, a2, a3, a4, a5, a6, a7]
directlyCallFns (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.Symbol runFn9) f) a0) a1) a2) a3) a4) a5) a6) a7) a8)
    | runFn9 == identRunFn 9 = funcallN f [a0, a1, a2, a3, a4, a5, a6, a7, a8]
directlyCallFns (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.FunCall1 (P.Symbol runFn10) f) a0) a1) a2) a3) a4) a5) a6) a7) a8) a9)
    | runFn10 == identRunFn 10 = funcallN f [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9]
directlyCallFns s = s

identRunFn :: Int -> Symbol
identRunFn i = globalVar moduleFunctionUncurried (PS.Ident $ "runFn" <> textDisplay i)
moduleFunctionUncurried = PS.ModuleName "Data.Function.Uncurried"

-- 自己再帰関数の最適化
-- ただし最適化可能なケース全てに於いて最適化を実行できるわけではない。
-- 差し当り自明なケースのみ最適化を実行する。
-- トップダウンに適用する必要あり。whileループに変換すると
-- 変換後のコードでは末尾呼出し検出がtrue/negativeになってしまう。
--
-- sym 現在最適化対象の識別子
-- args は少なくとも一つ以上持つとする
-- body は関数の中身
--
selfRecursiveTCO :: Symbol -> ([Symbol], SExp) -> Maybe (OptimizeM ([Symbol], SExp))
selfRecursiveTCO sym (args, body) = do
    let argLen = length args
    let calls = filter ((== sym) . snd) $ itoListOf freeVars body
    if not (null calls) && all (isTC argLen . fst) calls
        then Just $ (args,) <$> performTCO
        else Nothing
  where
    -- 末尾呼出しかを判定。
    -- 必要な1引数呼出し回数が第一引数で渡される。
    -- 第二引数の[Index]は逆順であることに注意(内から外の順)。
    --
    -- (a)
    -- 呼出し途中にある識別子に束縛された場合。現在は単に末尾呼出しされていないとしているが,
    -- その識別子が(元bodyから見て)末尾呼出しされている場合は最適化可能なケースが存在する。
    -- 実際JSバックエンドはそこまで最適化している。
    --
    -- (b)
    -- 引数位置にあったとしても最適化可能なケースは存在するが解析が難しいため現在はしない。
    --
    -- (c)
    -- 本来必要な引数の数を超えて呼び出されるケースは存在する。
    -- 例えば foo :: Int -> Int -> Int という型でも実装が foo i = if ... (\j -> i + j)
    -- のような形であった場合, 元の args引数の長さは1になる。
    --
    -- ?? Lambda0/FunCall0 は直Falseにしなくても救済措置があるかも。
    -- ただ実際に有り得るかは疑問。
    isTC :: Int -> [Index] -> Bool
    isTC i [] = i == 0
    isTC i (ix : ixs) = case ix of
        ILambda1 -> isTC (i + 1) ixs
        ILambda0 -> False
        IBind _ -> False -- (a)
        IArg -> False -- (b)
        ICond -> False
        ITail -> isTC i ixs
        IFunCall1
            | i > 0 -> isTC (i - 1) ixs
            | otherwise -> False -- (c)
        IFunCall0 -> False
        IFunCallN -> False

    -- 例えば次の関数が最適化対象とする。
    --
    --  (lambda (i)
    --    (lambda (j)
    --      <..body..>))
    --
    -- 次のようにwhile文を使ったループに変更する。
    -- 生成変数($ prefix)は一意性を持つよう被らないsuffixを付ける。
    -- <..body'..> は <..body..>内の再帰呼出しを $loop-fn に置き換え,
    -- lambda引数をローカルの物に置き換えたもの。
    -- 現状再帰呼出し関数をshadowすれば置き換えは不要だが,
    -- 将来的に部分的に置き換えの必要が発生した場合も考えて全て置き換えておく。
    -- lambda引数をローカルに置き換える(下の例だと $i, $j)のは,
    -- 関数ば部分適用された場合,lambda引数を直でsetqすると呼出し間でその
    -- 変更が共有されてしまうため。
    --
    --  (lambda (i)
    --    (lambda (j)
    --      (let* (($i i)
    --             ($j j)
    --             ($continue (make-symbol ""))  ;; uniq symbol
    --             ($result $do-loop)
    --             ($loop-fn (lambda (_i)
    --                         (lambda (_j)
    --                           (setq $i _i $j _j)
    --                           $continue))))
    --        (while (eq $result $continue)
    --          (setq $result <..body'..>))
    --        $result)))
    --
    performTCO :: OptimizeM SExp
    performTCO = do
        varContinue <- mkUniqVar "continue"
        varResult <- mkUniqVar "result"
        varLoopFn <- mkUniqVar $ symbolText sym
        argsLocal <- traverse (mkUniqVar . symbolText) args
        argsTmp <- traverse (mkUniqVar . symbolText) args
        let bindLocalVars =
                zipWith (curry (fmap symbol)) argsLocal args
        let bindSpecialVars =
                [ (varContinue, funcallNative "make-symbol" [string ""])
                , (varResult, symbol varContinue)
                ,
                    ( varLoopFn
                    , foldr
                        lambda1
                        ( progn2
                            (funcallNative "setq" (zipWith (\a a' -> [symbol a, symbol a']) argsLocal argsTmp & mconcat))
                            (symbol varContinue)
                        )
                        argsTmp
                    )
                ]
        let body' =
                body & over freeVars (symbolsRewrite ((sym, varLoopFn) : zip args argsLocal))
        pure $
            letStar
                (bindLocalVars <> bindSpecialVars)
                ( progn2
                    ( funcallNative
                        "while"
                        [ funcallNative "eq" [symbol varResult, symbol varContinue]
                        , funcallNative "setq" [symbol varResult, body']
                        ]
                    )
                    (symbol varResult)
                )
      where
        progn2 e0 e1 = progn [e0, e1]

symbolsRewrite :: [(Symbol, Symbol)] -> Symbol -> Symbol
symbolsRewrite symPairs =
    let symMap = Map.fromList symPairs
     in \sym -> fromMaybe sym (Map.lookup sym symMap)
