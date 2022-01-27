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
import Data.Functor.Foldable (Recursive (para), cataA)
import Data.Generics.Sum (_Ctor)
import Data.Generics.Wrapped (_Unwrapped)
import Language.PureScript qualified as PS
import PsEl.SExp
import PsEl.SExpPattern qualified as P
import PsEl.SExpTraverse (Index (..), freeVars)
import PsEl.Transpile (globalVar)
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
    cataA optimize' definition
        >>= applyTopLevelTCO name
        >>= applyTCO
  where
    optimize' :: SExpF (OptimizeM SExp) -> OptimizeM SExp
    optimize' s = do
        s <- sequence s
        pure
            . removeDataFunctions
            . removeRebindOnlyPcase
            . replacePcaseToIf
            $ SExp s

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
-- ASTの構造を書き換えるためTCOと同じくトップダウンに適用する必要あり。
-- FunCall0 と Lambda0 が必要。
magicDo :: SExp -> SExp
magicDo = para go
  where
    go :: SExpF (SExp, SExp) -> SExp
    go v = SExp $ snd <$> v

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
