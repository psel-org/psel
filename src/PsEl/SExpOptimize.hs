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
import PsEl.SExp
import PsEl.SExpTraverse (Index (..), freeVars)
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
            . SExp
            . removeRebindOnlyPcase
            $ replacePcaseToIf s

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
replacePcaseToIf :: SExpF SExp -> SExpF SExp
replacePcaseToIf
    ( Pcase
            [e]
            [PcaseAlt [PPredBool b] Nothing thenE, PcaseAlt [PAny] Nothing elseE]
        ) = If (bool (funcallNative "not" [e]) e b) thenE elseE
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
removeRebindOnlyPcase :: SExpF SExp -> SExpF SExp
removeRebindOnlyPcase (Pcase exps [PcaseAlt pats Nothing code])
    | Just expsSyms <- traverse (^? _Unwrapped . _Ctor @"Symbol") exps
      , Just patsSyms <- traverse (^? _Ctor @"PBind") pats =
        code
            & over freeVars (symbolsRewrite (zip patsSyms expsSyms))
            & view _Unwrapped
removeRebindOnlyPcase s = s

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
    -- 必要な呼出し回数が第一引数で渡される。
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
    isTC :: Int -> [Index] -> Bool
    isTC i [] = i == 0
    isTC i (ix : ixs) = case ix of
        ILambda1 -> isTC (i + 1) ixs
        IBind _ -> False -- (a)
        IArg -> False -- (b)
        ICond -> False
        IBody1 -> isTC i ixs
        IFunCall1
            | i > 0 -> isTC (i - 1) ixs
            | otherwise -> False -- (c)

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
        -- TODO: Replace with proper SExp constructor
        progn2 e0 e1 = funcallNative "progn" [e0, e1]

symbolsRewrite :: [(Symbol, Symbol)] -> Symbol -> Symbol
symbolsRewrite symPairs =
    let symMap = Map.fromList symPairs
     in \sym -> fromMaybe sym (Map.lookup sym symMap)
