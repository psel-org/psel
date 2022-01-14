{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module PsEl.SExpOptimize where

import Data.Functor.Foldable (cataA)
import Data.Generics.Sum (_Ctor)
import Data.Generics.Wrapped (_Unwrapped)
import PsEl.SExp
import PsEl.SExpTraverse (freeVars)
import RIO
import RIO.Map qualified as Map

newtype OptimizeM a = Optimize {runOptimize :: Identity a}
    deriving (Functor, Applicative, Monad)

optimize :: Feature -> Feature
optimize f@Feature{defVars} =
    f
        { defVars =
            map
                (\dv@DefVar{definition} -> dv{definition = optimizeSExp definition})
                defVars
        }

optimizeSExp :: SExp -> SExp
optimizeSExp sexp =
    runIdentity . runOptimize $ cataA optimize' sexp
  where
    optimize' :: SExpF (OptimizeM SExp) -> OptimizeM SExp
    optimize' s = do
        s <- sequence s
        pure
            . SExp
            . removeRebindOnlyPcase
            $ replacePcaseToIf s

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
        let symMap = Map.fromList $ zip patsSyms expsSyms
            symRewrite sym = fromMaybe sym $ Map.lookup sym symMap
         in code
                & over freeVars symRewrite
                & view _Unwrapped
removeRebindOnlyPcase s = s
