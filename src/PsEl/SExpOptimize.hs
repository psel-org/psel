{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module PsEl.SExpOptimize where

import Data.Functor.Foldable (cataA)
import PsEl.SExp
import RIO

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
        pure . SExp $ optimizeIf s

-- PSでif文を使うとcorefnではcaseに変換される。素直にそのままコード生成すると例
-- えばのようになる。
--
-- (pcase b ((pred identity) 1) (_ 0))
--
-- 上記ケースは (if b 1 0) に変換できる。ただpaseマクロが次まで展開してくれる。
--
-- (if (identity b) (let nil 1) (let nil 0))
--
-- そのためあまり
optimizeIf :: SExpF SExp -> SExpF SExp
optimizeIf
    ( Pcase
            [e]
            [PcaseAlt [PPredBool b] Nothing thenE, PcaseAlt [PAny] Nothing elseE]
        ) = If (bool (list [symbol "not", e]) e b) thenE elseE
optimizeIf s = s
