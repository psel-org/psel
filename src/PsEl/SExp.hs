{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module PsEl.SExp where

import Data.Functor.Foldable qualified as RS
import RIO
import RIO.Lens (_1, _2)
import RIO.NonEmpty qualified as NonEmpty
import RIO.Text (unpack)

-- Fix Point
newtype SExp = SExp {unSExpr :: SExpF SExp}

type instance RS.Base SExp = SExpF
instance RS.Recursive SExp where
    project = unSExpr

-- Expression Functor
-- 制御構造(let*,letrec,cond,pcase)は List でも表現できるが,
-- SExpに対して最適化をかける際に解析する必要があるため別途コンストラクタで表現。
-- Listは関数呼出しもしくはリテラル時のみ使う。
data SExpF e
    = Integer Integer
    | Double Double
    | String Text
    | Character Char
    | Symbol Symbol
    | Cons e e
    | List [e]
    | Vector [e]
    | If e e e
    | Cond [(e, e)]
    | Let LetType [(Symbol, e)] [e]
    | Pcase [e] [PcaseAlt e]
    | -- | 基本一引数のlambdaしか使わないので
      Lambda1 Symbol [e]
    | -- | e.g. '(foo 2)
      Quote e
    | -- | e.g. `(foo 1)
      Backquote e
    | -- | e.g. `(,a)
      Comma e
    deriving (Functor, Foldable, Traversable)

data LetType
    = LetStar
    | LetRec
    deriving (Eq, Ord)

data PcaseAlt e = PcaseAlt
    { patterns :: [PPattern e]
    , guard :: Maybe e
    , code :: e
    }
    deriving (Functor, Foldable, Traversable)

-- pcase pattern
-- 相互再帰の recursion scheme は面倒なのでこちらは普通に定義。
-- guard用のconstructerを用意していないのは, guardを使うのは最も外側のみのため。
-- コンストラクタを減らしたほうが解析しやずいはず。
data PPattern e
    = PAny
    | PInteger Integer
    | PString Text
    | PCharacter Char
    | PBind Symbol
    | -- | pattern中基本裸の Symbol は束縛を意味するが,
      -- backquote中では定数を意味するSymbolも出現する。
      PBackquotedList [Either Symbol (PPattern e)]
    | PBackquotedVector [Either Symbol (PPattern e)]
    | PAnd [PPattern e]
    | PPred e
    | -- | Boolは特別扱い(最適化でif文に変更しうるので)
      -- 本来は以下のようにしていた
      -- list [symbol "pred", bool (symbol "null") (symbol "identity") b]
      PPredBool Bool
    | PApp e (PPattern e)
    deriving (Functor, Foldable, Traversable)

-- Unsafe prefixは任意のテキストが妥当なelispシンボルにならないことを示している。
newtype Symbol = UnsafeSymbol Text
    deriving (Eq, Ord, IsString)

integer :: Integer -> SExp
integer = SExp . Integer

double :: Double -> SExp
double = SExp . Double

string :: Text -> SExp
string = SExp . String

character :: Char -> SExp
character = SExp . Character

symbol :: Symbol -> SExp
symbol = SExp . Symbol

vector :: [SExp] -> SExp
vector = SExp . Vector

-- e.g. (car . cdr)
cons :: SExp -> SExp -> SExp
cons car cdr = SExp $ Cons car cdr

list :: [SExp] -> SExp
list = SExp . List

quote :: SExp -> SExp
quote = SExp . Quote

backquote :: SExp -> SExp
backquote = SExp . Backquote

comma :: SExp -> SExp
comma = SExp . Comma

-- 文字列や数値などのリテラル表記かを判定。
-- vector(e.g. [1 2 3])もリテラルであることに注意。
isLiteral :: SExp -> Bool
isLiteral (SExp s') = case s' of
    Integer _ -> True
    Double _ -> True
    String _ -> True
    Character _ -> True
    Vector _ -> True
    _ -> False

-- association list(e.g. `((foo . ,v) (bar . 2))
-- don't need comma
alist :: [(Symbol, SExp)] -> SExp
alist =
    backquote
        . list
        . map (uncurry cons . over _2 comma' . over _1 symbol)
  where
    comma' s
        | isLiteral s = s
        | otherwise = comma s

cond :: [(SExp, SExp)] -> SExp
cond = SExp . Cond

letStar :: [(Symbol, SExp)] -> [SExp] -> SExp
letStar bindings body = SExp $ Let LetStar bindings body

letRec :: [(Symbol, SExp)] -> [SExp] -> SExp
letRec bindings body = SExp $ Let LetRec bindings body

pcase :: [SExp] -> [PcaseAlt SExp] -> SExp
pcase exps cases = SExp $ Pcase exps cases

lambda1 :: Symbol -> [SExp] -> SExp
lambda1 arg body = SExp $ Lambda1 arg body

-- e.g. (lambda (a) (lambda (b) ...))
lambdaN :: NonEmpty Symbol -> [SExp] -> SExp
lambdaN args body =
    let a0 :| as = NonEmpty.reverse args
     in foldl' (\body arg -> lambda1 arg [body]) (lambda1 a0 body) as

--
data DefVar = DefVar
    { name :: Symbol
    , definition :: SExp
    }

newtype FeatureName = FeatureName Symbol

-- Feature (Emacs requirable file)
data Feature = Feature
    { name :: FeatureName
    , requires :: [FeatureName]
    , requireFFI :: Maybe (FeatureName, [Symbol])
    , defVars :: [DefVar]
    }

featureFileName :: FeatureName -> String
featureFileName (FeatureName (UnsafeSymbol s)) =
    unpack s <> ".el"
