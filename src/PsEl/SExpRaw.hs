{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module PsEl.SExpRaw where

import Data.Functor.Foldable qualified as RS
import PsEl.SExp (Symbol)
import RIO

newtype SExp = SExp {unSExpr :: SExpF SExp}

type instance RS.Base SExp = SExpF
instance RS.Recursive SExp where
    project = unSExpr

data SExpF e
    = Integer Integer
    | Double Double
    | String Text
    | Character Char
    | Symbol Symbol
    | Cons e e
    | List [e]
    | Vector [e]
    | Quote e
    | Backquote e
    | Comma e
    deriving (Functor, Foldable, Traversable)

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
