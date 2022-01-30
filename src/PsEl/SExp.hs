{-# LANGUAGE DeriveGeneric #-}
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
    deriving (Generic)

type instance RS.Base SExp = SExpF
instance RS.Recursive SExp where
    project = unSExpr

-- Expression Functor
-- 制御構造(let*,letrec,cond,pcase)は List でも表現できるが,
-- SExpに対して最適化をかける際に解析する必要があるため別途コンストラクタで表現。
-- Listは関数呼出しもしくはリテラル時のみ使う。
--
-- S式とCoreFnの間の中間言語を目指す?
-- 一般的なS式までトランスパイル時に落してしまうと情報も落ちてしまって最適化がしづらくなる。
-- 例えば Quote/Backquote/Comm。自由変数の参照を取得する場合,quote中のシンボルは無視する必要がある。
-- またbackquoteがネストされた場合,そのコンテキストも考慮する必要がある。
-- Quote/Backqoute自体は限定的に使われているので,より情報を残した形で
-- SExpFから除くというのが正しい気もするが
-- Quote もSymbolにしか適用されていないので Quote e よりは QuotedSymbol Symbol として方が
--
-- ただ懸念としてはあまり上げすぎるとCoreFnと同じレベルになってしまう。
--
-- Lambda0/FunCall0はMagicDo実装のために導入
data SExpF e
    = Integer Integer
    | Double Double
    | String Text
    | Character Char
    | Symbol Symbol
    | MkAlist [(Symbol, e)]
    | Progn [e]
    | If e e e
    | Cond [(e, e)]
    | Let LetType [(Symbol, e)] e
    | Pcase [e] [PcaseAlt e]
    | -- | 1引数関数
      Lambda1 Symbol e
    | -- | 1引数関数の呼出し
      -- e.g. FunCall1 f a -> (funcall f a)
      FunCall1 e e
    | -- | 任意引数関数の呼出し
      -- Data.Function.UncurriedのFn2などの呼出しを最適化するに利用する。
      -- FunCall{0,1}も包含しているが,基本上記の最適化以外では利用しない。
      -- e.g. FunCallN f [a0, .., an] -> (funcall f a0 a1 ... an)
      FunCallN e [e]
    | -- | 任意引数のネイティブ関数の呼出し
      -- e.g. FunCallNative "foo" [a, b] -> (foo a b)
      FunCallNative Symbol [e]
    | -- | e.g. 'foo
      QuotedSymbol Symbol
    | Lambda0 e
    | FunCall0 e
    deriving (Functor, Foldable, Traversable, Generic)

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
    | PBackquotedCons (PPattern e) (PPattern e)
    | PAnd [PPattern e]
    | PPred e
    | -- | Boolは特別扱い(最適化でif文に変更しうるので)
      -- 本来は以下のようにしていた
      -- list [symbol "pred", bool (symbol "null") (symbol "identity") b]
      PPredBool Bool
    | PApp e (PPattern e)
    deriving (Functor, Foldable, Traversable, Generic)

-- Unsafe prefixは任意のテキストが妥当なelispシンボルにならないことを示している。
newtype Symbol = UnsafeSymbol {symbolText :: Text}
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

progn :: [SExp] -> SExp
progn = SExp . Progn

quotedSymbol :: Symbol -> SExp
quotedSymbol = SExp . QuotedSymbol

-- association list(e.g. `((foo . ,v) (bar . 2))
-- don't need comma
alist :: [(Symbol, SExp)] -> SExp
alist = SExp . MkAlist

if' :: SExp -> SExp -> SExp -> SExp
if' c t e = SExp $ If c t e

cond :: [(SExp, SExp)] -> SExp
cond = SExp . Cond

letStar :: [(Symbol, SExp)] -> SExp -> SExp
letStar bindings body = SExp $ Let LetStar bindings body

letRec :: [(Symbol, SExp)] -> SExp -> SExp
letRec bindings body = SExp $ Let LetRec bindings body

pcase :: [SExp] -> [PcaseAlt SExp] -> SExp
pcase exps cases = SExp $ Pcase exps cases

lambda1 :: Symbol -> SExp -> SExp
lambda1 arg body = SExp $ Lambda1 arg body

-- e.g. (lambda (a) (lambda (b) ...))
lambda1Fold :: NonEmpty Symbol -> SExp -> SExp
lambda1Fold args body =
    let a0 :| as = NonEmpty.reverse args
     in foldl' (flip lambda1) (lambda1 a0 body) as

funcall1 :: SExp -> SExp -> SExp
funcall1 f arg = SExp $ FunCall1 f arg

funcallN :: SExp -> [SExp] -> SExp
funcallN f args = SExp $ FunCallN f args

lambda0 :: SExp -> SExp
lambda0 = SExp . Lambda0

funcall0 :: SExp -> SExp
funcall0 = SExp . FunCall0

funcallNative :: Symbol -> [SExp] -> SExp
funcallNative f args = SExp $ FunCallNative f args

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
