{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module PsEl.Transpile where

import Language.PureScript (Ident (..), ModuleName (ModuleName), ProperNameType (ConstructorName))
import Language.PureScript qualified as P hiding (ProperName)
import Language.PureScript.CoreFn
import Language.PureScript.Errors (SourceSpan)
import Language.PureScript.Names (ProperName (runProperName), Qualified (Qualified))
import Language.PureScript.PSString (PSString (toUTF16CodeUnits))
import Language.PureScript.PSString qualified as PS
import PsEl.SExp
import RIO
import RIO.Lens
import RIO.NonEmpty qualified as NonEmpty
import RIO.Set qualified as Set
import RIO.Text.Partial qualified as Partial

-- -- PureScript Module Info
-- data PSModuleInfo

transpile :: Module Ann -> Feature
transpile
    Module
        { moduleSourceSpan
        , moduleComments
        , moduleName
        , modulePath
        , moduleImports
        , moduleExports
        , moduleForeign
        , moduleDecls
        } = Feature{name, requires, requireFFI, defVars}
      where
        name =
            featureName moduleName
        requires =
            pselFeature : map (featureName . snd) moduleImports
        defVars =
            mconcat $ map (decl moduleName) moduleDecls
        requireFFI =
            if null moduleForeign
                then Nothing
                else Just (ffiFeatureName moduleName, map (globalVar moduleName) moduleForeign)

-- 全ての生成モジュールに必要になるヘルパーライブラリ
pselFeature :: FeatureName
pselFeature = FeatureName $ UnsafeSymbol "psel"

-- モジュール名はそのまま Feqatureとする
-- キャメルケース,ドット区切りはelispの規約に沿っていないが,
-- PSからの生成ファイルということ
featureName :: ModuleName -> FeatureName
featureName (ModuleName t) = FeatureName $ UnsafeSymbol t

-- PSのモジュール名では _ は多分使わないので大丈夫かな？
ffiFeatureName :: ModuleName -> FeatureName
ffiFeatureName (ModuleName s) = FeatureName $ UnsafeSymbol $ s <> ffiFeatureSuffix

ffiFeatureSuffix :: Text
ffiFeatureSuffix = "._FOREIGN_"

-- マクロや組込関数(built-ins, special-formも含む)の名前衝突も値スロットだけ使う分には考える必要はない。
-- シンタックス上のキーワードではなく特別な関数が関数スロットに設定されている。
-- 以下のように値スロットに束縛したところで問題ない。
--
--    (let ((defun 1)  ;; defun is macro
--          (let 2))   ;; let is special-form
--      (+ defun let)) ;; => 3
--
localVar :: Ident -> Symbol
localVar = UnsafeSymbol . _identToText

-- グローバル変数の衝突に関しては,PSモジュールのprefix(e.g. Foo.Bar.foo)を使うので衝突は基本起こらない。
globalVar :: ModuleName -> Ident -> Symbol
globalVar (ModuleName mn) ident =
    UnsafeSymbol $ mn <> "." <> _identToText ident

-- PSでは ' を付けことが多いが elisp では許されていない。$ は OK。
-- ' は ^ に変換する。元々の ^ は ^^ に(ただPSではあまり ^ を使うことはないが..)変換。
-- replaceの順序は重要。
_identToText :: Ident -> Text
_identToText (Ident t) = Partial.replace "'" "^" $ Partial.replace "^" "^^" t
_identToText (GenIdent mvar i) = fromMaybe "__instance" mvar <> textDisplay i
_identToText UnusedIdent = error "impossible"

-- | Bind

-- top-level binding で Rec な binding はありえるが(相互再帰の場合),
-- 定義順は関係ないので flatten すればいいだけ。
decl :: ModuleName -> Bind a -> [DefVar]
decl mn bind = map (uncurry decl') binds
  where
    decl' ident e =
        DefVar
            { name = globalVar mn ident
            , definition = expr e
            }
    binds =
        case bind of
            NonRec _ ident expr -> [(ident, expr)]
            Rec bs -> map (over _1 snd) bs

-- | Expr
expr :: Expr a -> SExp
expr (Literal _ lit) = literal lit
expr (Constructor _ _tname cname ids) = constructor cname ids
expr (Accessor _ ps e) = objectAccess ps (expr e)
expr (ObjectUpdate _ e xs) = objectUpdate (map (over _2 expr) xs) (expr e)
expr (Abs _ id e) = lambda1 (localVar id) [(expr e)]
expr (App _ e0 e1) = list [symbol "funcall", expr e0, expr e1]
expr (Var _ qident) = var qident
expr (Case _ es cas) = case' (map expr es) cas
expr (Let _ binds e) = let' binds (expr e)

-- nil 及び t は特別な定数でありは束縛やsetqはできない。
literal :: Literal (Expr a) -> SExp
literal (NumericLiteral (Left i)) = integer i
literal (NumericLiteral (Right d)) = double d
literal (StringLiteral ps) = string $ psstring ps
literal (CharLiteral c) = character c
literal (BooleanLiteral b) = bool (symbol "nil") (symbol "t") b
literal (ArrayLiteral exs) = vector $ map expr exs
literal (ObjectLiteral xs) = objectLiteral $ map (over _2 expr) xs

var :: Qualified Ident -> SExp
var (Qualified mn id) = symbol (maybe localVar globalVar mn id)

-- Rec(相互参照と自己参照など) と NonRec があるので注意が必要。
-- PSのletは順序関係なし(順序によってshadowingは変化しない)
-- 全部 letrec で束縛してしまうのが多分正解かな？
-- ただ殆どのケースで let* (頑張れば let)で十分なのに letrec は微妙か？
-- NonRec のみなら let*,一つでも Rec があれば letrec でいいかな。
let' :: [Bind a] -> SExp -> SExp
let' binds body = list [letS, list bindS, body]
  where
    ext = \case
        NonRec _ ident expr -> [((ident, expr), False)]
        Rec bs -> map (,True) $ map (over _1 snd) bs
    binds' = mconcat $ map ext binds
    letS =
        if any snd binds'
            then symbol "letrec"
            else symbol "let*"
    bindS =
        map
            (\(ident, e) -> list [symbol (localVar ident), expr e])
            (map fst binds')

-- pcaseマクロを利用する
--
-- 対象がリストなのはカンマ区切りで複数対象を指定できるので(e.g. case a, b of)
-- 各CaseAlternativeは同じ数だけのbinderが必要。
-- 複数指定の場合はリストに包んでpcaseに適用させる。(e.g. (pcase (list a b) ..))
case' :: [SExp] -> [CaseAlternative a] -> SExp
case' ss cas = list $ [symbol "pcase", target] <> map caseAlt cas
  where
    target = case ss of
        [] -> error "Empty case target"
        [s] -> s
        ss -> list (symbol "list" : ss)

    caseAlt :: CaseAlternative a -> SExp
    caseAlt (CaseAlternative bs e) = list [binders bs, exec e]

    -- binderが複数ある場合は `(,a ,b) のようにリストでまとめる
    binders [] = error "Empty binder"
    binders [b] = binder b
    binders bs = backquote $ list $ map (comma . binder) bs

    binder :: Binder a -> SExp
    binder (NullBinder _) = symbol "_"
    binder (LiteralBinder _ lit) = literalBinder lit
    binder (VarBinder _ id) = symbol $ localVar id
    binder (ConstructorBinder _ _qual (Qualified _ cname) bs) = constructorBinder cname (map binder bs)
    binder (NamedBinder _ id b) = list [symbol "and", symbol (localVar id), binder b]

    -- boolean binder と object binder がやっかい。
    -- boolean binder だからといって単に t, nil というシンボル使っても意味がない
    -- (pred ..) を使って nil か t(nil以外)を判別する必要がある。
    literalBinder :: Literal (Binder a) -> SExp
    literalBinder (NumericLiteral (Left i)) = integer i
    literalBinder (NumericLiteral (Right d)) = double d
    literalBinder (StringLiteral ps) = string $ psstring ps
    literalBinder (CharLiteral c) = character c
    literalBinder (BooleanLiteral b) = list [symbol "pred", bool (symbol "null") (symbol "identity") b]
    literalBinder (ArrayLiteral bs) = backquote $ vector $ map (comma . binder) bs
    literalBinder (ObjectLiteral xs) = objectLiteralBinder $ map (over _2 binder) xs

    -- ガード節がある場合はcondを使う
    -- type Guard a = Expr a
    -- A guard is just a boolean-valued expression that appears alongside a set of binders
    exec :: Either [(Guard a, Expr a)] (Expr a) -> SExp
    exec (Left xs) = list $ symbol "cond" : map (\(g, e) -> list [expr g, expr e]) xs
    exec (Right e) = expr e

-- | DataType

-- data型によるコンストラクタ(newtype のコンスラクタははidentity関数にbindされる)。
-- top-level の bind でしか表われない。
-- 引数を取りelispにおけるデータ型に変換する
-- e.g. Foo 1 2 -> ['Foo 1 2]
constructor :: ProperName 'ConstructorName -> [Ident] -> SExp
constructor cname ids =
    case NonEmpty.nonEmpty (map localVar ids) of
        Just args ->
            let vals = map symbol $ NonEmpty.toList args
             in lambdaN args [construct cname vals]
        Nothing ->
            construct cname []
  where
    -- TODO: 空の場合はそもそも Vectorで囲む必要はないかな。
    construct :: ProperName 'ConstructorName -> [SExp] -> SExp
    construct cname vals =
        vector $ quote (symbol (constructorTag cname)) : vals

-- e.g. `[Foo ,e0 ,e1]
constructorBinder :: ProperName 'ConstructorName -> [SExp] -> SExp
constructorBinder cname binds =
    backquote
        . list
        $ symbol (constructorTag cname) : map comma binds

constructorTag :: ProperName 'ConstructorName -> Symbol
constructorTag = UnsafeSymbol . runProperName

-- | Object

-- Assocation List
objectLiteral :: [(PSString, SExp)] -> SExp
objectLiteral xs = alist $ map (over _1 objectField) xs

-- pcaseに使われるbinder
-- 複数ある場合は and で連結する必要がある。
-- alistを使っての構造分解はできない。順序が異なるし,部分的にマッチングも有りえるため。
-- (app (lambda (v) (alist-get '<field> v)) PATTERN) を利用する
-- lambda1 で名前vが導入されているが,シャドウする危険はない。
objectLiteralBinder :: [(PSString, SExp)] -> SExp
objectLiteralBinder = \case
    [] -> symbol "_"
    [(ps, bind')] -> bind (objectField ps, bind')
    bs -> list $ symbol "and" : map (bind . over _1 objectField) bs
  where
    bind (field, bind') =
        list
            [ symbol "app"
            , lambda1
                (UnsafeSymbol "v")
                [ list
                    [ symbol "alist-get"
                    , quote (symbol field)
                    , symbol "v"
                    ]
                ]
            , bind'
            ]

-- e.g. (cdr (assq 'foo obj))
objectAccess :: PSString -> SExp -> SExp
objectAccess fname obj =
    list
        [ symbol "cdr"
        , list
            [ symbol "assq"
            , quote (symbol (objectField fname))
            , obj
            ]
        ]

-- 標準で非破壊的にalistを設定するための関数が提供されていない。
-- (copy-alist + setf + alist-get で出来なくはないがややこい)。
-- 下記関数を psel.el から提供する。
--
-- (defun psel/alist-set (field val alist)
--   "Update the first cons with car eq to field in a immutable way."
--   (cond ((null alist)
--          (throw 'ps nil))
--         ((eq (caar alist) field)
--          (cons (cons field val) (cdr alist)))
--         (t
--          (cons (car alist) (psel/alist-set field val (cdr alist))))))
--
-- e.g. (psel/alist-set 'foo 1 (psel/alist-set 'bar "a" obj))
objectUpdate :: [(PSString, SExp)] -> SExp -> SExp
objectUpdate updates obj = foldl' alistSet obj updates
  where
    alistSet obj (fname, s) =
        list
            [ symbol "psel/alist-set"
            , quote (symbol (objectField fname))
            , s
            ]

-- PSのフィールド名をそのままSymbolにして使う
objectField :: PSString -> Symbol
objectField = UnsafeSymbol . psstring

-- | PSString

-- 参照
-- https://hackage.haskell.org/package/purescript-0.13.8/docs/Language-PureScript-PSString.html#t:PSString
-- https://github.com/purescript/purescript/issues/2434
--
-- 何やら厄介な話。
-- Strings in PureScript are sequences of UTF-16 code units,
-- which do not necessarily represent UTF-16 encoded text.
--
-- lone surrogatesが含まれる場合が厄介らしい。
-- 恐らく殆んど使われることはないと思うので docodeStringで Nothingの場合はエラーを投げる
-- https://hackage.haskell.org/package/purescript-0.13.8/docs/Language-PureScript-PSString.html#v:decodeString
--
psstring :: PSString -> Text
psstring ps = case PS.decodeString ps of
    Just t -> t
    Nothing -> error "Unexpected one surragates in string literal"
