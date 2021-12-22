{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module PsEl.Transpile where

import Language.PureScript (Ident (..), ModuleName (ModuleName), ProperNameType (ConstructorName))
import Language.PureScript qualified as P hiding (ProperName)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn
import Language.PureScript.Errors (SourceSpan)
import Language.PureScript.Names (ProperName (runProperName), Qualified (Qualified), mkQualified)
import Language.PureScript.PSString (PSString (toUTF16CodeUnits))
import Language.PureScript.PSString qualified as PS
import PsEl.SExp
import PsEl.SExpConstructor qualified as C
import RIO
import RIO.Lens
import RIO.NonEmpty qualified as NonEmpty
import RIO.Set qualified as Set
import RIO.Text qualified as T
import RIO.Text.Partial qualified as Partial

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

        -- 理由は分からないが, moduleImportsには自モジュールも含まれている。
        -- それをrequireしてしまうと再帰ruquireのエラーになるため除外する。
        requires =
            pselFeature : map featureName (filter (not . ignoreModule) (map snd moduleImports))

        ignoreModule mn =
            mn == moduleName || isPrimModule mn

        defVars =
            mconcat $ map (decl moduleName) moduleDecls

        requireFFI =
            if null moduleForeign
                then Nothing
                else Just (ffiFeatureName moduleName, map (globalVar moduleName) moduleForeign)

        isPrimModule mn@(ModuleName t) =
            mn == C.Prim || T.isPrefixOf "Prim." t

-- 全ての生成モジュールに必要になるヘルパーライブラリ
pselFeature :: FeatureName
pselFeature = FeatureName "psel"

-- モジュール名はそのまま Feqatureとする
-- キャメルケース,ドット区切りはelispの規約に沿っていないが,
-- PSからの生成ファイルということ
featureName :: ModuleName -> FeatureName
featureName (ModuleName t) = FeatureName $ mkSymbol t

-- PSのモジュール名では _ は多分使わないので大丈夫かな？
ffiFeatureName :: ModuleName -> FeatureName
ffiFeatureName (ModuleName s) = FeatureName $ mkSymbol $ s <> ffiFeatureSuffix

ffiFeatureSuffix :: Text
ffiFeatureSuffix = "._FOREIGN_"

-- PSでは ' を付けことが多いが elisp では許されていない。$ は OK。
-- ' は ~ に変換する。元々の ~ は ~~ に(ただPSではあまり ~ を使うことはないが..)変換。
-- replaceの順序は重要。定数シンボル以外は必ずこの関数を通りてSymbolを作成すること。
mkSymbol :: Text -> Symbol
mkSymbol =
    UnsafeSymbol . Partial.replace "'" "~" . Partial.replace "~" "~~"

-- マクロや組込関数(built-ins, special-formも含む)の名前衝突も値スロットだけ使う分には考える必要はない。
-- シンタックス上のキーワードではなく特別な関数が関数スロットに設定されている。
-- 以下のように値スロットに束縛したところで問題ない。
--
--    (let ((defun 1)  ;; defun is macro
--          (let 2))   ;; let is special-form
--      (+ defun let)) ;; => 3
--
-- ただ定数(特別なシンボルで,キーワードを除けば t, nilのみ)は束縛するとエラーが出る。
-- https://www.gnu.org/software/emacs/manual/html_node/elisp/Constant-Variables.html
-- Purescriptの識別子には使えない ^ 文字をケツにつける。
localVar :: Ident -> Symbol
localVar ident =
    mkSymbol $
        if t `elem` constants
            then t <> "^"
            else t
  where
    t = _identToText ident
    constants = ["t", "nil"]

-- グローバル変数の衝突に関しては,PSモジュールのprefix(e.g. Foo.Bar.foo)を使うので衝突は基本起こらない。
globalVar :: ModuleName -> Ident -> Symbol
globalVar (ModuleName mn) ident =
    mkSymbol $ mn <> "." <> _identToText ident

_identToText :: Ident -> Text
_identToText (Ident t) = t
_identToText (GenIdent mvar i) = fromMaybe "__instance" mvar <> textDisplay i
_identToText UnusedIdent = error "impossible"

-- | Bind

-- top-level binding で Rec な binding はありえるが(相互再帰の場合),
-- 定義順は関係ないので flatten すればいいだけ。
decl :: ModuleName -> Bind Ann -> [DefVar]
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
expr :: Expr Ann -> SExp
expr (Literal _ lit) = literal lit
expr (Constructor _ _tname cname ids) = constructor cname ids
expr (Accessor _ ps e) = objectAccess ps (expr e)
expr (ObjectUpdate _ e xs) = objectUpdate (map (over _2 expr) xs) (expr e)
expr (Abs _ id e) = lambda1 (localVar id) [expr e]
expr (App _ e0 e1) = C.funcall1 (expr e0) (expr e1)
expr (Var _ qident) = var qident
expr (Case _ es cas) = case' (map expr es) cas
expr (Let _ binds e) = let' binds (expr e)

-- nil 及び t は特別な定数でありは束縛やsetqはできない。
-- vectorのリテラル表記 [a b] は a や b を評価しないことに注意。
-- そのためvectorのリテラル表記ではなく vector関数を使う必要がある
--
--   A vector, like a string or a number, is considered a constant for evaluation:
--   the result of evaluating it is the same vector. This does not evaluate or
--   even examine the elements of the vector.
--
literal :: Literal (Expr Ann) -> SExp
literal (NumericLiteral (Left i)) = integer i
literal (NumericLiteral (Right d)) = double d
literal (StringLiteral ps) = string $ psstring ps
literal (CharLiteral c) = character c
literal (BooleanLiteral b) = bool C.nil C.t b
literal (ArrayLiteral exs) = list $ symbol "vector" : map expr exs
literal (ObjectLiteral xs) = objectLiteral $ map (over _2 expr) xs

-- 型チェックの都合上 Prim.undefinedという未定義の参照が入ることがある。
-- (実装に問題がなければ)参照されることはないので適当な未定義の参照に置き換える。
-- -> 違うっぽい。参照はされるが使われることはない,かな。なので 'ps-prim-undefined に。
var :: Qualified Ident -> SExp
var v@(Qualified mn id)
    | v == primUndefined = quote $ symbol "ps-prim-undefined"
    | otherwise = symbol (maybe localVar globalVar mn id)
  where
    primUndefined = mkQualified (Ident C.undefined) C.Prim

-- Rec(相互参照と自己参照など) と NonRec があるので注意が必要。
-- PSのletは順序関係なし(順序によってshadowingは変化しない)
-- 全部 letrec で束縛してしまうのが多分正解かな？
-- ただ殆どのケースで let* (頑張れば let)で十分なのに letrec は微妙か？
-- NonRec のみなら let*,一つでも Rec があれば letrec でいいかな。
let' :: [Bind Ann] -> SExp -> SExp
let' binds body = list [letS, list bindS, body]
  where
    ext = \case
        NonRec _ ident expr -> [((ident, expr), False)]
        Rec bs -> map ((,True) . over _1 snd) bs
    binds' = mconcat $ map ext binds
    letS =
        if any snd binds'
            then symbol "letrec"
            else symbol "let*"
    bindS =
        map ((\(ident, e) -> list [symbol (localVar ident), expr e]) . fst) binds'

-- pcaseマクロを利用する
--
-- 対象がリストなのはカンマ区切りで複数対象を指定できるので(e.g. case a, b of)
-- 各CaseAlternativeは同じ数だけのbinderが必要。
-- 複数指定の場合はリストに包んでpcaseに適用させる。(e.g. (pcase (list a b) ..))
case' :: [SExp] -> [CaseAlternative Ann] -> SExp
case' ss cas = list $ [symbol "pcase", target] <> concatMap caseAlt cas
  where
    target = case ss of
        [] -> error "Empty case target"
        [s] -> s
        ss -> list (symbol "list" : ss)

    -- ガード毎に別のマッチングにする必要がある。そのためにリストを返している。
    -- 同じbinderなのにガード節毎にbinderが重複する形になるが仕方なし。
    caseAlt :: CaseAlternative Ann -> [SExp]
    caseAlt (CaseAlternative bs e) = do
        (guard', ex) <- case e of
            Left xs -> do
                (guard, ex) <- xs
                pure (Just guard, ex)
            Right ex ->
                pure (Nothing, ex)
        pure $
            list
                [ maybe id addGuard guard' $ binders bs
                , expr ex
                ]
      where
        addGuard guard sexp =
            list [symbol "and", sexp, list [symbol "guard", expr guard]]

    -- binderが複数ある場合は `(,a ,b) のようにリストでまとめる
    binders [] = error "Empty binder"
    binders [b] = binder b
    binders bs = backquote $ list $ map (comma . binder) bs

    -- newtypeのマッチングの場合はConstructorBindersが呼ばれる。
    -- 当然newtypeなので下の値がそのまま入っているサブbinderは一つのはず。
    -- Annのメタ情報を見る必要がある。
    binder :: Binder Ann -> SExp
    binder (NullBinder _) = symbol "_"
    binder (LiteralBinder _ lit) = literalBinder lit
    binder (VarBinder _ id) = symbol $ localVar id
    binder (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) = binder b
    binder (ConstructorBinder (_, _, _, Just IsNewtype) _ _ _bs) = error "Unexpected binding"
    binder (ConstructorBinder _ _ (Qualified _ cname) bs) = constructorBinder cname (map binder bs)
    binder (NamedBinder _ id b) = list [symbol "and", symbol (localVar id), binder b]

    -- boolean binder と object binder がやっかい。
    -- boolean binder だからといって単に t, nil というシンボル使っても意味がない
    -- (pred ..) を使って nil か t(nil以外)を判別する必要がある。
    -- floatのbinderも使えないので pred + = を使う必要がある。
    literalBinder :: Literal (Binder Ann) -> SExp
    literalBinder (NumericLiteral (Left i)) =
        integer i
    literalBinder (NumericLiteral (Right d)) =
        list [symbol "pred", lambda1 "v" [list [symbol "=", symbol "v", double d]]]
    literalBinder (StringLiteral ps) =
        string $ psstring ps
    literalBinder (CharLiteral c) =
        character c
    literalBinder (BooleanLiteral b) =
        list [symbol "pred", bool (symbol "null") (symbol "identity") b]
    literalBinder (ArrayLiteral bs) =
        backquote $ vector $ map (comma . binder) bs
    literalBinder (ObjectLiteral xs) =
        objectLiteralBinder $ map (over _2 binder) xs

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
        list $ symbol "vector" : quote (symbol (constructorTag cname)) : vals

-- e.g. `[Foo ,e0 ,e1]
constructorBinder :: ProperName 'ConstructorName -> [SExp] -> SExp
constructorBinder cname binds =
    backquote
        . vector
        $ symbol (constructorTag cname) : map comma binds

constructorTag :: ProperName 'ConstructorName -> Symbol
constructorTag = mkSymbol . runProperName

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
                "v"
                [ list
                    [ symbol "psel/alist-get"
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
        [ symbol "psel/alist-get"
        , quote (symbol (objectField fname))
        , obj
        ]

-- 標準で非破壊的にalistを設定するための関数が提供されていない。
-- (copy-alist + setf + alist-get で出来なくはないがややこい)。
-- psel.el からimmutableにalist を更新するpset/alist-set関数を提供する。
-- e.g. (psel/alist-set 'foo 1 (psel/alist-set 'bar "a" obj))
--
-- またレコードの更新構文を使ったとしても必ずしもObjectUpdateにコンパイルされるわけではない。
-- (恐らく)レコードが小さければレコードリテラル+フィールド参照に置き換えられる。
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
objectField = mkSymbol . psstring

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
