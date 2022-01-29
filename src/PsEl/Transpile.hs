{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module PsEl.Transpile where

import Data.Reflection (Given (given), give)
import Language.PureScript (Ident (..), ModuleName (ModuleName), ProperName (ProperName), ProperNameType (ConstructorName, TypeName))
import Language.PureScript qualified as P hiding (ProperName)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn
import Language.PureScript.Errors (SourceSpan)
import Language.PureScript.Names (ProperName (runProperName), Qualified (Qualified), mkQualified)
import Language.PureScript.PSString (PSString (toUTF16CodeUnits))
import Language.PureScript.PSString qualified as PS
import PsEl.SExp hiding (Let)
import PsEl.SExpConstructor qualified as C
import RIO
import RIO.Lens
import RIO.NonEmpty qualified as NonEmpty
import RIO.NonEmpty.Partial qualified as NonEmptyPartial
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

-- elispではシンボルは任意の文字列を
-- ただしS式として表示する際適切なエスケープが必要(例えば空白や ' は \ でのエスケープが必要)。
-- 参照 https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Type.html
mkSymbol :: Text -> Symbol
mkSymbol = UnsafeSymbol

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

-- bindで結果を使わない場合,$__unusedというローカル変数が割当てられる。
-- 最適化の際に識別できるよう識別子として提供する。
-- 本来であれば型によって表現されるべきである。
-- 関連issueが作成されていた記憶がある。
localUnusedVar :: Symbol
localUnusedVar = mkSymbol "$__unused"

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
            , definition = give mn (expr e)
            }
    binds =
        case bind of
            NonRec _ ident expr -> [(ident, expr)]
            Rec bs -> map (over _1 snd) bs

-- 現在処理中のモジュール名を返す。
-- モジュール別の特殊対応が必要な場合に使う。
-- 引数で渡す・readerモナド導入はかなり面倒なので reflectionライブラリを利用する。
-- https://hackage.haskell.org/package/reflection-2.1.6/docs/Data-Reflection.html
currentModuleName :: Given ModuleName => ModuleName
currentModuleName = given

-- | Expr
expr :: Given ModuleName => Expr Ann -> SExp
expr (Literal _ lit) = literal lit
expr (Constructor _ tname cname ids) = constructor tname cname ids
expr (Accessor _ ps e) = objectAccess ps (expr e)
expr (ObjectUpdate _ e xs) = objectUpdate (map (over _2 expr) xs) (expr e)
expr (Abs _ id e) = lambda1 (localVar id) (expr e)
expr (App _ e0 e1) = funcall1 (expr e0) (expr e1)
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
literal :: Given ModuleName => Literal (Expr Ann) -> SExp
literal (NumericLiteral (Left i)) = integer i
literal (NumericLiteral (Right d)) = double d
literal (StringLiteral ps) = string $ psstring ps
literal (CharLiteral c) = character c
literal (BooleanLiteral b) = bool C.nil C.t b
literal (ArrayLiteral exs) = funcallNative "vector" (map expr exs)
literal (ObjectLiteral xs) = objectLiteral $ map (over _2 expr) xs

-- 型チェックの都合上 Prim.undefinedという未定義の参照が入ることがある。
-- (実装に問題がなければ)参照されることはないので適当な未定義の参照に置き換える。
-- -> 違うっぽい。参照はされるが使われることはない,かな。なので 'ps-prim-undefined に。
var :: Qualified Ident -> SExp
var v@(Qualified mn id)
    | v == primUndefined = quotedSymbol "ps-prim-undefined"
    | otherwise = symbol (maybe localVar globalVar mn id)
  where
    primUndefined = mkQualified (Ident C.undefined) C.Prim

-- Rec(相互参照と自己参照など) と NonRec があるので注意が必要。
-- PSのletは順序関係なし(順序によってshadowingは変化しない)
-- 全部 letrec で束縛してしまうのが多分正解かな？
-- ただ殆どのケースで let* (頑張れば let)で十分なのに letrec は微妙か？
-- NonRec のみなら let*,一つでも Rec があれば letrec でいいかな。
let' :: Given ModuleName => [Bind Ann] -> SExp -> SExp
let' binds = letC bindS
  where
    ext :: Bind a -> [((Ident, Expr a), Bool)]
    ext = \case
        NonRec _ ident expr -> [((ident, expr), False)]
        Rec bs -> map ((,True) . over _1 snd) bs

    binds' =
        mconcat $ map ext binds

    letC =
        if any snd binds'
            then letRec
            else letStar

    bindS =
        map (bimap localVar expr . fst) binds'

-- pcaseマクロを利用する
--
-- 対象がリストなのはカンマ区切りで複数対象を指定できるので(e.g. case a, b of)
-- 各CaseAlternativeは同じ数だけのbinderが必要。
-- 複数指定の場合はリストに包んでpcaseに適用させる。(e.g. (pcase (list a b) ..))
case' :: Given ModuleName => [SExp] -> [CaseAlternative Ann] -> SExp
case' ss cas = pcase ss cases
  where
    -- マッチング節が一つしかなくガード節を使っている場合は cond が利用できる。
    cases :: [PcaseAlt SExp]
    cases = case cas of
        [] ->
            []
        [CaseAlternative bs (Left xs)] ->
            [ PcaseAlt
                { patterns = map binder bs
                , guard = Nothing
                , code = cond (map (bimap expr expr) xs)
                }
            ]
        cs ->
            concatMap caseAlt cs

    -- ガード毎に別のマッチングにする必要がある。
    -- あるマッチング節のいずれのガード節でも該当しない場合次のマッチング節に移る必要があるが,
    -- マッチング節一つでcondで分岐した場合,移ることができないため。そのためにリストを返している。
    -- 同じbinderなのにガード節毎にbinderが重複する形になるが仕方なし。
    caseAlt :: CaseAlternative Ann -> [PcaseAlt SExp]
    caseAlt (CaseAlternative bs e) = do
        (guard', ex) <- case e of
            Left xs -> do
                (guard, ex) <- xs
                pure (Just guard, ex)
            Right ex ->
                pure (Nothing, ex)
        pure $
            PcaseAlt
                { patterns = map binder bs
                , guard = expr <$> guard'
                , code = expr ex
                }

    -- (1)
    -- newtypeのマッチングの場合はConstructorBindersが呼ばれる。
    -- 当然newtypeなので下の値がそのまま入っているサブbinderは一つのはず。
    -- Annのメタ情報を見る必要がある。
    --
    -- (2)
    -- QualifiedのmnはMaybe ModuleNameである。
    -- どのようなケースでNothingとなるのか？
    binder :: Binder Ann -> PPattern SExp
    binder (NullBinder _) =
        PAny
    binder (LiteralBinder _ lit) =
        literalBinder lit
    binder (VarBinder _ id) =
        PBind $ localVar id
    binder (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
        binder b -- (1)
    binder (ConstructorBinder (_, _, _, Just IsNewtype) _ _ _bs) =
        error "Unexpected binding"
    binder (ConstructorBinder _ (Qualified (Just mn) tname) (Qualified _ cname) bs) =
        constructorBinder mn tname cname (map binder bs)
    binder (ConstructorBinder _ (Qualified Nothing tname) (Qualified _ cname) bs) =
        error "Unexpected binding" -- (2)
    binder (NamedBinder _ id b) =
        PAnd [PBind (localVar id), binder b]

    -- boolean binder と object binder がやっかい。
    -- boolean binder だからといって単に t, nil というシンボル使っても意味がない
    -- (pred ..) を使って nil か t(nil以外)を判別する必要がある。
    -- floatのbinderも使えないので pred + = を使う必要がある。
    literalBinder :: Literal (Binder Ann) -> PPattern SExp
    literalBinder (NumericLiteral (Left i)) =
        PInteger i
    literalBinder (NumericLiteral (Right d)) =
        PPred (lambda1 "v" (funcallNative "=" [symbol "v", double d]))
    literalBinder (StringLiteral ps) =
        PString $ psstring ps
    literalBinder (CharLiteral c) =
        PCharacter c
    literalBinder (BooleanLiteral b) =
        PPredBool b
    literalBinder (ArrayLiteral bs) =
        PBackquotedVector $ map (Right . binder) bs
    literalBinder (ObjectLiteral xs) =
        objectLiteralBinder $ map (over _2 binder) xs

-- | DataType

-- データ型は先頭にタグの入ったベクターで表現する。
--
-- e.g. Foo 1 2 -> ['Foo 1 2]
-- e.g. Nothing -> ['Nothing]
--
-- consturctorはデータ型の定義箇所で呼ばれる。
-- (newtype のコンスラクタははidentity関数にbindされる)。
-- 例えば Data.List.Types には List型が次のように定義されている。
--
--   data List a = Nil | Cons a (List a)
--
-- NilとConsコンストラクに対してそれぞれconstructor関数が呼ばれ,
-- 次のコンストラクタ関数が定義される。
--
--   (defvar Data.List.Types.Nil (vector 'Nil))
--   (defvar Data.List.Types.Cons (lambda (value0) (lambda (value1) (vector 'Cons value0 value1))))
--
-- ただList型に関しては特別に elisp に於けるリストで表現する。
-- リストはlispに於いて普遍的に使われる構造でありリストを表現に持つデータ型により
-- Elisp/PureScript間のデータの遣り取りが自然に書けるようになる。
--
-- NOTE: 空の場合はそもそも Vectorで囲む必要がないなど細かい最適化は可能
constructor ::
    Given ModuleName =>
    ProperName 'TypeName ->
    ProperName 'ConstructorName ->
    [Ident] ->
    SExp
constructor (ProperName "List") cname ids
    | ModuleName "Data.List.Types" <- currentModuleName =
        case (cname, ids) of
            (ProperName "Nil", []) ->
                C.nil
            (ProperName "Cons", [_, _]) ->
                let args = localVar <$> NonEmptyPartial.fromList ids
                    vals = map symbol $ NonEmpty.toList args
                 in lambdaN args (funcallNative "cons" vals)
            _ ->
                error "Unexpected List constcutor"
constructor tname cname ids =
    case NonEmpty.nonEmpty (map localVar ids) of
        Just args ->
            let vals = map symbol $ NonEmpty.toList args
             in lambdaN args (construct cname vals)
        Nothing ->
            construct cname []
  where
    construct :: ProperName 'ConstructorName -> [SExp] -> SExp
    construct cname vals =
        funcallNative "vector" $ quotedSymbol (constructorTag cname) : vals

-- e.g. `[Foo ,e0 ,e1]
constructorBinder ::
    ModuleName ->
    ProperName 'TypeName ->
    ProperName 'ConstructorName ->
    [PPattern SExp] ->
    PPattern SExp
constructorBinder (ModuleName "Data.List.Types") (ProperName "List") cname binds =
    case (cname, binds) of
        (ProperName "Nil", []) -> PBackquotedList []
        (ProperName "Cons", [car, cdr]) -> PBackquotedCons car cdr
        _ -> error "Unexpected List binder"
constructorBinder _ _ cname binds =
    PBackquotedVector $ Left (constructorTag cname) : map Right binds

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
objectLiteralBinder :: [(PSString, PPattern SExp)] -> PPattern SExp
objectLiteralBinder = \case
    [] -> PAny
    [(ps, bind')] -> bind (objectField ps, bind')
    bs -> PAnd $ map (bind . over _1 objectField) bs
  where
    bind (field, bind') =
        PApp
            ( lambda1
                "v"
                ( funcallNative
                    "psel/alist-get"
                    [ quotedSymbol field
                    , symbol "v"
                    ]
                )
            )
            bind'

-- e.g. (cdr (assq 'foo obj))
objectAccess :: PSString -> SExp -> SExp
objectAccess fname obj =
    funcallNative
        "psel/alist-get"
        [ quotedSymbol (objectField fname)
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
        funcallNative
            "psel/alist-set"
            [ quotedSymbol (objectField fname)
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
