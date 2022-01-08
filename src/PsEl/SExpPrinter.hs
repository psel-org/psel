{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PsEl.SExpPrinter where

import Data.Functor.Foldable (cata, para)
import Data.List (intersperse)
import PsEl.SExp
import PsEl.SExpConstructor qualified as C
import RIO hiding (bracket)
import RIO.Text.Partial qualified as T
import Text.RE.TDFA.Text qualified as RE

displayFeature :: Feature -> Utf8Builder
displayFeature Feature{name, requires, requireFFI, defVars} =
    mconcat . intersperse "\n" $
        mconcat
            [ [headLine]
            , requireLines
            , loadFFILine
            , defVarLines
            , [provideLine]
            ]
  where
    headLine =
        ";; -*- lexical-binding: t; -*-"

    requireLines =
        map (displaySExp . C.require) requires

    -- FFIファイルのファイル名と最終的なファイル名が異なるため,FFIファイルには
    -- provide は書かず, load する。FFIファイルは対応するelモジュールファイルか
    -- らしかloadされず,elモジュールファイルのほうはrequireされるので複数回load
    -- されることはない。loadはロード時にメッセージを出すが第3引数にtを渡すこと
    -- で抑制できる。
    --
    -- require側で provideするよることも考えたが, requireされた側にprovideがなかっ
    -- た場合emacsが例外を投げる。
    --
    -- 逆にFFIファイル内ではprovideを書いてはいけない(チェックするべき)。例えば
    -- Data/Eq.el という FFIファイルは Dat.Eq._FOREIGN_.el というファイルにコピー
    -- される。
    loadFFILine = case requireFFI of
        Just (FeatureName (UnsafeSymbol name), _) ->
            [displaySExp $ list [symbol "load", string name, C.nil, C.t]]
        Nothing -> []

    defVarLines =
        map displayDefVar defVars

    provideLine =
        displaySExp $ C.provide name

displayDefVar :: DefVar -> Utf8Builder
displayDefVar DefVar{name, definition} =
    displaySExp $ list [symbol "defvar", symbol name, definition]

displaySExp :: SExp -> Utf8Builder
displaySExp = para display''

-- 遅延評価だからコレ重くないはず？遅い場合は諦めて displayPcase を実装する。
-- 綺麗にやるなら Pcaseコンストラクタは SExpF から分離するべきなんだろうが..
display'' :: SExpF (SExp, Utf8Builder) -> Utf8Builder
display'' (Pcase exprs cases) = displaySExp $ convPcase (fst <$> exprs) (fmap (fst <$>) cases)
display'' s = display' $ snd <$> s

display' :: SExpF Utf8Builder -> Utf8Builder
display' (Integer i) = display i
display' (Double d) = display d
display' (String t) = displayString t
display' (Character c) = "?" <> display c
display' (Symbol sym) = displaySymbol sym
display' (Cons car cdr) = paren [car <> " . " <> cdr]
display' (List xs) = paren xs
display' (Vector xs) = bracket xs
display' (Cond cases) = displayCond cases
display' (Let letType binds body) = displayLetish letType binds body
display' (Lambda1 arg body) = paren $ ["lambda", paren [displaySymbol arg]] <> body
display' (Quote s) = "'" <> s
display' (Backquote s) = "`" <> s
display' (Comma s) = "," <> s
display' (Pcase _ _) = error "Unexpected"

displayCond :: [(Utf8Builder, Utf8Builder)] -> Utf8Builder
displayCond cases =
    paren $
        displaySymbol "cond" :
        map (\(p, e) -> paren [p, e]) cases

-- Let系
displayLetish :: LetType -> [(Symbol, Utf8Builder)] -> [Utf8Builder] -> Utf8Builder
displayLetish letType binds body =
    paren $
        [ displaySymbol name
        , paren (map (\(s, e) -> paren [displaySymbol s, e]) binds)
        ]
            <> body
  where
    name = case letType of
        LetStar -> "let*"
        LetRec -> "letrec"

-- displayPcase :: [Utf8Builder] -> [PcaseAlt Utf8Builder] -> Utf8Builder
-- displayPcase exprs cases = _

convPcase :: [SExp] -> [PcaseAlt SExp] -> SExp
convPcase exprs cases =
    list $ [symbol "pcase", unifyExprs exprs] <> map caseAlt cases
  where
    unifyExprs = \case
        [] -> error "Empty pcase exprs"
        [s] -> s
        ss -> list (symbol "list" : ss)

    unifyPatterns = \case
        [] -> error "Empty pcase exprs"
        [p] -> p
        ps -> PBackquotedList (map Right ps)

    caseAlt PcaseAlt{patterns, guard, code} =
        list
            [ pattern' (unifyPatterns patterns) & maybe id addGuard guard
            , code
            ]
      where
        addGuard guard sexp =
            list [symbol "and", sexp, list [symbol "guard", guard]]

    pattern' :: PPattern SExp -> SExp
    pattern' PAny =
        symbol "_"
    pattern' (PInteger i) =
        integer i
    pattern' (PString s) =
        string s
    pattern' (PCharacter c) =
        character c
    pattern' (PBind sym) =
        symbol sym
    pattern' (PBackquotedList ps) =
        backquote . list $ map (either symbol (commaMaybe . pattern')) ps
    pattern' (PBackquotedVector ps) =
        backquote . vector $ map (either symbol (commaMaybe . pattern')) ps
    pattern' (PAnd ps) =
        list $ symbol "and" : map pattern' ps
    pattern' (PPred pred) =
        list [symbol "pred", pred]
    pattern' (PPredBool b) =
        list [symbol "pred", bool (symbol "null") (symbol "identity") b]
    pattern' (PApp s p) =
        list [symbol "app", s, pattern' p]

    -- 入れ子になっている不要な (`) は取り除かれる。
    -- e.g. `[ ... ,`[...] -> `[ ... [...]
    commaMaybe :: SExp -> SExp
    commaMaybe (SExp (Backquote se)) =
        se
    commaMaybe s
        | isLiteral s = s
        | otherwise = comma s

-- 取り敢えず雑なquotingで
displayString :: Text -> Utf8Builder
displayString t = "\"" <> display (quote t) <> "\""
  where
    quote = T.replace "\"" "\\\""

-- elispでは
-- https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Type.html
-- まずエスケープが必要な文字に関しては \ を前置。
-- 文字レベルでエスケープが不要としても全体として数値として解釈されてしまうものは先頭を\でエスケープする。
displaySymbol :: Symbol -> Utf8Builder
displaySymbol (UnsafeSymbol s) = display $ escape s
  where
    -- 取り敢えず[0-9a-zA-Z-+=*/_~!@$%^&:<>{}_] 以外は \ でエスケープすることにする。
    -- ? もエスケープは不要なのだが ?a など文字リテラルと
    -- ただし文字リテラル(?a)や数値として解釈されるもの(-1.5)などは先頭に \ を付ける必要がある
    escape =
        (RE.*=~/ [RE.ed|[^[:alnum:].+=*/_~!@$%^&:<>{}_-]///\${0}|])
    isNumberLiteral =
        RE.matched . (RE.?=~ [RE.re|^[+-]?[[:digit:]]*(\.[[:digit:]]+)?$|])
    unambiguousify t =
        bool id ("\\" <>) $ isNumberLiteral t

paren :: [Utf8Builder] -> Utf8Builder
paren xs = "(" <> mconcat (intersperse " " xs) <> ")"

bracket :: [Utf8Builder] -> Utf8Builder
bracket xs = "[" <> mconcat (intersperse " " xs) <> "]"
