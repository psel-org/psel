{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PsEl.SExpPrinter where

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
displaySExp = cata display'
  where
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

    cata :: (SExpF r -> r) -> SExp -> r
    cata f = go
      where
        go = f . fmap go . unSExpr

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
