{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module PsEl.SExpPrinter where

import Data.List (intersperse)
import PsEl.SExp
import RIO hiding (bracket)
import RIO.Text.Partial qualified as T

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
        map (onFeature "require") requires

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
            [displaySExp $ list [symbol "load", string name, symbol "nil", symbol "t"]]
        Nothing -> []

    defVarLines =
        map displayDefVar defVars

    provideLine =
        onFeature "provide" name

    onFeature op (FeatureName name) =
        displaySExp $ list [symbol op, quote (symbol name)]

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
    display' (Lambda1 arg body) = paren $ ["lambda", paren [displaySymbol arg]] <> body
    display' (Quote s) = "'" <> s
    display' (Backquote s) = "`" <> s
    display' (Comma s) = "," <> s

    cata :: (SExpF r -> r) -> SExp -> r
    cata f = go
      where
        go = f . fmap go . unSExpr

-- 取り敢えず雑なquotingで
displayString :: Text -> Utf8Builder
displayString t = "\"" <> display (quote t) <> "\""
  where
    quote = T.replace "\"" "\\\""

-- 空白を含むシンボルってないよな..?
displaySymbol :: Symbol -> Utf8Builder
displaySymbol (UnsafeSymbol s) = display s

paren :: [Utf8Builder] -> Utf8Builder
paren xs = "(" <> (mconcat $ intersperse " " xs) <> ")"

bracket :: [Utf8Builder] -> Utf8Builder
bracket xs = "[" <> (mconcat $ intersperse " " xs) <> "]"
