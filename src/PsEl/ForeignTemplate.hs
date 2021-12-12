{-# LANGUAGE OverloadedStrings #-}

module PsEl.ForeignTemplate where

import PsEl.SExp (DefVar (DefVar, definition, name), SExp (SExp), Symbol, symbol)
import PsEl.SExpPrinter qualified as Printer
import RIO
import RIO.List (intersperse)

-- 必要なforeign識別子からFFIファイルのテンプレートを作成。
foreignTemplate :: [Symbol] -> Utf8Builder
foreignTemplate symbols =
    mconcat . intersperse "\n\n" $
        mconcat
            [ headLines
            , defVarLines
            ]
  where
    headLines =
        [ ";; -*- lexical-binding: t; -*-"
        ]

    defVarLines =
        map Printer.displayDefVar templateDefVars

    templateDefVars =
        map (\s -> DefVar{name = s, definition = placehold}) symbols

    -- わざと束縛されていないシンボルを使うことでrequire時に実装漏れで例外投げるように
    placehold =
        symbol "foreign-not-implemeneted"
