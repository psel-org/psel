{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PsEl.SExpDisplay where

import Data.Functor.Foldable (cata, para)
import Data.List (intersperse)
import PsEl.SExp
import PsEl.SExpConstructor qualified as C
import PsEl.SExpRaw qualified as Raw
import RIO hiding (bracket)
import RIO.Lens (_1, _2)
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
            [displaySExp $ funcallNative "load" [string name, C.nil, C.t]]
        Nothing -> []

    defVarLines =
        map displayDefVar defVars

    provideLine =
        displaySExp $ C.provide name

displayDefVar :: DefVar -> Utf8Builder
displayDefVar DefVar{name, definition} =
    displaySExp $ funcallNative "defvar" [symbol name, definition]

displaySExp :: SExp -> Utf8Builder
displaySExp = cata displaySExpFRaw . convSExp

convSExp :: SExp -> Raw.SExp
convSExp = cata conv
  where
    conv :: SExpF Raw.SExp -> Raw.SExp
    conv (Integer i) = Raw.integer i
    conv (Double d) = Raw.double d
    conv (String s) = Raw.string s
    conv (Character c) = Raw.character c
    conv (Symbol sym) = Raw.symbol sym
    conv (MkAlist xs) = convMkAlist xs
    conv (Progn es) = convProgn es
    conv (If e et ee) = Raw.list [Raw.symbol "if", e, et, ee]
    conv (Cond alts) = Raw.list $ Raw.symbol "cond" : map (\(p, e) -> Raw.list (p : eraseProgn e)) alts
    conv (Let letType binds body) = convLetish letType binds body
    conv (Pcase exprs cases) = convPcase exprs cases
    conv (Lambda1 arg body) = Raw.list $ [Raw.symbol "lambda", Raw.list [Raw.symbol arg]] <> eraseProgn body
    conv (Lambda0 body) = Raw.list $ [Raw.symbol "lambda", Raw.list []] <> eraseProgn body
    conv (FunCall1 f arg) = Raw.list [Raw.symbol "funcall", f, arg]
    conv (FunCall0 f) = Raw.list [Raw.symbol "funcall", f]
    conv (FunCallNative sym args) = Raw.list $ Raw.symbol sym : args
    conv (QuotedSymbol qs) = Raw.quote $ Raw.symbol qs

-- erase uneeded progns
eraseProgn :: Raw.SExp -> [Raw.SExp]
eraseProgn (Raw.SExp (Raw.Progn es)) = es
eraseProgn e = [e]

-- remvoe nested progns
convProgn :: [Raw.SExp] -> Raw.SExp
convProgn = Raw.progn . foldr go []
  where
    go :: Raw.SExp -> [Raw.SExp] -> [Raw.SExp]
    go (Raw.SExp (Raw.Progn es)) = (es <>)
    go e = (e :)

convMkAlist :: [(Symbol, Raw.SExp)] -> Raw.SExp
convMkAlist =
    Raw.backquote
        . Raw.list
        . map (uncurry Raw.cons . over _2 comma' . over _1 Raw.symbol)
  where
    comma' s
        | Raw.isLiteral s = s
        | otherwise = Raw.comma s

-- Let系
convLetish :: LetType -> [(Symbol, Raw.SExp)] -> Raw.SExp -> Raw.SExp
convLetish letType binds body =
    Raw.list $
        [ Raw.symbol name
        , Raw.list (map (\(s, e) -> Raw.list [Raw.symbol s, e]) binds)
        ]
            <> eraseProgn body
  where
    name = case letType of
        LetStar -> "let*"
        LetRec -> "letrec"

-- displayPcase :: [Utf8Builder] -> [PcaseAlt Utf8Builder] -> Utf8Builder
-- displayPcase exprs cases = _

convPcase :: [Raw.SExp] -> [PcaseAlt Raw.SExp] -> Raw.SExp
convPcase exprs cases =
    Raw.list $ [Raw.symbol "pcase", unifyExprs exprs] <> map caseAlt cases
  where
    unifyExprs = \case
        [] -> error "Empty pcase exprs"
        [s] -> s
        ss -> Raw.list (Raw.symbol "list" : ss)

    unifyPatterns = \case
        [] -> error "Empty pcase exprs"
        [p] -> p
        ps -> PBackquotedList (map Right ps)

    caseAlt PcaseAlt{patterns, guard, code} =
        Raw.list $
            [ pattern' (unifyPatterns patterns) & maybe id addGuard guard
            ]
                <> eraseProgn code
      where
        addGuard guard sexp =
            Raw.list [Raw.symbol "and", sexp, Raw.list [Raw.symbol "guard", guard]]

    pattern' :: PPattern Raw.SExp -> Raw.SExp
    pattern' PAny =
        Raw.symbol "_"
    pattern' (PInteger i) =
        Raw.integer i
    pattern' (PString s) =
        Raw.string s
    pattern' (PCharacter c) =
        Raw.character c
    pattern' (PBind sym) =
        Raw.symbol sym
    pattern' (PBackquotedList ps) =
        Raw.backquote . Raw.list $ map (either Raw.symbol (commaMaybe . pattern')) ps
    pattern' (PBackquotedVector ps) =
        Raw.backquote . Raw.vector $ map (either Raw.symbol (commaMaybe . pattern')) ps
    pattern' (PAnd ps) =
        Raw.list $ Raw.symbol "and" : map pattern' ps
    pattern' (PPred pred) =
        Raw.list [Raw.symbol "pred", pred]
    pattern' (PPredBool b) =
        Raw.list [Raw.symbol "pred", bool (Raw.symbol "null") (Raw.symbol "identity") b]
    pattern' (PApp s p) =
        Raw.list [Raw.symbol "app", s, pattern' p]

    -- 入れ子になっている不要な (`) は取り除かれる。
    -- e.g. `[ ... ,`[...] -> `[ ... [...]
    commaMaybe :: Raw.SExp -> Raw.SExp
    commaMaybe (Raw.SExp (Raw.Backquote se)) =
        se
    commaMaybe s
        | Raw.isLiteral s = s
        | otherwise = Raw.comma s

displaySExpFRaw :: Raw.SExpF Utf8Builder -> Utf8Builder
displaySExpFRaw (Raw.Integer i) = display i
displaySExpFRaw (Raw.Double d) = display d
displaySExpFRaw (Raw.String t) = displayString t
displaySExpFRaw (Raw.Character c) = "?" <> display c
displaySExpFRaw (Raw.Symbol sym) = displaySymbol sym
displaySExpFRaw (Raw.Cons car cdr) = paren [car <> " . " <> cdr]
displaySExpFRaw (Raw.Progn es) = paren $ "progn" : es
displaySExpFRaw (Raw.List xs) = paren xs
displaySExpFRaw (Raw.Vector xs) = bracket xs
displaySExpFRaw (Raw.Quote s) = "'" <> s
displaySExpFRaw (Raw.Backquote s) = "`" <> s
displaySExpFRaw (Raw.Comma s) = "," <> s

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
