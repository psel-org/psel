{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module PsEl.CommandRun (run) where

import Data.Coerce (Coercible, coerce)
import Language.PureScript.CST (ParserState (ParserState), lex, parseQualIdentP, runParser)
import Language.PureScript.CST.Types (Ident (getIdent), QualifiedName (QualifiedName, qualModule, qualName))
import Language.PureScript.Names (Ident (Ident))
import PsEl.SExp (symbol)
import PsEl.SExpConstructor qualified as C
import PsEl.SExpPrinter (displaySExp)
import PsEl.Transpile qualified as Trp
import RIO
import RIO.Lens (_1, _2)
import RIO.Process (HasProcessContext (processContextL), ProcessContext, mkDefaultProcessContext, proc, runProcess_)
import RIO.Text (unpack)

-- parseQualIdentP :: Parser (QualifiedName Ident) を
-- e.g. mainFunc = Test.Main.main
-- emacsに渡す引数は次のようになる。
--
--   emacs --batch --directory output.el  --eval "(require 'Test.Main)" --eval "(funcall Test.Main.main)"
--
-- デバッグしたい場合は --funcall toggle-debug-on-error を --evalの前に入れれば良い
run :: HasLogFunc env => Text -> RIO env ()
run mainFunc = do
    pc <- mkDefaultProcessContext
    env <- ask
    runRIO (pc, env) $ do
        case snd (parseMainFunc mainFunc) of
            Right QualifiedName{qualModule = Just mn, qualName = ident} -> do
                let featureName = Trp.featureName mn
                let mainFuncSym = symbol $ Trp.globalVar mn (Ident (getIdent ident))
                let args =
                        mconcat
                            [ ["--batch"]
                            , ["--directory", "output.el"]
                            , ["--eval", showSExp $ C.require featureName]
                            , ["--eval", showSExp $ C.funcall0 mainFuncSym]
                            ]
                proc "emacs" args runProcess_
            Right _ ->
                logError "Unexpected"
            Left ne ->
                logError "Unexpected"
  where
    parseMainFunc t =
        runParser (ParserState (lex t) [] []) parseQualIdentP

    showSExp =
        unpack . utf8BuilderToText . displaySExp

-- newtype Ctx e = Ctx (ProcessContext, e)
-- orphan intance にはならない???

instance HasProcessContext (ProcessContext, e) where
    processContextL = _1

instance HasLogFunc e => HasLogFunc (a, e) where
    logFuncL = _2 . logFuncL
