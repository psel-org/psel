{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic (ParseRecord (parseRecord), getRecord, lispCaseModifiers, parseRecordWithModifiers)
import PsEl.CommandBuild qualified as CommandBuild
import PsEl.CommandRun qualified as CommandRun
import RIO
import System.Environment (getArgs)

data Option = Option
    { generateMissingFfi :: Bool
    , run :: Maybe Text
    }
    deriving (Generic, Show)

instance ParseRecord Option where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

-- spagoは一つのコマンドで複数回バックエンドコマンドを起動する
-- 例えばspago test はビルドを意味する無引数で実行した後,
-- --run Test.Main.main コマンドでテストを実行する。
-- ["--run","Test.Main.main"]
main :: IO ()
main = do
    Option{generateMissingFfi, run} <- getRecord "psel"
    runSimpleApp $ do
        case run of
            Just mainFunc -> do
                CommandRun.run mainFunc
            Nothing -> do
                let config =
                        CommandBuild.defaultConfig
                            { CommandBuild.generateMissingFfi = generateMissingFfi
                            }
                liftIO $ CommandBuild.build config
