module Main where

import PsEl.Main
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    defaultMain $ mkConfig args
  where
    mkConfig [] = defaultConfig
    mkConfig ["--generate-missing-foreign-files"] =
        defaultConfig
            { generateMissingForeignFiles = True
            }
