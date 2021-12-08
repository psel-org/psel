{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PsEl.Main where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Language.PureScript (ModuleName (ModuleName), getModuleName)
import Language.PureScript.CoreFn qualified as P
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import PsEl.SExp (Feature (..), featureFileName)
import PsEl.SExpPrinter (displayFeature)
import PsEl.Transpile (transpile)
import RIO
import RIO.ByteString.Lazy qualified as TL
import RIO.Directory qualified as Dir
import RIO.FilePath (replaceExtension, (</>))
import RIO.FilePath qualified as FP
import RIO.List (intercalate)
import RIO.Text (unpack)
import System.Exit qualified as Sys
import System.IO (hPutStrLn, putStrLn)
import Text.Pretty.Simple (pPrint, pShow)

defaultMain :: IO ()
defaultMain = do
    let workdir = "."
    let moduleRoot = workdir </> "output"
    let elispRoot = workdir </> "output.el"
    whenM (Dir.doesDirectoryExist elispRoot) $ Dir.removeDirectoryRecursive elispRoot
    Dir.createDirectory elispRoot
    moduleDirs <- filter (/= "cache-db.json") <$> Dir.listDirectory moduleRoot
    forM_ moduleDirs $ \rel -> do
        let coreFnPath = moduleRoot </> rel </> "corefn.json"
        value <- Aeson.eitherDecodeFileStrict coreFnPath >>= either Sys.die pure
        (_version, module') <- either Sys.die pure $ parseEither moduleFromJSON value
        let P.Module{P.moduleName, P.modulePath} = module'
        let feature@Feature{name, requireFFI} = transpile module'
        let targetPath = elispRoot </> featureFileName name
        let foreignSourcePath = replaceExtension modulePath "el"
        writeFileUtf8Builder targetPath (displayFeature feature)
        hasForeignSource <- Dir.doesFileExist foreignSourcePath
        warnings <- case (hasForeignSource, requireFFI) of
            (True, Just ffiName) -> do
                -- ForeignFileが(provide)を行なっている場合警告を出す必要があるかも。
                let foreignTargetPath = elispRoot </> featureFileName ffiName
                Dir.copyFile foreignSourcePath foreignTargetPath
                pure []
            (True, Nothing) ->
                pure [modulePath <> " has an FFI file, but does not use FFI!"]
            (False, Just ffiName) -> do
                let foreignTargetPath = elispRoot </> featureFileName ffiName
                pure
                    [ modulePath <> " calls foreign functions, but has no associated FFI file!"
                    , "You can write the missing FFI file yourself as " <> FP.takeFileName foreignTargetPath <> " and place under load-path"
                    ]
            (False, Nothing) ->
                pure []
        when (not (null warnings)) $ do
            let ModuleName mn = moduleName
            hPutStrLn stderr $ unpack mn
            traverse_ (hPutStrLn stderr) $ map ("WARNING: " <>) warnings
