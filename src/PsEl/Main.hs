{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PsEl.Main where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Language.PureScript (ModuleName (ModuleName))
import Language.PureScript.CoreFn qualified as P
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import PsEl.SExp (featureFileName)
import PsEl.SExpPrinter (displayFeature)
import PsEl.Transpile (transpile)
import RIO
import RIO.ByteString.Lazy qualified as TL
import RIO.Directory qualified as Dir
import RIO.FilePath ((</>))
import RIO.FilePath qualified as FP
import RIO.List (intercalate)
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
        let dir = moduleRoot </> rel
        let coreFnPath = dir </> "corefn.json"
        value <- Aeson.eitherDecodeFileStrict coreFnPath >>= either Sys.die pure
        (_version, module') <- either Sys.die pure $ parseEither moduleFromJSON value
        let feature = transpile module'
        let path = elispRoot </> featureFileName feature
        writeFileUtf8Builder path (displayFeature feature)

-- let (nix, ModuleInfo usesFFI interpolations) = convert module'
-- writeFileUtf8Builder (dir </> "default.nix") (renderExpr nix)
-- let modulePath = P.modulePath module'
--     foreignSrc = workdir </> FP.replaceExtension modulePath "nix"
--     foreignTrg = dir </> "foreign.nix"
-- hasForeign <- Dir.doesFileExist foreignSrc
-- case (hasForeign, usesFFI) of
--     (True, True) -> Dir.copyFile foreignSrc foreignTrg
--     (True, False) -> hPutStrLn stderr $ "Warning: " <> modulePath <> " has an FFI file, but does not use FFI!"
--     (False, True) -> hPutStrLn stderr $ "Warning: " <> modulePath <> " calls foreign functions, but has no associated FFI file!"
--     (False, False) -> pure ()
-- unless (null interpolations) $ do
--     hPutStrLn stderr $
--         unlines
--             [ "Warning: " <> modulePath <> " appears to perform Nix string interpolation in the following locations:"
--             , "  " <> intercalate ", " (show <$> toList interpolations)
--             , "Nix string interpolations are currently not officially supported and may cause unexpected behavior."
--             ]
