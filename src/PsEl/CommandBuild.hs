{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PsEl.CommandBuild (
    build,
    Config (..),
    defaultConfig,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Language.PureScript (ModuleName (ModuleName))
import Language.PureScript.CoreFn qualified as P
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import PsEl.ForeignTemplate (foreignTemplate)
import PsEl.PselEl (pselEl)
import PsEl.SExp (Feature (..), Symbol, featureFileName)
import PsEl.SExpOptimize (optimize)
import PsEl.SExpDisplay (displayFeature, displayString)
import PsEl.Transpile (ffiFeatureSuffix, pselFeature, transpile)
import RIO
import RIO.Directory qualified as Dir
import RIO.FilePath ((</>))
import RIO.FilePath qualified as FP
import RIO.List (intersperse, sort)
import RIO.Text (justifyLeft, pack, unpack)
import RIO.Text qualified as T
import System.Exit qualified as Sys
import System.IO (hPutStrLn, putStrLn)

build :: Config -> IO ()
build Config{generateMissingFfi} = do
    let workdir = "."
    let moduleRoot = workdir </> "output"
    let elispRoot = workdir </> "output.el"
    whenM (Dir.doesDirectoryExist elispRoot) $ Dir.removeDirectoryRecursive elispRoot
    Dir.createDirectory elispRoot
    writeFileUtf8 (elispRoot </> featureFileName pselFeature) (pselEl pselFeature)
    moduleDirs <- filter (/= "cache-db.json") <$> Dir.listDirectory moduleRoot
    warngings' <- forM moduleDirs $ \rel -> do
        let coreFnPath = moduleRoot </> rel </> "corefn.json"
        value <- Aeson.eitherDecodeFileStrict coreFnPath >>= either Sys.die pure
        (_version, module') <- either Sys.die pure $ parseEither moduleFromJSON value
        handleModule elispRoot module'
    let warnings = mconcat warngings'
    printWarnings warnings
    when generateMissingFfi
        . doGenerateMissingForeignFiles
        . snd
        $ partitionEithers warnings

data Config = Config
    { generateMissingFfi :: Bool
    }

defaultConfig :: Config
defaultConfig =
    Config
        { generateMissingFfi = False
        }

-- (1)
-- ForeignFileが(provide)を行なっている場合警告を出す必要があるかも。
-- また必要なFFI定義を
handleModule :: FilePath -> P.Module P.Ann -> IO [Warning]
handleModule elispRoot module'@P.Module{P.moduleName, P.modulePath} = do
    let feature = optimize $ transpile module'
    let Feature{name, requireFFI} = feature
    let targetPath = elispRoot </> featureFileName name
    let foreignSourcePath = FP.replaceExtension modulePath "el"
    writeFileUtf8Builder targetPath (displayFeature feature)
    hasForeignSource <- Dir.doesFileExist foreignSourcePath
    case (hasForeignSource, requireFFI) of
        (True, Just (ffiName, _foreignSymbols)) -> do
            let foreignTargetPath = elispRoot </> featureFileName ffiName
            Dir.copyFile foreignSourcePath foreignTargetPath -- (1)
            pure []
        (True, Nothing) ->
            pure [Left UnneededFFIFileWarning{moduleName, modulePath}]
        (False, Just (ffiName, foreignSymbols)) -> do
            pure
                [ Right
                    MissingFFIFileWarning
                        { moduleName
                        , modulePath
                        , foreignSourcePath
                        , foreignSymbols
                        }
                ]
        (False, Nothing) ->
            pure []

-- PSコンパイラはpackage-awareではない。そのため当然corefnにはモジュールがどのパッ
-- ケージのどのバージョンのものかの情報は含まれていない。ただspago使っている場合,
-- PSのソースコードのパスでパッケージ名は推測できる。
--
-- 例えばspagoでpreludeをコンパイルした場合,Data.Eqモジュールは次のmodulePathを持つ。
--
-- e.g. "modulePath":".spago/prelude/master/src/Data/Eq.purs"
--
guessPackageByModulePath :: FilePath -> Maybe Text
guessPackageByModulePath path = do
    path' <- T.stripPrefix ".spago/" (pack path)
    let pkg = T.takeWhile (/= '/') path'
    guard $ not (T.null pkg)
    pure pkg

-- 依存ではなく現在のパッケージの
guessIsCurrentPackageByModulePath :: FilePath -> Bool
guessIsCurrentPackageByModulePath path =
    let path' = pack path
     in T.isPrefixOf "src/" path' || T.isPrefixOf "test/" path'

printWarnings :: [Warning] -> IO ()
printWarnings warnings = do
    let (unneeds, missings) = partitionEithers warnings
    unless (null unneeds) $ putStderrLn $ displayUnneedWarngins unneeds
    unless (null missings) $ putStderrLn $ displayMissingWarngins missings

displayUnneedWarngins :: [UnneededFFIFileWarning] -> Utf8Builder
displayUnneedWarngins [] =
    mempty
displayUnneedWarngins warnings =
    mconcat
        . intersperse "\n"
        $ mconcat [header, [""], modules, [""]]
  where
    header =
        [ "!!! WARNING !!!"
        , "These modules contains FFI file but does not use any FFI."
        , "These FFI files are ignored, so no worry, but this could be smell of a bug."
        ]

    modules = map display' warnings

    display' UnneededFFIFileWarning{moduleName = ModuleName mn} =
        display mn

displayMissingWarngins :: [MissingFFIFileWarning] -> Utf8Builder
displayMissingWarngins [] =
    mempty
displayMissingWarngins warnings =
    mconcat
        . intersperse "\n"
        $ mconcat [header, [""], modules, [""]]
  where
    header =
        [ "!!! WARNING !!!"
        , "These modules uses FFI but missing corresponding FFI file."
        , "If you require these module it will fail try requrieing its FFI file."
        ]

    modules =
        map display $ sort $ map displayText' warnings

    displayText' MissingFFIFileWarning{moduleName = ModuleName mn, modulePath} =
        displayColumns
            24
            [ "Package: " <> fromMaybe "--" (guessPackageByModulePath modulePath)
            , "Module: " <> mn
            ]

--
doGenerateMissingForeignFiles :: [MissingFFIFileWarning] -> IO ()
doGenerateMissingForeignFiles ws =
    go $
        filter
            ( \MissingFFIFileWarning{modulePath} ->
                guessIsCurrentPackageByModulePath modulePath
            )
            ws
  where
    go [] =
        putStdoutLn "No missing foreign file."
    go ws = do
        putStdoutLn "Generating missing foreign files.\n"
        forM_ ws $ \MissingFFIFileWarning{foreignSourcePath, foreignSymbols} -> do
            let template = foreignTemplate foreignSymbols
            writeFileUtf8Builder foreignSourcePath template
            putStdoutLn $ display (pack foreignSourcePath)
        putStdoutLn "\nNote: You don't need to provide feature(e.g. (provide 'Data.Eq)) in a foreign file."
        putStdoutLn "Foreign files will be copied with a diffirent file name."

putStderrLn :: Utf8Builder -> IO ()
putStderrLn ub = hPutBuilder stderr . getUtf8Builder $ ub <> "\n"

putStdoutLn :: Utf8Builder -> IO ()
putStdoutLn ub = hPutBuilder stdout . getUtf8Builder $ ub <> "\n"

displayColumns :: Int -> [Text] -> Text
displayColumns _ [] = mempty
displayColumns width vs =
    mconcat $ mapBut1 (justifyLeft width ' ' . (<> ",")) vs
  where
    mapBut1 :: (a -> a) -> [a] -> [a]
    mapBut1 f [] = []
    mapBut1 f [x] = [x]
    mapBut1 f (x : xs) = f x : mapBut1 f xs

type Warning =
    Either UnneededFFIFileWarning MissingFFIFileWarning

data UnneededFFIFileWarning = UnneededFFIFileWarning
    { moduleName :: ModuleName
    , modulePath :: FilePath
    }

data MissingFFIFileWarning = MissingFFIFileWarning
    { moduleName :: ModuleName
    , modulePath :: FilePath
    , foreignSourcePath :: FilePath
    , foreignSymbols :: [Symbol]
    }
