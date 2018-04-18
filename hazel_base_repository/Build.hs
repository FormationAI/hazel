module Build
    ( PackageFiles
    , buildRules
    ) where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, maybeToList)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Simple.Build.Macros (generatePackageVersionMacros)
import Distribution.Text (display)
import Language.Haskell.Extension (Language(Haskell98))
import System.FilePath ((<.>), (</>))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as P
import qualified Distribution.Version as Version

import Skylark

-- | The set of files in this distribution.
-- Used for resolving the locations of modules and header files.
type PackageFiles = Set.Set FilePath

type PackageList = Map.Map P.PackageName Version.Version

-- | Generates a BUILD file which compiles the given package.
buildRules :: PackageFiles -> PackageList -> PackageList -> P.PackageDescription -> [Statement]
buildRules packageFiles packages prebuilt pkg =
    [ Load "@io_tweag_rules_haskell//haskell:haskell.bzl"
        ["haskell_library"]
    , Load "@ai_formation_hazel//:hazel.bzl"
        ["hazel_paths_module", "hazel_writefile"]
    , Rule "package" ["default_visibility" =: ["//visibility:public"]]
    , Rule "filegroup" ["name" =: "files", "srcs" =: allRuleNames]
    , Rule "hazel_paths_module"
        [ "name" =: display (pathsModule (P.packageName pkg))
        , "version_number" =: Version.versionNumbers (P.packageVersion pkg)
        ]
    , Rule "haskell_library"
        [ "name" =: "paths"
        , "srcs" =: [":" ++ display (pathsModule $ P.packageName pkg)]
        , "prebuilt_dependencies" =: ["base", "Cabal"]
        ]
    -- TODO: janky
    , Rule "hazel_writefile"
        [ "name" =: "cabal_macros"
        , "output" =: "cabal-macros.h"
        -- TODO: filter
        , "contents" =: generatePackageVersionMacros
                          . map (\n -> P.PackageIdentifier n (get allAll n))
                          $ allDependencies
        ]
    ]
    ++
    allRules
  where
    allAll = packages `mappend` prebuilt
    allRules = foldMap (renderLibrary packageFiles prebuilt pkg) (P.library pkg)
    allRuleNames = [":" ++ n | Rule _ es <- allRules, ("name", ExprString n) <- es]
    allDependencies =
          map P.depPkgName
          . concatMap P.targetBuildDepends
            $ map P.libBuildInfo (maybeToList $ P.library pkg)
            ++ map P.buildInfo (P.executables pkg) 
            ++ map P.testBuildInfo (P.testSuites pkg) 
            ++ map P.benchmarkBuildInfo (P.benchmarks pkg) 
    get m k = case Map.lookup k m of
                Nothing -> error $ "Missing key " ++ show k
                Just x -> x

locateModule :: [FilePath] -> PackageFiles -> ModuleName.ModuleName -> FilePath
locateModule srcDirs fs m
    | f:_ <- filter (`Set.member` fs)
                      $ map (\d -> d </> ModuleName.toFilePath m <.> ".hs")
                          srcDirs
          = f
    | otherwise = error $ "Couldn't locate module " ++ display m
                            ++ " in source directories " ++ show srcDirs
                            ++ ": " ++ show fs

renderLibrary :: PackageFiles -> PackageList -> P.PackageDescription -> P.Library -> [Statement]
renderLibrary packageFiles prebuilt pkg lib
    | null (P.exposedModules lib)
        = [Rule "cc_library"
              [ "name" =: "lib-" ++ display (P.packageName pkg)
              ]
          ]
    | otherwise =
    [ Rule "haskell_library"
        [ "name" =: "lib-" ++ display (P.packageName pkg)
        , "srcs" =: map (locateModule (prepDirs $ P.hsSourceDirs bi) packageFiles)
                        $ filter (/= pathsMod)
                        $ P.exposedModules lib ++ P.otherModules bi
        , "hidden_modules" =: map display $ filter (/= pathsMod) $ P.otherModules bi
        , "deps" =: ":cbits-lib" : ":cbits-extra" : haskellDeps
                    ++ if pathsMod `elem` (P.exposedModules lib ++ P.otherModules bi)
                          then [":paths"]
                          else []
        , "prebuilt_dependencies" =: map display . filter (`Map.member` prebuilt)
                                      $ allDeps
        , "src_strip_prefix" =: pre
        , "compiler_flags" =: map ("-X" ++)
                    (display (fromMaybe Haskell98 $ P.defaultLanguage bi)
                        : map display (P.defaultExtensions bi ++ P.oldExtensions bi))
                    ++ filterOptions (concat [opts | (GHC,opts) <- P.options bi])
                    ++ map ("-optP" ++) (P.cppOptions bi)
                    ++ ["-optP-include", "-optPcabal-macros.h"]
                    ++ ["-w"] -- We don't care about warnings
                              -- (TODO: configure this)
        ]
    , Rule "cc_library"
        [ "name" =: "cbits-lib"
        , "srcs" =: P.cSources bi
        , "hdrs" =: ExprOp (expr . Set.toList
                      . Set.fromList
                      $ filter (`Set.member` packageFiles)
                      $ [ d </> f
                        | d <- prepDirs $ P.includeDirs bi
                        , f <- P.includes bi ++ P.installIncludes bi
                        ])
                      "+"
                     (glob . filter (headerPre `isPrefixOf`) $ P.extraSrcFiles pkg)
        -- TODO: janky.  Why is this needed again, when toktok doesn't want
        -- it?
        , "strip_include_prefix" =: headerPre
        -- TODO: don't hard-code in "@ghc"
        , "deps" =: ["@ghc//:threaded-rts"]
        ]
    , Rule "cc_library"
        [ "name" =: "cbits-extra"
        , "hdrs" =: ExprOp (expr [":cabal_macros"])
                    "+"
                      (glob
                      . filter (not . isPrefixOf headerPre)
                      . Set.toList . Set.fromList
                      $ P.extraSrcFiles pkg)
        ]
    ]
  where
    pathsMod = pathsModule (P.packageName pkg)
    bi = P.libBuildInfo lib
    allDeps = map P.depPkgName $ P.targetBuildDepends bi
    haskellDeps =  map (\d -> "@haskell_" ++ d ++ "//:lib-" ++ d)
                      . map display
                      . filter (`Map.notMember` prebuilt)
                      $ allDeps
    -- TODO: this will fail if there's more than one...
    pre = head $ prepDirs $ P.hsSourceDirs bi
    headerPre = head $ prepDirs $ P.includeDirs bi

filterOptions :: [String] -> [String]
filterOptions = filter (`notElem` badOptions)
  where
    badOptions = ["-O0", "-O", "-O2", "-Wall", "-Werror", "-Wwarn"]

glob :: [String] -> Expr
glob fs = ExprCall "glob" ["include" =: fs]

prepDirs :: [FilePath] -> [FilePath]
prepDirs [] = [""]
prepDirs fs = fs

pathsModule :: P.PackageName -> ModuleName.ModuleName
pathsModule n = ModuleName.fromString $ "Paths_" ++ map fixHyphen (display n)
  where
    fixHyphen '-' = '_'
    fixHyphen c = c
