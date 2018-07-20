-- | A program for reifying a .cabal file into a Bazel structure.
--
-- Takes as input a Cabal file and a GHC version.
-- Evaluates all of the flags in the file to generate a PackageDescription,
-- and then writes out a `.bzl` file containing a single definition:
--
--    package = struct(...)
--
-- which can be loaded into Bazel BUILD files.
{-# LANGUAGE CPP #-}
module Main (main) where

#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
    (readGenericPackageDescription, parseHookedBuildInfo, runParseResult)
#else
import Distribution.PackageDescription.Parse
    (readGenericPackageDescription, parseHookedBuildInfo, ParseResult(..))
#endif
import Distribution.Text (display, simpleParse)
import Distribution.Verbosity (normal)
import System.Environment (getArgs)
import System.FilePath ((<.>))
import System.Process (callProcess)

#if MIN_VERSION_Cabal(2,2,0)
import qualified Data.ByteString as BS
#endif
import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as P
import qualified System.Directory as Directory

import Description
import Flatten
import Skylark

main :: IO ()
main = do
    [ghcVersionStr, cabalFile, outFile] <- getArgs
    gdesc <- readGenericPackageDescription normal cabalFile
    let ghcVersion = case simpleParse ghcVersionStr of
                      Nothing -> error $ "Error parsing ghc version: " ++ show ghcVersionStr
                      Just v -> v
    desc <- maybeConfigure $ flattenToDefaultFlags ghcVersion gdesc
    writeFile outFile $ show $ renderStatements
        [Assign "package" $ packageDescriptionExpr desc]

maybeConfigure :: P.PackageDescription -> IO P.PackageDescription
maybeConfigure desc = case P.buildType desc of
#if MIN_VERSION_Cabal(2,2,0)
    P.Configure -> do
#else
    Just P.Configure -> do
#endif
        callProcess "./configure" []
        let buildInfoFile = display (P.packageName desc) <.> "buildinfo"
        buildInfoExists <- Directory.doesFileExist buildInfoFile
        if buildInfoExists
          then do
#if MIN_VERSION_Cabal(2,2,0)
              hookedBIParse <- runParseResult . parseHookedBuildInfo <$> BS.readFile buildInfoFile
              case hookedBIParse of
                  (_, Left (_, e)) -> error $ "Error reading buildinfo " ++ show buildInfoFile
                                                ++ ": " ++ show e
                  (_, Right hookedBI) -> return $ P.updatePackageDescription hookedBI desc
#else
              hookedBIParse <- parseHookedBuildInfo <$> readFile buildInfoFile
              case hookedBIParse of
                  ParseFailed e -> error $ "Error reading buildinfo " ++ show buildInfoFile
                                              ++ ": " ++ show e
                  ParseOk _ hookedBI -> return $ P.updatePackageDescription hookedBI desc
#endif
          else return desc
    _ -> return desc
