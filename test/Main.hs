{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.Providers

import Path
import Path.IO

import System.Process.Typed
import System.Exit

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

main :: IO ()
main = do
  sourceFiles <- getHsFiles [reldir|src|]
  shouldPassFiles <- getHsFiles [reldir|test/should-pass|]
  shouldFailFiles <- getHsFiles [reldir|test/should-fail|]

  sourceTests <- mapM (runLiquid True) sourceFiles
  shouldPassTests <- mapM (runLiquid True) shouldPassFiles
  shouldFailTests <- mapM (runLiquid False) shouldFailFiles
 
  defaultMain $ testGroup "Liquid Haskell Tests" [
    testGroup "Sources" sourceTests,
    testGroup "Should Pass" shouldPassTests,
    testGroup "Should Fail" shouldFailTests
    ]

getHsFiles :: Path b Dir -> IO [Path Abs File]
getHsFiles path = do
  (_, files) <- listDirRecur path
  return $ filter isHsFile files
  where
    isHsFile file = fileExtension file == ".hs"

runLiquid :: Bool -> Path Abs File -> IO TestTree
runLiquid shouldPass file = do
  relativeFile <- makeRelativeToCurrentDir file
  return $ singleTest
    ("Running liquid on " ++ (toFilePath (relativeFile)))
    (RunLiquid file shouldPass)

data RunLiquid = RunLiquid {
  runLiquidFile :: Path Abs File,
  runLiquidShouldPass :: Bool
  }

instance IsTest RunLiquid where
  run _ (RunLiquid file shouldPass) _ = do
    (exitCode, stdout, stderr)  <- readProcess liquid
    return $
      if (isSuccess exitCode) == shouldPass then
        testPassed ""
      else if shouldPass then
        testFailed $ T.unpack $ T.unlines [
          "STDOUT:",
          T.decodeUtf8 stdout,
          "",
          "STDERR:",
          T.decodeUtf8 stderr
          ]
      else
        testFailed "" -- We don't get useful output when liquid succeeds.
    where
      liquid = proc "liquid" [
        toFilePath file,
        "--idirs=src",
        "--reflection",
        "--ple"
        ]
      isSuccess exitCode = exitCode == ExitSuccess

  testOptions = return []
  
