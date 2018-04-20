{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import Data.Maybe
import System.Exit

import Test.Tasty
import Test.Tasty.Providers

import Path
import Path.IO

import System.Process.Typed

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

main :: IO ()
main = do
  sourceFiles <- getHsFiles [reldir|src|]
  shouldPassFiles <- getHsFiles [reldir|test/should-pass|]
  shouldFailFiles <- getHsFiles [reldir|test/should-fail|]

  sourceTests <- mapM (runLiquid Safe) sourceFiles
  shouldPassTests <- mapM (runLiquid Safe) shouldPassFiles
  shouldFailTests <- mapM (runLiquid Unsafe) shouldFailFiles
 
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

runLiquid :: LiquidResult -> Path Abs File -> IO TestTree
runLiquid expectedResult file = do
  relativeFile <- makeRelativeToCurrentDir file
  return $ singleTest
    ("Running liquid on " ++ (toFilePath (relativeFile)))
    (RunLiquid file expectedResult)

data RunLiquid = RunLiquid (Path Abs File) LiquidResult

data LiquidResult = Safe | Unsafe
  deriving (Eq)

exitCodeToLiquidResult :: ExitCode -> Maybe LiquidResult
exitCodeToLiquidResult ExitSuccess = Just Safe
exitCodeToLiquidResult (ExitFailure 1) = Just Unsafe
exitCodeToLiquidResult _ = Nothing

instance IsTest RunLiquid where
  run _ (RunLiquid file expectedResult) _ = do
    (exitCode, stdout, stderr)  <- readProcess liquid
    return $ fromMaybe (testFailed $ failureString stdout stderr) $ do
      result <- exitCodeToLiquidResult exitCode
      if result == expectedResult then
        return $ testPassed ""
      else if result == Safe then
        return $ testFailed "" -- We don't get useful output when code is deemed
                               -- safe.
      else
        Nothing
    where
      liquid = proc "liquid" $
        [ toFilePath file
        , "--idirs=src"
        , "--ghc-option=-XNoImplicitPrelude"
        , "--reflection"
        , "--ple"
        ]
      failureString stdout stderr = T.unpack $ T.unlines $
        [ "STDOUT:"
        , T.decodeUtf8 stdout
        , ""
        , "STDERR:"
        , T.decodeUtf8 stderr
        ]

  testOptions = return []
  
