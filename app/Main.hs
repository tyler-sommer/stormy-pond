{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import System.Environment
import System.Exit

import Pond.Data

import qualified Data.ByteString.Lazy.Char8 as LS

data Command =
  AddCommand deriving (Show)

command :: String -> Either String Command
command "add" = Right AddCommand
command unk = Left ("unknown command \"" ++ unk ++ "\"")

printUsageAndExit :: IO Command
printUsageAndExit = do
  putStrLn "Usage: pond <command> [arg, ...]"
  exitWith (ExitFailure 1)

parseArgs :: [String] -> IO Command
parseArgs [] = printUsageAndExit
parseArgs argv =
  case command (head argv) of
    Right cmd -> return $ cmd
    Left err -> do
      putStrLn ("error: " ++ err)
      printUsageAndExit

main :: IO ()
main = do
  cmd <- getArgs >>= parseArgs
  print cmd
