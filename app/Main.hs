{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import System.Environment
import System.Exit

import Pond.Data

import qualified Data.ByteString.Lazy.Char8 as LS

data Command =
  Command
  { name :: String
  , args :: [String]
  } deriving (Show)

parseArgs :: [String] -> IO Command
parseArgs [] = do
  putStrLn "Usage: pond <command> [arg, ...]"
  exitWith (ExitFailure 1)
parseArgs argv = return $ Command { name = head argv, args = tail argv }

main :: IO ()
main = do
  command <- getArgs >>= parseArgs
  print command
  let facet = "interesting" :: Facet
  let ripple = Ripple { summary = "do a thing", description = Nothing, facets = [facet] }
  LS.putStrLn (encode ripple)
