{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.String
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

import Pond.Data

import qualified Data.ByteString.Lazy.Char8 as LS

data State = ParseAny | ParseTag | ParseDesc

parseArg :: State -> Ripple -> [String] -> Ripple
parseArg ParseAny r ("-t":xs) = parseArg ParseTag r xs
parseArg ParseAny r (s:xs)    = parseArg ParseAny (r { summary = s }) xs
parseArg ParseTag r (s:xs)    = parseArg ParseAny (r { facets = (facets r) ++ [(fromString s :: Facet)] }) xs
parseArg _ r _                = r

parseArgs :: [String] -> Ripple
parseArgs argv = parseArg ParseAny (Ripple "" Nothing []) argv

parseEditorLine :: State -> Ripple -> [String] -> Ripple
parseEditorLine ParseAny r (s:xs)        = parseEditorLine ParseDesc (r { summary = s }) xs
parseEditorLine ParseDesc r ("":xs)      = parseEditorLine ParseDesc r xs
parseEditorLine ParseDesc r (("#"):xs)   = parseEditorLine ParseDesc r xs
parseEditorLine ParseDesc r (('#':_):xs) = parseEditorLine ParseDesc r xs
parseEditorLine ParseDesc r (('F':'a':'c':'e':'t':':':' ':s):xs) =
  parseEditorLine ParseTag r ([s] ++ xs)
parseEditorLine ParseDesc r (s:xs) =
  case (description r) of
    Just desc -> parseEditorLine ParseDesc (r { description = Just (desc ++ "\n" ++ s) }) xs
    _         -> parseEditorLine ParseDesc (r { description = (Just s) }) xs

parseEditorLine ParseTag r (s:xs) =
  parseEditorLine ParseDesc (r { facets = (facets r) ++ [(fromString s :: Facet)] }) xs
parseEditorLine _ r _ = r

parseEditorLines :: [String] -> Ripple
parseEditorLines lines = parseEditorLine ParseAny (Ripple "" Nothing []) lines

usageText :: String
usageText = unlines
  [ "Usage: pond <command> [arg, ...]"
  , ""
  , "Commands:"
  , "\tadd\t\tAdd a new ripple"
  , "\tadd [-t facet [-t facet]] [summary]"
  ]

templateText :: Ripple -> String
templateText ripple = unlines
  [ "# Edit the ripple above. Summary is the first line, followed by a blank line"
  , "# then an optional long description. Add facets with \"Facet: facet\"."
  , "# Lines starting with \"#\" are ignored."
  , "#"
  , "# Example:"
  , "# Clean the rain gutters"
  , "#"
  , "# The rain gutters at the house are getting pretty nasty. It's about time to"
  , "# clean them out."
  , "#"
  , "# Facet: chore"
  , "# Facet: weekend"
  ]

openEditor :: Ripple -> IO Ripple
openEditor ripple = do
  tmpdir <- getTemporaryDirectory
  (file, tmp) <- openTempFile tmpdir "ripple.txt"
  hPutStr tmp (summary ripple)
  case description ripple of
    Just desc -> hPutStrLn tmp (unlines ["", "", desc])
    _ -> hPutStrLn tmp ""
  hPutStr tmp (templateText ripple)
  hFlush tmp
  hClose tmp
  (_, Just hout, _, _) <-
    createProcess (proc "bash" ["-c", ("$EDITOR " ++ file ++ " && cat " ++ file)]){ std_out = CreatePipe }
  contents <- hGetContents hout
  return $ parseEditorLines (lines contents)

printUsageAndExit :: IO Ripple
printUsageAndExit = do
  putStr usageText
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  r <- getArgs >>= (\argv ->
    case argv of
      "add":xs -> do
        let s = parseArgs xs
        case s of
          (Ripple "" _ _) -> do
            openEditor s
          _ -> do
            return $ s
      unk:s -> do
        putStrLn ("unknown command: " ++ unk)
        printUsageAndExit
      _ -> printUsageAndExit)
  print r
  LS.putStrLn (encode r)
