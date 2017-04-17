{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.String
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
    _         -> parseEditorLine ParseDesc (r { description = Just s }) xs
parseEditorLine ParseTag r (s:xs) =
  parseEditorLine ParseDesc (r { facets = (facets r) ++ [(fromString s :: Facet)] }) xs
parseEditorLine _ r _ = r

parseEditorLines :: [String] -> Ripple
parseEditorLines lns = parseEditorLine ParseAny (Ripple "" Nothing []) lns

usageText :: String
usageText = "Usage: pond <command> [arg, ...]"

helpText :: String
helpText = unlines
  [ ""
  , "Commands:"
  , "\tadd\t\tAdd a new ripple"
  , "\tadd [-t facet [-t facet]] [summary]"
  , "\thelp\t\tShow help text"
  , ""
  , "pond is a utility for tracking and managing tasks and other lists. Each"
  , "pond contains a collection of items called ripples. Each ripple must have"
  , "a summary and may be tagged and grouped using labels called facets."
  , ""
  , "Invoking the \"add\" or \"edit\" commands without arguments will open your"
  , "system editor with a git-style format for modifying ripples."
  ]

templateText :: Ripple -> String
templateText r = unlines
  [ (show r) ++
    "# Edit the ripple above. Summary is the first line, followed by a blank line"
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

readIndex :: FilePath -> IO Pond
readIndex base = do
  handle   <- openFile (base ++ "/" ++ "index.json") ReadMode
  contents <- hGetContents handle
  case decode (LS.pack contents) of
    Just p -> return $ p
    _      -> return $ (Pond [])

readRipple :: FilePath -> FilePath -> IO Ripple
readRipple base csum = do
  handle   <- openFile (base ++ "/" ++ (csum ++ ".json")) ReadMode
  contents <- hGetContents handle
  case decode (LS.pack contents) of
    Just r -> return $ r
    _      -> return $ (Ripple "" Nothing [])

writeIndex :: Pond -> FilePath -> IO ()
writeIndex pond base = do
  writeFile (base ++ "/" ++ "index.json") (LS.unpack (encode pond))

writeRipple :: Ripple -> FilePath -> IO ()
writeRipple ripple base = do
  writeFile (base ++ "/" ++ ((checksum ripple) ++ ".json")) (LS.unpack (encode ripple))

printUsageAndExit :: IO Ripple
printUsageAndExit = do
  putStrLn usageText
  exitWith (ExitFailure 1)

printHelpAndExit :: IO Ripple
printHelpAndExit = do
  putStrLn usageText
  putStr helpText
  exitWith ExitSuccess

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
      ["help"] -> printHelpAndExit
      "help":_ -> printHelpAndExit
      unk:_    -> do
        putStrLn ("unknown command: " ++ unk)
        printUsageAndExit
      _ -> printUsageAndExit)
  print r
  home <- getHomeDirectory
  let pondDir = home ++ "/" ++ ".pond"
  _ <- createDirectoryIfMissing True pondDir
  LS.putStrLn (encode r)
  writeRipple r pondDir
