{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.Aeson
import Data.String
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

import Pond.Data

type State = (Either String (Ripple -> String -> Ripple), Ripple)

parseTagArg :: Ripple -> String -> Ripple
parseTagArg r arg = do
  let facet = fromString arg :: Facet
  r { facets = (facets r) ++ [facet] }

parseShortArg :: Ripple -> String -> Ripple
parseShortArg r arg = r { summary = arg }

configureCommand :: State -> String -> State
configureCommand (Left desc, c) arg = do
  case arg of
    "-t" -> (Right parseTagArg, c)
    otherwise -> (Left "short", parseShortArg c arg)
configureCommand (Right fn, c) arg =
  (Left "reset", fn c arg)

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
  hGetContents hout >>= (parseEditor ripple)

parseEditor :: Ripple -> String -> IO Ripple
parseEditor ripple input = return $ snd (foldl (parseEditorRipple) (Right parseEditorSummary, ripple) (lines input))

type EditorParser = Ripple -> String -> Maybe Ripple
type EditorState = (Either String EditorParser, Ripple)

parseEditorSummary :: Ripple -> String -> Maybe Ripple
parseEditorSummary ripple line =
  case line of
    '#':_ -> Nothing
    s -> Just (ripple { summary = s })

parseEditorNoOp :: Ripple -> String -> Maybe Ripple
parseEditorNoOp ripple _ = Nothing

nextEditorParser :: EditorParser -> EditorParser
nextEditorParser curr = parseEditorNoOp

parseEditorRipple :: EditorState -> String -> EditorState
parseEditorRipple state line = do
  case state of
    (Left desc, ripple) -> (Left desc, ripple)
    (Right fn, ripple) -> do
      case fn ripple line of
        Just r -> (Right (nextEditorParser fn), r)
        otherwise -> (Right fn, ripple)

printUsageAndExit :: IO Ripple
printUsageAndExit = do
  putStr usageText
  exitWith (ExitFailure 1)

parseArgs :: [String] -> IO Ripple
parseArgs [] = printUsageAndExit
parseArgs argv =
  case head argv of
    "add" -> do
      let s = foldl configureCommand (Left "start", Ripple "" Nothing []) (tail argv)
      case s of
        (Left desc, ripple) -> do
          if summary ripple == ""
            then openEditor ripple
            else
              return $ ripple
        (Right _, _) -> do
          putStrLn "expected value, got nothing"
          printUsageAndExit
    unk -> do
      putStrLn ("unknown command: " ++ unk)
      printUsageAndExit

main :: IO ()
main = do
  cmd <- getArgs >>= parseArgs
  print cmd
