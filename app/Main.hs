{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.String
import Data.Time (getCurrentTime)
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import System.Process

import Pond.Data

import qualified Data.ByteString.Lazy.Char8 as LS
import qualified Data.Map as Map

data State = ParseAny | ParseTag | ParseDesc

-- | parseArgs parses the command-line arguments, creating a new 'Ripple'.
parseArgs :: [String] -> Ripple
parseArgs argv = parseArg ParseAny (Ripple "" Nothing []) argv

parseArg :: State -> Ripple -> [String] -> Ripple
parseArg ParseAny r ("-r":xs) = parseArg ParseTag r xs
parseArg ParseAny r (s:xs)    = parseArg ParseAny (r { summary = s }) xs
parseArg ParseTag r (s:xs)    = parseArg ParseAny (r { reflections = (reflections r) ++ [(fromString s :: Reflection)] }) xs
parseArg _ r _                = r

-- | parseEditorLines parses input received from an editor, creating a new 'Ripple'.
parseEditorLines :: [String] -> Ripple
parseEditorLines lns = parseEditorLine ParseAny (Ripple "" Nothing []) lns

parseEditorLine :: State -> Ripple -> [String] -> Ripple
parseEditorLine ParseAny r (s:xs)        = parseEditorLine ParseDesc (r { summary = s }) xs
parseEditorLine ParseDesc r ("":xs)      = parseEditorLine ParseDesc r xs
parseEditorLine ParseDesc r (("#"):xs)   = parseEditorLine ParseDesc r xs
parseEditorLine ParseDesc r (('#':_):xs) = parseEditorLine ParseDesc r xs
parseEditorLine ParseDesc r (('R':'e':'f':'l':'e':'c':'t':'i':'o':'n':':':s):xs) =
  parseEditorLine ParseTag r ([s] ++ xs)
parseEditorLine ParseDesc r (s:xs) =
  case (description r) of
    Just desc -> parseEditorLine ParseDesc (r { description = Just (desc ++ "\n" ++ s) }) xs
    _         -> parseEditorLine ParseDesc (r { description = Just s }) xs
parseEditorLine ParseTag r (s:xs) =
  parseEditorLine ParseDesc (r { reflections = (reflections r) ++ [(fromString s :: Reflection)] }) xs
parseEditorLine _ r _ = r

usageText :: String
usageText = "Usage: pond <command> [arg, ...]"

helpText :: String
helpText = unlines
  [ ""
  , "Commands:"
  , "\tlist\t\tList ripples"
  , "\tadd\t\tAdd a new ripple"
  , "\tadd [-r reflection [-r ...]] [summary]"
  , "\tedit\t\tEdit an existing ripple"
  , "\tedit <id>"
  , "\tshow\t\tPrint the contents of a ripple"
  , "\tshow <id>"
  , "\thelp\t\tShow help text"
  , ""
  , "pond is a utility for tracking and managing tasks and other lists. Each"
  , "pond contains a collection of items called ripples. Each ripple must have"
  , "a summary and may be tagged and grouped using labels called reflections."
  , ""
  , "Invoking the \"add\" command without arguments will open your"
  , "system editor with a git-style format for modifying ripples."
  ]

templateText :: Ripple -> String
templateText r = unlines
  [ (show r) ++
    "# Edit the ripple above. Summary is the first line, followed by a blank line"
  , "# then an optional long description. Add reflections with \"Reflection: reflection\"."
  , "# Lines starting with \"#\" are ignored."
  , "#"
  , "# Example:"
  , "# Clean the rain gutters"
  , "#"
  , "# The rain gutters at the house are getting pretty nasty. It's about time to"
  , "# clean them out."
  , "#"
  , "# Reflection: chore"
  , "# Reflection: weekend"
  ]

openEditor :: Ripple -> IO Ripple
openEditor ripple = do
  tmpdir <- getTemporaryDirectory
  (file, tmp) <- openTempFile tmpdir "ripple.txt"
  hPutStr tmp (templateText ripple)
  hFlush tmp
  hClose tmp
  (_, Just hout, _, _) <-
    createProcess (proc "bash" ["-c", ("$EDITOR " ++ file ++ " && cat " ++ file)]){ std_out = CreatePipe }
  contents <- hGetContents hout
  return $ parseEditorLines (lines contents)

quietGetContents :: FilePath -> IO String
quietGetContents f = catchIOError (readFile f) (\_ -> return $ "")

readIndex :: FilePath -> IO Pond
readIndex base = do
  contents <- quietGetContents (base ++ "/" ++ "index.json")
  return $ case decode (LS.pack contents) of
    Just p  -> p
    Nothing -> (Pond Nothing Nothing Map.empty)

readRipple :: FilePath -> FilePath -> IO Ripple
readRipple base sum = do
  contents <- quietGetContents (base ++ "/" ++ sum ++ ".json")
  return $ case decode (LS.pack contents) of
    Just r  -> r
    Nothing -> (Ripple "" Nothing [])

writeIndex :: Pond -> FilePath -> IO ()
writeIndex pond base = do
  let name = (base ++ "/" ++ "index.json")
  let tmpName = (name ++ ".tmp")
  writeFile tmpName (LS.unpack (encode pond))
  catchIOError (removeFile name) (\_ -> return $ ())
  renameFile tmpName name

writeRipple :: Ripple -> FilePath -> IO ()
writeRipple ripple base = do
  writeFile (base ++ "/" ++ ((checksum ripple) ++ ".json")) (LS.unpack (encode ripple))

printUsageAndExit :: IO ()
printUsageAndExit = do
  putStrLn usageText
  exitWith (ExitFailure 1)

printHelpAndExit :: IO ()
printHelpAndExit = do
  putStrLn usageText
  putStr helpText
  exitWith ExitSuccess

printShimmer :: Pond -> Shimmer -> IO ()
printShimmer p s = do
  putStrLn $ "ID: " ++ (rippleId s) ++ "\nDate: " ++ (show (date s))
  case nextM p s of
    Just sh -> do
      putStrLn ""
      printShimmer p sh
    Nothing -> return ()

main :: IO ()
main = do
  pondDir <- getHomeDirectory >>= (\home -> return $ home ++ "/.pond")
  _ <- createDirectoryIfMissing True pondDir
  pond <- readIndex pondDir
  getArgs >>= (\argv ->
    case argv of
      "add":xs -> do
        let s = parseArgs xs
        r <- case s of
          (Ripple "" _ _) -> openEditor s
          _               -> return $ s
        now <- getCurrentTime
        writeIndex (with r now pond) pondDir
        writeRipple r pondDir
      "edit":x:xs -> case searchM pond x of
        Just sh -> do
          now <- getCurrentTime
          r <- readRipple pondDir (rippleId sh) >>= openEditor
          writeIndex (replaceWith sh r now pond) pondDir
          writeRipple r pondDir
        _       -> exitWith (ExitFailure 1)
      "show":x:xs -> case searchM pond x of
        Just sh -> do
          r <- readRipple pondDir (rippleId sh)
          print r
        _       -> exitWith (ExitFailure 1)
      "list":xs -> do
        case centerM pond of
          Just s -> do
            printShimmer pond s
          _ -> exitWith (ExitFailure 1)
      ["help"] -> printHelpAndExit
      "help":_ -> printHelpAndExit
      unk:_    -> do
        putStrLn $ "unknown command: " ++ unk
        printUsageAndExit
      _ -> printUsageAndExit)
