module Main where

import Data.Aeson

import Pond.Data

import qualified Data.ByteString.Lazy.Char8 as LS

main :: IO ()
main = do
  let facet = "interesting" :: Facet
  let ripple = Ripple { summary = "do a thing", description = Nothing, facets = [facet] }
  LS.putStrLn (encode ripple)
