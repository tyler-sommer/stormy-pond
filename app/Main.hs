module Main where

import Pond.Data

main :: IO ()
main = do
  let facet = "interesting" :: Facet
  let ripple = Ripple { summary = "do a thing", description = Nothing, facets = [facet] }
  print ripple
