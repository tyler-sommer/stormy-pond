module Pond.Data
  ( Facet
  , Ripple(..)
  ) where

type Facet = String

data Ripple =
  Ripple
  { summary :: String
  , description :: Maybe String
  , facets :: [Facet]
  } deriving Show
