{-# LANGUAGE DeriveGeneric #-}
module Pond.Data
  ( Facet
  , Ripple(..)
  ) where

import Data.Aeson
import GHC.Generics

type Facet = String

data Ripple =
  Ripple
  { summary :: String
  , description :: Maybe String
  , facets :: [Facet]
  } deriving (Generic, Show, Eq)

instance ToJSON Ripple where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Ripple