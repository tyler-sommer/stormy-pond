{-# LANGUAGE DeriveGeneric #-}
module Pond.Data
  ( Facet(..)
  , Ripple(..)
  ) where

import Data.Aeson
import Data.String
import GHC.Generics

data Facet =
  Facet
  { name :: String
  , group :: Maybe String
  } deriving (Generic, Show, Eq)

instance IsString Facet where
  fromString n =
    Facet
    { name = n
    , group = Nothing
    }

instance ToJSON Facet where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Facet

data Ripple =
  Ripple
  { summary :: String
  , description :: Maybe String
  , facets :: [Facet]
  } deriving (Generic, Show, Eq)

instance ToJSON Ripple where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Ripple