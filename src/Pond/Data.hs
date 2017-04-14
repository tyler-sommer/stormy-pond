{-|
Module      : Pond.Data
Description : Data facilities for pond
Copyright   : (c) 2017 Tyler Sommer
License     : MIT
Maintainer  : sommertm@gmail.com
Stability   : experimental
-}
{-# LANGUAGE DeriveGeneric #-}
module Pond.Data
  ( Facet(..)
  , Ripple(..)
  ) where

import Data.Aeson
import Data.String
import GHC.Generics

-- | A facet is a label describing a ripple. Facets can be used to group and
-- filter ripples.
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

-- | A ripple is a task or other list item.
data Ripple =
  Ripple
  { summary :: String
  , description :: Maybe String
  , facets :: [Facet]
  } deriving (Generic, Show, Eq)

instance ToJSON Ripple where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Ripple