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
  , Pond(..)

  , checksum
  ) where

import Data.Aeson
import Data.String
import GHC.Generics

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LS

-- | A facet is a label describing a ripple. Facets can be used to group and
-- filter ripples.
data Facet =
  Facet
  { name :: String
  , group :: Maybe String
  } deriving (Generic, Eq)

instance IsString Facet where
  fromString n =
    Facet
    { name = n
    , group = Nothing
    }

instance Show Facet where
  show f = "Facet: " ++ (name f)

instance ToJSON Facet where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Facet

-- | A ripple is a task or other list item.
data Ripple =
  Ripple
  { summary :: String
  , description :: Maybe String
  , facets :: [Facet]
  } deriving (Generic)

type RippleID = String

-- | Return the SHA256 checksum of a ripple.
checksum :: Ripple -> RippleID
checksum r =
  ByteString.unpack (Base16.encode (SHA256.hash (LS.toStrict (encode r))))

instance Show Ripple where
  show r = do
    (summary r) ++
      "\n" ++
      (case description r of
        Just desc -> "\n" ++ desc ++ "\n"
        _ -> "") ++
      (foldl (\i -> \f -> i ++ "\n" ++ (show f)) "" (facets r))

instance Eq Ripple where
  r1 == r2 = (checksum r1) == (checksum r2)

instance ToJSON Ripple where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Ripple

data Pond =
  Pond
  { ripples :: [RippleID]
  } deriving (Generic, Show, Eq)

instance ToJSON Pond where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Pond