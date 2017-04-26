{-|
Module      : Pond.Data
Description : Data facilities for pond
Copyright   : (c) 2017 Tyler Sommer
License     : MIT
Maintainer  : sommertm@gmail.com
Stability   : experimental
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Pond.Data
  ( Reflection(..)
  , Ripple(..)
  , Pond(..)
  , Shimmer(..)

  , checksum

  -- Modifying Ponds
  , with

  -- Navigating Shimmers
  , centerM
  , outerM
  , nextM
  ) where

import Data.Aeson
import Data.String
import Data.Map (Map)
import Data.Time (UTCTime)
import GHC.Generics

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LS
import qualified Data.Map as Map

-- | A 'Reflection' is a label describing a 'Ripple'. Reflections can be used to
-- group and filter ripples.
data Reflection =
  Reflection
  { name :: String
  , group :: Maybe String
  } deriving (Generic, Eq)

instance IsString Reflection where
  fromString n =
    Reflection
    { name = n
    , group = Nothing
    }

instance Show Reflection where
  show f = "Reflection: " ++ (name f)

instance ToJSON Reflection where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Reflection

-- | A 'Ripple' is a task or other list item.
data Ripple =
  Ripple
  { summary :: String
  , description :: Maybe String
  , reflections :: [Reflection]
  } deriving (Generic)

type RippleID = String

-- | 'checksum' calculates the SHA256 checksum of a 'Ripple'
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
      (foldl (\i -> \f -> i ++ "\n" ++ (show f)) "" (reflections r))

instance Eq Ripple where
  r1 == r2 = (checksum r1) == (checksum r2)

instance ToJSON Ripple where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Ripple

-- | A 'Shimmer' contains metadata about a specific 'Ripple'.
data Shimmer =
  Shimmer
  { rippleId :: RippleID
  , next :: Maybe RippleID
  , date :: UTCTime
  } deriving (Generic, Show, Eq)

instance ToJSON Shimmer where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Shimmer

-- | A 'Pond' is a central index of Ripples.
data Pond =
  Pond
  { center :: Maybe RippleID
  , outer :: Maybe RippleID
  , ripples :: Map RippleID Shimmer
  } deriving (Generic, Show, Eq)

instance ToJSON Pond where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Pond

-- | 'centerM' returns the center-most 'Shimmer' or Nothing.
centerM :: Pond -> Maybe Shimmer
centerM pond = do
  id <- center pond
  Map.lookup id (ripples pond)

-- | 'nextM' returns the next 'Shimmer' or Nothing.
nextM :: Pond -> Shimmer -> Maybe Shimmer
nextM pond s = do
  id <- next s
  Map.lookup id (ripples pond)

-- | 'outerM' returns the outer-most 'Shimmer' or Nothing.
outerM :: Pond -> Maybe Shimmer
outerM pond = do
  id <- outer pond
  Map.lookup id (ripples pond)

-- | 'with' returns a copy of the given 'Pond' with the given 'Ripple' added.
with :: Ripple -> UTCTime -> Pond -> Pond
with r t p =
  case po of
    Just res -> res
    Nothing  -> p
  where
    sum = checksum r
    c = case center p of
      Just ce -> ce
      Nothing -> sum
    osum = case outer p of
      Just oe -> oe
      Nothing -> sum
    po = do
      sh <- Map.lookup osum (ripples p)
      po <- return $ p { ripples = Map.insert osum (sh { next = Just sum }) (ripples p) }
      sh <- return $ Shimmer { rippleId = sum, next = Nothing, date = t }
      return $ po { ripples = Map.insert sum sh (ripples po), center = Just c, outer = Just sum }
