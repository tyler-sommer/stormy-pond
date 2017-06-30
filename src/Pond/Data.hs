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
  , replaceWith

  -- Navigating Shimmers
  , centerM
  , outerM
  , nextM
  , searchM
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
  , prev :: Maybe RippleID
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

-- | prevM returns the previous 'Shimmer' or Nothing.
prevM :: Pond -> Shimmer -> Maybe Shimmer
prevM pond s = do
  id <- prev s
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

-- | 'searchM' returns the specified 'Shimmer' or Nothing.
searchM :: Pond -> RippleID -> Maybe Shimmer
searchM pond id =
  Map.lookup id (ripples pond)

-- | 'replaceWith' returns a copy of 'Pond' with the given 'Shimmer'
-- replaced with the given 'Ripple'.
replaceWith :: Shimmer -> Ripple -> UTCTime -> Pond -> Pond
replaceWith fs to t p =
  pp { ripples = Map.insert tsum sh (ripples pp), center = cn, outer = ou }
  where
    tsum = checksum to
    fsum = rippleId fs
    pn = case nextM p fs of
      Just ns -> p { ripples = Map.insert (rippleId ns) (ns { prev = Just tsum }) (ripples p) }
      Nothing -> p
    pp = case prevM pn fs of
      Just ps -> pn { ripples = Map.insert (rippleId ps) (ps { next = Just tsum }) (ripples pn) }
      Nothing -> pn
    sh = Shimmer { rippleId = tsum, prev = prev fs, next = next fs, date = t }
    ou = if outer pp == Just fsum
      then Just tsum
      else outer pp
    cn = if center pp == Just fsum
      then Just tsum
      else center pp

-- | 'with' returns a copy of the given 'Pond' with the given 'Ripple' added.
with :: Ripple -> UTCTime -> Pond -> Pond
with r t p =
  pn { ripples = Map.insert sum sh (ripples pn), center = Just c, outer = Just sum }
  where
    sum = checksum r
    c = case center p of
      Just ce -> ce
      Nothing -> sum
    pn = case outerM p of
      Just os -> p { ripples = Map.insert (rippleId os) (os { next = Just sum }) (ripples p) }
      Nothing -> p
    sh = Shimmer { rippleId = sum, prev = outer pn, next = Nothing, date = t }
