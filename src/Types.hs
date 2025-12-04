{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Crime(..)
  , Location(..)
  , Street(..)
  ) where

import GHC.Generics        (Generic)
import Data.Aeson
  ( FromJSON(..)
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  )
import Data.Text           (Text)


data Crime = Crime
    { crimeId :: Maybe T.Text
    , crimeCategory :: T.Text
    , crimeMonth :: T.Text
    , crimeLatitude :: Double
    , crimeLongitude :: Double
    , crimeStreetName :: Maybe T.Text
    , crimeOutcomeStatus :: Maybe T.Text
    , crimeOutcomeDate :: Maybe T.Text
    } deriving (Show)

instance FromJSON Crime where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = crimeFieldModifier }

crimeFieldModifier :: String -> String
crimeFieldModifier "crimeId" = "id"
crimeFieldModifier other     = other

-- | Location info for a crime.
data Location = Location
  { latitude  :: Text
  , longitude :: Text
  , street    :: Street
  } deriving (Show, Generic)

instance FromJSON Location

-- | Street info (shared between many crimes).
data Street = Street
  { streetId :: Int
  , name     :: Text
  } deriving (Show, Generic)

instance FromJSON Street where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = streetFieldModifier }

streetFieldModifier :: String -> String
streetFieldModifier "streetId" = "id"
streetFieldModifier other      = other
