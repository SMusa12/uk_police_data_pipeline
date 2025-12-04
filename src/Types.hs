{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics (Generic)

data Crime = Crime
  { category :: String
  , location :: String
  } deriving (Show, Generic)
