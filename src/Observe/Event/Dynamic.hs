{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Observe.Event.Dynamic where

import Data.Aeson
import Data.String
import Data.Text

import Observe.Event

data DynamicField = MkDynamicField
  { name :: !Text
  , value :: !Value
  }

instance (ToJSON x) => IsString (x -> DynamicField) where
  fromString s = MkDynamicField (fromString s) . toJSON

data DynamicEventSelector f where
  MkDynamicEventSelector :: !Text -> DynamicEventSelector DynamicField

instance (f ~ DynamicField) => IsString (DynamicEventSelector f) where
  fromString = MkDynamicEventSelector . fromString

type DynamicEventBackend m r = EventBackend m r DynamicEventSelector

type DynamicEvent m r = Event m r DynamicField
