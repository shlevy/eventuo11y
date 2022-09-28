{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Observe.Event.Dynamic where

import Data.Aeson
import Data.String
import Data.Text
import Observe.Event

data DynamicField = DynamicField
  { name :: !Text,
    value :: !Value
  }

instance (ToJSON x) => IsString (x -> DynamicField) where
  fromString s = DynamicField (fromString s) . toJSON

data DynamicEventSelector f where
  DynamicEventSelector :: !Text -> DynamicEventSelector DynamicField

instance (f ~ DynamicField) => IsString (DynamicEventSelector f) where
  fromString = DynamicEventSelector . fromString

type DynamicEventBackend m r = EventBackend m r DynamicEventSelector

type DynamicEvent m r = Event m r DynamicEventSelector DynamicField
