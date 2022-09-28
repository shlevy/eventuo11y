{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Observe.Event.Dynamic where

import Data.Aeson
import Data.String
import Data.Text
import Observe.Event

-- | A simple type usable as an 'Event' field type.
--
-- Individual 'DynamicField's are typically constructed via
-- the 'IsString' instance, using 'ToJSON' for the value,
-- e.g. @addField ev $ "foo" x@ will add @DynamicField "foo" (toJSON x)@
-- as a field to @ev@.
data DynamicField = DynamicField
  { name :: !Text,
    value :: !Value
  }

-- | Treat a string as a function to a 'DynamicField', calling
-- 'toJSON' on its argument.
instance (ToJSON x) => IsString (x -> DynamicField) where
  fromString s = DynamicField (fromString s) . toJSON

-- | A simple type usable as an 'EventBackend' selector.
--
-- All 'Event's have 'DynamicField' field types.
--
-- Individual 'DynamicEventSelector's are typically constructed
-- via the 'IsString' instance, e.g. @withEvent backend "foo" go@
-- will call @go@ with an @Event m r DynamicEventSelector DynamicField@
-- named "foo".
data DynamicEventSelector f where
  DynamicEventSelector :: !Text -> DynamicEventSelector DynamicField

instance (f ~ DynamicField) => IsString (DynamicEventSelector f) where
  fromString = DynamicEventSelector . fromString

-- | Shorthand for an 'EventBackend' using 'DynamicEventSelector's.
type DynamicEventBackend m r = EventBackend m r DynamicEventSelector

-- | Shorthand for an 'Event' using 'DynamicField's.
type DynamicEvent m r = Event m r DynamicEventSelector DynamicField
