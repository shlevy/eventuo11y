{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Description : "Dynamically typed" Event selectors and fields.
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- Instrumentors can use the types in this module if they don't want
-- to define domain-specific types for the code they're instrumenting.
module Observe.Event.Dynamic
  ( DynamicEventSelector (..),
    DynamicField (..),

    -- * Shorthand types
    DynamicEventBackend,
    DynamicEvent,
  )
where

import Data.Aeson
import Data.String
import Data.Text
import Observe.Event
import Observe.Event.Syntax

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

-- | A simple type usable as an 'Event' field type.
--
-- Individual 'DynamicField's are typically constructed via
-- the 'RecordField' instance, using 'ToJSON' for the value,
-- e.g. @addField ev $ "foo" ≔ x@ will add @DynamicField "foo" (toJSON x)@
-- as a field to @ev@.
data DynamicField = DynamicField
  { name :: !Text,
    value :: !Value
  }

instance (ToJSON a) => RecordField Text a DynamicField where
  k ≔ v = DynamicField k $ toJSON v

-- | Shorthand for an 'EventBackend' using 'DynamicEventSelector's.
type DynamicEventBackend m r = EventBackend m r DynamicEventSelector

-- | Shorthand for an 'Event' using 'DynamicField's.
type DynamicEvent m r = Event m r DynamicEventSelector DynamicField
