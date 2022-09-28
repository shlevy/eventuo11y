{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Observe.Event.Render.JSON where

import Control.Exception
import Data.Aeson
import Data.Aeson.Key
import Data.Typeable
import Observe.Event.Dynamic

-- | A function to render a given @field@ as JSON.
--
-- The 'Key' is a field name, the 'Value' is an arbitrary
-- rendering of the field value (if any).
type RenderFieldJSON field = field -> (Key, Value)

-- | A function to render a given selector, its fields, as JSON.
--
-- The 'Key' is the event name/category.
type RenderSelectorJSON sel = forall f. sel f -> (Key, RenderFieldJSON f)

-- | Render a 'DynamicField'
renderDynamicFieldJSON :: RenderFieldJSON DynamicField
renderDynamicFieldJSON f = (fromText (name f), value f)

-- | Render a 'DynamicEventSelector' and all its sub-fields.
renderDynamicEventSelectorJSON :: RenderSelectorJSON DynamicEventSelector
renderDynamicEventSelectorJSON (DynamicEventSelector n) =
  (fromText n, renderDynamicFieldJSON)

-- | A function to render a given structured exception to JSON.
type RenderExJSON stex = stex -> Value

-- | Render a 'SomeJSONException' to JSON.
--
-- It is __not__ necessary to use 'SomeJSONException' for the base of your
-- structured exceptions in a JSON backend, so long as you provide a
-- 'RenderExJSON' for your base exception type.
renderJSONException :: RenderExJSON SomeJSONException
renderJSONException (SomeJSONException e) = toJSON e

-- | A possible base type for structured exceptions renderable to JSON.
--
-- It is __not__ necessary to use 'SomeJSONException' for the base of your
-- structured exceptions in a JSON backend, so long as you provide a
-- 'RenderExJSON' for your base exception type.
data SomeJSONException = forall e. (Exception e, ToJSON e) => SomeJSONException e

instance Show SomeJSONException where
  show (SomeJSONException e) = show e
  showsPrec i (SomeJSONException e) = showsPrec i e

instance Exception SomeJSONException

-- | Used to create sub-classes of 'SomeJSONException'.
jsonExceptionToException :: (Exception e, ToJSON e) => e -> SomeException
jsonExceptionToException = toException . SomeJSONException

-- | Used to create sub-classes of 'SomeJSONException'.
jsonExceptionFromException :: (Exception e) => SomeException -> Maybe e
jsonExceptionFromException x = do
  SomeJSONException a <- fromException x
  cast a
