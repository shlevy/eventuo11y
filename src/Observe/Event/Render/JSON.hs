{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Description : Renderers for serializing Events as JSON
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- Instrumentors will need to provide instances of 'RenderSelectorJSON'
-- and 'RenderFieldJSON' for their domain-specific types to use their
--  t'Observe.Event.Event's with JSON-consuming t'Observe.Event.EventBackend's.
module Observe.Event.Render.JSON
  ( RenderSelectorJSON,
    RenderFieldJSON,

    -- * Rendering structured exceptions
    RenderExJSON,

    -- ** SomeJSONException
    renderJSONException,
    SomeJSONException (..),
    jsonExceptionToException,
    jsonExceptionFromException,

    -- * Observe.Event.Dynamic support
    renderDynamicEventSelectorJSON,
    renderDynamicFieldJSON,
  )
where

import Control.Exception
import Data.Aeson
import Data.Aeson.Key
import Data.Typeable
import Observe.Event.Dynamic

-- | A function to render a given selector, its fields, as JSON.
--
-- The 'Key' is the event name/category.
type RenderSelectorJSON sel = forall f. sel f -> (Key, RenderFieldJSON f)

-- | A function to render a given @field@ as JSON.
--
-- The 'Key' is a field name, the 'Value' is an arbitrary
-- rendering of the field value (if any).
type RenderFieldJSON field = field -> (Key, Value)

-- | A function to render a given structured exception to JSON.
type RenderExJSON stex = stex -> Value

-- | Render a 'DynamicEventSelector' and all its sub-fields.
renderDynamicEventSelectorJSON :: RenderSelectorJSON DynamicEventSelector
renderDynamicEventSelectorJSON (DynamicEventSelector n) =
  (fromText n, renderDynamicFieldJSON)

-- | Render a 'DynamicField'
renderDynamicFieldJSON :: RenderFieldJSON DynamicField
renderDynamicFieldJSON f = (fromText (name f), value f)

-- | Render a 'SomeJSONException' to JSON.
--
-- It is __not__ necessary to use 'SomeJSONException' for the base of your
-- structured exceptions in a JSON backend, so long as you provide a
-- 'RenderExJSON' for your base exception type.
renderJSONException :: RenderExJSON SomeJSONException
renderJSONException (SomeJSONException render e) = render e

-- | A possible base type for structured exceptions renderable to JSON.
--
-- It is __not__ necessary to use 'SomeJSONException' for the base of your
-- structured exceptions in a JSON backend, so long as you provide a
-- 'RenderExJSON' for your base exception type.
data SomeJSONException
  = forall e. Exception e => SomeJSONException (RenderExJSON e) e

instance Show SomeJSONException where
  show (SomeJSONException _ e) = show e
  showsPrec i (SomeJSONException _ e) = showsPrec i e

instance Exception SomeJSONException

-- | Used to create sub-classes of 'SomeJSONException'.
jsonExceptionToException :: (Exception e) => RenderExJSON e -> e -> SomeException
jsonExceptionToException render = toException . SomeJSONException render

-- | Used to create sub-classes of 'SomeJSONException'.
jsonExceptionFromException :: (Exception e) => SomeException -> Maybe e
jsonExceptionFromException x = do
  SomeJSONException _ a <- fromException x
  cast a
