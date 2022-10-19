{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Description : Renderers for serializing Events as JSON
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- Rendering types for JSON-consuming 'Observe.Event.EventBackend's.
--
-- Instances of 'RenderSelectorJSON' and 'RenderFieldJSON' can be generated
-- by "Observe.Event.Render.JSON.DSL.Compile".
module Observe.Event.Render.JSON
  ( RenderSelectorJSON,
    RenderFieldJSON,

    -- * Default renderers
    DefaultRenderSelectorJSON (..),
    DefaultRenderFieldJSON (..),

    -- * Rendering structured exceptions
    RenderExJSON,

    -- ** SomeJSONException
    SomeJSONException (..),
    jsonExceptionToException,
    jsonExceptionFromException,
  )
where

import Control.Exception
import Data.Aeson
import Data.Aeson.Key
import Data.Typeable
import Data.Void
import Observe.Event.Dynamic

-- | A function to render a given selector, and its fields, as JSON.
--
-- The 'Key' is the event name/category.
type RenderSelectorJSON sel = forall f. sel f -> (Key, RenderFieldJSON f)

-- | A function to render a given @field@ as JSON.
--
-- The 'Key' is a field name, the 'Value' is an arbitrary
-- rendering of the field value (if any).
type RenderFieldJSON field = field -> (Key, Value)

-- | A default 'RenderSelectorJSON', useful for auto-generation and simple
-- backend invocation.
class DefaultRenderSelectorJSON sel where
  defaultRenderSelectorJSON :: RenderSelectorJSON sel

-- | A default 'RenderFieldJSON', useful for auto-generation and simple
-- backend invocation.
class DefaultRenderFieldJSON field where
  defaultRenderFieldJSON :: RenderFieldJSON field

instance DefaultRenderFieldJSON Void where
  defaultRenderFieldJSON = absurd

instance DefaultRenderSelectorJSON DynamicEventSelector where
  defaultRenderSelectorJSON (DynamicEventSelector n) =
    (fromText n, defaultRenderFieldJSON)

instance DefaultRenderFieldJSON DynamicField where
  defaultRenderFieldJSON (DynamicField {..}) = (fromText name, value)

-- | A function to render a given structured exception to JSON.
type RenderExJSON stex = stex -> Value

-- | A possible base type for structured exceptions renderable to JSON.
--
-- It is __not__ necessary to use 'SomeJSONException' for the base of your
-- structured exceptions in a JSON backend, so long as you provide a
-- 'RenderExJSON' for your base exception type (or use 'ToJSON'-based rendering).
data SomeJSONException = forall e. (Exception e, ToJSON e) => SomeJSONException e

instance Show SomeJSONException where
  showsPrec i (SomeJSONException e) = showsPrec i e

instance ToJSON SomeJSONException where
  toJSON (SomeJSONException e) = toJSON e
  toEncoding (SomeJSONException e) = toEncoding e

instance Exception SomeJSONException

-- | Used to create sub-classes of 'SomeJSONException'.
jsonExceptionToException :: (Exception e, ToJSON e) => e -> SomeException
jsonExceptionToException = toException . SomeJSONException

-- | Used to create sub-classes of 'SomeJSONException'.
jsonExceptionFromException :: (Exception e) => SomeException -> Maybe e
jsonExceptionFromException x = do
  SomeJSONException a <- fromException x
  cast a
