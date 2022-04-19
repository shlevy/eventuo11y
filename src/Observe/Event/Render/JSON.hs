{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Observe.Event.Render.JSON where

import Control.Exception
import Data.Aeson
import Data.Aeson.Key
import Data.Typeable
import Observe.Event.Dynamic

type RenderFieldJSON field = field -> (Key, Value)

type RenderSelectorJSON sel = forall f . sel f -> (Key, RenderFieldJSON f)

renderDynamicFieldJSON :: RenderFieldJSON DynamicField
renderDynamicFieldJSON f = (fromText (name f), value f)

renderDynamicEventSelectorJSON :: RenderSelectorJSON DynamicEventSelector
renderDynamicEventSelectorJSON (MkDynamicEventSelector n) = (fromText n, renderDynamicFieldJSON)

type RenderExJSON stex = stex -> Value

data SomeJSONException = forall e . (Exception e, ToJSON e) => SomeJSONException e

instance Show SomeJSONException where
  show (SomeJSONException e) = show e
  showsPrec i (SomeJSONException e) = showsPrec i e

instance Exception SomeJSONException

jsonExceptionToException :: (Exception e, ToJSON e) => e -> SomeException
jsonExceptionToException = toException . SomeJSONException

jsonExceptionFromException :: (Exception e) => SomeException -> Maybe e
jsonExceptionFromException x = do
  SomeJSONException a <- fromException x
  cast a

renderJSONException :: RenderExJSON SomeJSONException
renderJSONException (SomeJSONException e) = toJSON e
