{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Description : EventBackend for rendering events as JSON to a handle
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
module Observe.Event.Render.JSON.Handle
  ( jsonHandleBackend,
    simpleJsonStderrBackend,

    -- * Internals
    JSONRef (..),
    newJSONEvent,
  )
where

import Control.Concurrent.MVar
import Control.Exception
import Data.Aeson
import Data.Aeson.KeyMap (fromList, insert)
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Data.Coerce
import Data.IORef
import Data.Time.Clock
import Data.UUID (UUID)
import Data.UUID.V4
import Observe.Event
import Observe.Event.Backend
import Observe.Event.Render.JSON
import System.IO (Handle, stderr)

-- | The reference type for 'EventBackend's generated by 'jsonHandleBackend'.
--
-- Only expected to be used by type inference or by code implementing other backends
-- using this one.
newtype JSONRef = JSONRef UUID deriving newtype (ToJSON)

-- | Create a new 'Event' in a 'jsonHandleBackend'.
newJSONEvent ::
  (Exception stex) =>
  -- | Emit the final 'Object'. This will be called at most once.
  (Object -> IO ()) ->
  RenderExJSON stex ->
  RenderFieldJSON f ->
  -- | Parent
  Maybe JSONRef ->
  -- | Causes
  [JSONRef] ->
  -- | Initial fields
  [f] ->
  IO (Event IO JSONRef f)
newJSONEvent emit renderEx renderField parent causes initialFields = do
  eventRef <- coerce nextRandom
  start <- getCurrentTime
  fieldsRef <- newIORef . fromList $ map renderField initialFields
  finishOnce <- newEmptyMVar
  let finish r =
        tryPutMVar finishOnce () >>= \case
          False -> pure ()
          True -> do
            end <- getCurrentTime
            fields <- readIORef fieldsRef
            emit
              ( "event-id" .= eventRef
                  <> "start" .= start
                  <> "end" .= end
                  <> "duration" .= diffUTCTime end start
                  <> ifNotNull "fields" fields
                  <> ifNotNull "parent" parent
                  <> ifNotNull "proximate-causes" causes
                  <> case r of
                    StructuredFail e -> "structured-exception" .= renderEx e
                    UnstructuredFail e -> "unstructured-exception" .= show e
                    Finalized -> mempty
              )
  pure $
    Event
      { reference = eventRef,
        addField = \field ->
          atomicModifyIORef' fieldsRef \fields ->
            (uncurry insert (renderField field) fields, ()),
        finalize = \me -> finish $ case me of
          Just e -> case fromException e of
            Just se -> StructuredFail se
            Nothing -> UnstructuredFail e
          Nothing -> Finalized
      }

-- | An 'EventBackend' which posts events to a given 'Handle' as JSON.
--
-- Each 'Event' is posted as a single line, as it's completed. As a result, child events
-- will typically be posted __before__ their parents (though still possible to correlate via
-- event IDs).
--
-- The 'EventBackend' must be the exclusive writer to the 'Handle' while any events are live,
-- but it does not 'System.IO.hClose' it itself.
jsonHandleBackend ::
  -- The type of structured exceptions
  (Exception stex) =>
  -- | Where to write 'Event's.
  Handle ->
  -- | Render a structured exception to JSON
  RenderExJSON stex ->
  -- | Render a selector, and the fields of 'Event's selected by it, to JSON
  RenderSelectorJSON s ->
  IO (EventBackend IO JSONRef s)
jsonHandleBackend h renderEx renderSel = do
  outputLock <- newMVar ()
  let emit :: Object -> IO ()
      emit o = withMVar outputLock \() ->
        hPutStrLn h $ encode o
      eb =
        EventBackend
          { newEvent = \(NewEventArgs {..}) -> do
              let (k, renderField) = renderSel newEventSelector
              newJSONEvent (emit . (k .=)) renderEx renderField newEventParent newEventCauses newEventInitialFields,
            emitImmediateEvent = fmap reference . newEvent eb
          }
  pure eb

-- | An 'EventBackend' which posts events to @stderr@ as JSON.
--
-- Each 'Event' is posted as a single line, as it's completed. As a result, child events
-- will typically be posted __before__ their parents (though still possible to correlate via
-- event IDs).
--
-- Any instrumented 'Exception's descended from 'SomeJSONException' will be structurally rendered.
--
-- The 'EventBackend' must be the exclusive writer to @stderr@ while any events are live,
-- but it does not 'System.IO.hClose' it itself.
simpleJsonStderrBackend :: RenderSelectorJSON s -> IO (EventBackend IO JSONRef s)
simpleJsonStderrBackend = jsonHandleBackend stderr (toJSON @SomeJSONException)

-- | Why did an 'Event' finish?
data FinishReason stex
  = StructuredFail stex
  | UnstructuredFail SomeException
  | Finalized

-- | Add k .= v if v is not 'null'.
ifNotNull :: (Foldable f, ToJSON (f v)) => Key -> f v -> Object
ifNotNull k v = if null v then mempty else k .= v
