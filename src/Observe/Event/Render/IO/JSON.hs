{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Description : EventBackend for rendering events as JSON to a handle
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
module Observe.Event.Render.IO.JSON
  ( jsonHandleBackend,
    simpleJsonStderrBackend,

    -- * Internals
    JSONRef (..),
  )
where

import Control.Concurrent.MVar
import Control.Exception
import Data.Aeson
import Data.Aeson.KeyMap (insert)
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Data.Coerce
import Data.IORef
import Data.Time.Clock
import Data.UUID (UUID)
import Data.UUID.V4
import Observe.Event
import Observe.Event.Implementation
import Observe.Event.Render.JSON
import System.IO (Handle, stderr)

-- | The reference type for 'EventBackend's generated by 'jsonHandleBackend'.
--
-- Only expected to be used by type inference or by code implementing other backends
-- using this one.
newtype JSONRef = JSONRef UUID deriving newtype (ToJSON)

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
  pure $
    EventBackend
      { newEventImpl = \sel -> do
          let (k, renderField) = renderSel sel
          eventRef <- coerce nextRandom
          start <- getCurrentTime
          fieldsRef <- newIORef mempty
          parentsRef <- newIORef mempty
          proximatesRef <- newIORef mempty
          let finish r = do
                end <- getCurrentTime
                fields <- readIORef fieldsRef
                parents <- readIORef parentsRef
                proximates <- readIORef proximatesRef
                emit
                  ( k
                      .= Object
                        ( "event-id" .= eventRef
                            <> "start" .= start
                            <> "end" .= end
                            <> ifNotNull "fields" fields
                            <> ifNotNull "parents" parents
                            <> ifNotNull "proximate-causes" proximates
                            <> case r of
                              Abort -> "abort" .= True
                              StructuredFail e -> "structured-exception" .= renderEx e
                              UnstructuredFail e -> "unstructured-exception" .= show e
                              Finalized -> mempty
                        )
                  )
          pure $
            EventImpl
              { referenceImpl = eventRef,
                addFieldImpl = \field ->
                  atomicModifyIORef' fieldsRef \fields ->
                    (uncurry insert (renderField field) fields, ()),
                addParentImpl = \r ->
                  atomicModifyIORef' parentsRef \refs -> (r : refs, ()),
                addProximateImpl = \r ->
                  atomicModifyIORef' proximatesRef \refs -> (r : refs, ()),
                finalizeImpl = finish Finalized,
                failImpl = \me ->
                  finish
                    ( case me of
                        Nothing -> Abort
                        Just e -> case fromException e of
                          Just se -> StructuredFail se
                          Nothing -> UnstructuredFail e
                    )
              },
        newOnceFlag = newOnceFlagMVar
      }

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
simpleJsonStderrBackend = jsonHandleBackend stderr renderJSONException

-- | Why did an 'Event' finish?
data FinishReason stex
  = Abort
  | StructuredFail stex
  | UnstructuredFail SomeException
  | Finalized

-- | Add k .= v if v is not 'null'.
ifNotNull :: (Foldable f, ToJSON (f v)) => Key -> f v -> Object
ifNotNull k v = if null v then mempty else k .= v
