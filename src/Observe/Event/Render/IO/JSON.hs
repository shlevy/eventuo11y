{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Observe.Event.Render.IO.JSON
  ( JSONRef(..)
  , jsonHandleBackend
  ) where

import Data.Aeson
import Data.Aeson.KeyMap (insert)
import Data.Coerce
import Data.IORef
import Control.Concurrent.MVar
import Control.Exception
import System.IO (Handle)
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Data.Time.Clock
import Data.UUID (UUID)
import Data.UUID.V4

import Observe.Event
import Observe.Event.Render.JSON

newtype JSONRef = JSONRef UUID deriving newtype (ToJSON)

-- First finish event dominates
jsonHandleBackend :: (Exception stex) => Handle -> RenderExJSON stex -> RenderSelectorJSON s -> IO (EventBackend IO JSONRef s)
jsonHandleBackend h renderEx renderSel = do
  outputLock <- newMVar ()
  let
    emit :: Object -> IO ()
    emit o = withMVar outputLock \() ->
        hPutStrLn h $ encode o
  pure $ MkEventBackend
    { newEvent = \sel -> do
        let (k, renderField) = renderSel sel
        eventRef <- coerce nextRandom
        start <- getCurrentTime
        fieldsRef <- newIORef mempty
        parentsRef <- newIORef mempty
        proximatesRef <- newIORef mempty
        finish <- do
          finishOnce <- newEmptyMVar
          pure $ \r -> tryPutMVar finishOnce () >>= \case
            False -> pure ()
            True -> do
              end <- getCurrentTime
              fields <- readIORef fieldsRef
              parents <- readIORef parentsRef
              proximates <- readIORef proximatesRef
              emit
                ( k .= Object
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
        pure $ MkEvent
          { ref = eventRef
          , addField = \field ->
              atomicModifyIORef' fieldsRef \fields ->
                (uncurry insert (renderField field) fields, ())
          , addReference = \(MkReference ty r) -> do
              let refRef = case ty of
                    Parent -> parentsRef
                    Proximate -> proximatesRef
              atomicModifyIORef' refRef \refs -> (r : refs, ())
          , finalize = finish Finalized
          , failEvent = \me -> finish (case me of
              Nothing -> Abort
              Just e -> case fromException e of
                Just se -> StructuredFail se
                Nothing -> UnstructuredFail e)
          }
    }

data FinishReason stex
  = Abort
  | StructuredFail stex
  | UnstructuredFail SomeException
  | Finalized

ifNotNull :: (Foldable f, ToJSON (f v)) => Key -> f v -> Object
ifNotNull k v = if null v then mempty else k .= v
