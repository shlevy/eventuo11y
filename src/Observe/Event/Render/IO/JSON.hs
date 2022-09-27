{-# LANGUAGE BlockArguments #-}
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
import Observe.Event.Implementation
import Observe.Event.Render.JSON

newtype JSONRef = JSONRef UUID deriving newtype (ToJSON)

jsonHandleBackend :: (Exception stex) => Handle -> RenderExJSON stex -> RenderSelectorJSON s -> IO (EventBackend IO JSONRef s)
jsonHandleBackend h renderEx renderSel = do
  outputLock <- newMVar ()
  let
    emit :: Object -> IO ()
    emit o = withMVar outputLock \() ->
        hPutStrLn h $ encode o
  pure $ EventBackend
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
        pure $ EventImpl
          { referenceImpl = eventRef
          , addFieldImpl = \field ->
              atomicModifyIORef' fieldsRef \fields ->
                (uncurry insert (renderField field) fields, ())
          , addParentImpl = \r ->
              atomicModifyIORef' parentsRef \refs -> (r : refs, ())
          , addProximateImpl = \r ->
              atomicModifyIORef' proximatesRef \refs -> (r : refs, ())
          , finalizeImpl = finish Finalized
          , failImpl = \me -> finish (case me of
              Nothing -> Abort
              Just e -> case fromException e of
                Just se -> StructuredFail se
                Nothing -> UnstructuredFail e)
          }
    , newOnceFlag = newOnceFlagIO
    }

data FinishReason stex
  = Abort
  | StructuredFail stex
  | UnstructuredFail SomeException
  | Finalized

ifNotNull :: (Foldable f, ToJSON (f v)) => Key -> f v -> Object
ifNotNull k v = if null v then mempty else k .= v
