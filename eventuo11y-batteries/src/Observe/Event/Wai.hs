{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeInType #-}

-- |
-- Description : Instrument wai with eventuo11y
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
module Observe.Event.Wai
  ( -- * Application
    Application,
    application,

    -- ** Instrumentation
    ServeRequest (..),
    renderServeRequest,
    RequestField (..),
    renderRequestField,

    -- * setOnException
    OnExceptionCallback,
    onExceptionCallback,

    -- ** Instrumentation
    OnException (..),
    renderOnException,
    OnExceptionField (..),
    renderOnExceptionField,

    -- * Miscellaneous instrumentation
    renderRequest,
  )
where

import Control.Exception
import Control.Monad.IO.Unlift
import Data.Aeson
import Data.CaseInsensitive
import Data.Kind
import Data.Text.Encoding
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Network.Socket
import Network.Wai hiding (Application)
import qualified Network.Wai as W
import Network.Wai.Handler.Warp
import Network.Wai.Internal
import Observe.Event
import Observe.Event.Class
import Observe.Event.Render.JSON

-- | An instrumented 'W.Application'
type Application :: EventMonadKind -> ReferenceKind -> SelectorKind -> Type
type Application em r s = Request -> (Response -> em r s ResponseReceived) -> em r s ResponseReceived

-- | Run an 'Application' with generic 'Request'/'Response' instrumentation.
application ::
  (MonadUnliftIO (em r s), MonadWithEvent em) =>
  InjectSelector ServeRequest s ->
  Application em r s ->
  em r s W.Application
application inj app = withRunInIO \runInIO -> pure \req respond -> runInIO $
  inj ServeRequest \serveReq injField -> withEvent serveReq \ev -> do
    addField ev . injField $ ReqField req
    app req \res -> do
      addField ev . injField $ ResField res
      liftIO $ respond res

-- | Event selector for 'application'.
data ServeRequest f where
  ServeRequest :: ServeRequest RequestField

-- | Render a 'ServeRequest', and any 'Event's selected by it, to JSON
renderServeRequest :: RenderSelectorJSON ServeRequest
renderServeRequest ServeRequest = ("serve-request", renderRequestField)

-- | A field for v'ServeRequest' 'Event's.
data RequestField
  = ReqField Request
  | ResField Response

-- | Render a 'RequestField' to JSON
renderRequestField :: RenderFieldJSON RequestField
renderRequestField (ReqField req) =
  ( "request",
    renderRequest req
  )
renderRequestField (ResField res) = ("response-status" .= (statusCode $ responseStatus res))

-- | An instrumented 'Network.Wai.Handler.Warp.setOnException' callback.
type OnExceptionCallback :: EventMonadKind -> ReferenceKind -> SelectorKind -> Type
type OnExceptionCallback em r s = Maybe Request -> SomeException -> em r s ()

-- | Convert an 'OnExceptionCallback' to a 'Network.Wai.Handler.Warp.setOnException' callback.
--
-- The 'OnExceptionCallback' is called as the child of an 'Event' rendering the exception, if
-- it's one that should be displayed according to 'defaultShouldDisplayException'.
--
-- Ideally this would have a way to get a parent 'Event' from 'application'. Would be nice to
-- use 'vault', but there doesn't seem to be a way to get at the 'Request' that Warp will pass
-- here.
onExceptionCallback ::
  (MonadUnliftIO (em r s), MonadWithEvent em) =>
  InjectSelector OnException s ->
  OnExceptionCallback em r s ->
  em r s (Maybe Request -> SomeException -> IO ())
onExceptionCallback inj cb = withRunInIO \runInIO -> pure \req e -> runInIO $
  case defaultShouldDisplayException e of
    True -> inj OnException \onEx injField -> withEvent onEx \ev -> do
      addField ev . injField $ OnExceptionField req e
      cb req e
    False -> cb req e

-- | Selector for 'Observe.Event.Wai.onException'
data OnException f where
  OnException :: OnException OnExceptionField

-- | Render an 'OnException', and its selected-for 'Event's, as JSON, with a provided base structured exception type.
renderOnException :: (Exception stex) => RenderExJSON stex -> RenderSelectorJSON OnException
renderOnException renderEx OnException = ("on-exception", renderOnExceptionField renderEx)

-- | A field for a v'OnException' 'Event'.
data OnExceptionField = OnExceptionField (Maybe Request) SomeException

-- | Render an 'OnExceptionField' as JSON, with a provided base structured exception type.
renderOnExceptionField :: (Exception stex) => RenderExJSON stex -> RenderFieldJSON OnExceptionField
renderOnExceptionField renderEx (OnExceptionField mreq e) =
  ( "uncaught-exception",
    Object
      ( maybe mempty (("request" .=) . renderRequest) mreq
          <> maybe ("unstructured-exception" .= show e) (("structured-exception" .=) . renderEx) (fromException e)
      )
  )

-- | Render a 'Request' to JSON.
renderRequest :: Request -> Value
renderRequest (Request {..}) =
  Object
    ( "remote-addr" .= Object case remoteHost of
        SockAddrInet port addr ->
          ( "port" .= toInteger port
              <> "addr" .= hostAddressToTuple addr
          )
        SockAddrInet6 port flow addr scope ->
          ( "port" .= toInteger port
              <> "flow" .= flow
              <> "addr" .= hostAddress6ToTuple addr
              <> "scope" .= scope
          )
        SockAddrUnix path -> "path" .= path
        <> "method" .= decodeUtf8 requestMethod
        <> "http-version" .= Object ("major" .= httpMajor httpVersion <> "minor" .= httpMinor httpVersion)
        <> "path" .= pathInfo
        <> "query"
          .= fmap
            ( \(k, mv) ->
                Object
                  ( "param" .= decodeUtf8 k <> case mv of
                      Just v -> "value" .= decodeUtf8 v
                      Nothing -> mempty
                  )
            )
            queryString
        <> ( case requestBodyLength of
               ChunkedBody -> mempty
               KnownLength l -> "length" .= l
           )
        <> ( if null requestHeaders
               then mempty
               else "headers" .= fmap (\(nm, val) -> Object ("name" .= decodeUtf8 (original nm) <> (if nm == "Authorization" then mempty else "val" .= decodeUtf8 val))) requestHeaders
           )
    )
