{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Description : Instrument wai with eventuo11y
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
module Observe.Event.Wai
  ( -- * Application
    application,

    -- ** Instrumentation
    ServeRequest (..),
    renderServeRequest,
    RequestField (..),
    renderRequestField,

    -- * setOnException
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
import Data.Aeson
import Data.CaseInsensitive
import Data.Text.Encoding
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Internal
import Observe.Event
import Observe.Event.Render.JSON

-- | Run an 'Application' with generic 'Request'/'Response' instrumentation.
application ::
  EventBackend IO r ServeRequest ->
  -- | The application, called with a reference to the parent event.
  (r -> Application) ->
  Application
application backend app req respond = withEvent backend ServeRequest \ev -> do
  addField ev $ ReqField req
  app (reference ev) req \res -> do
    addField ev $ ResField res
    respond res

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

-- | A 'Network.Wai.Handler.Warp.setOnException' callback which creates an 'Event' rendering
-- 'Exception's.
onExceptionCallback :: EventBackend IO r OnException -> Maybe Request -> SomeException -> IO ()
onExceptionCallback backend req e =
  if defaultShouldDisplayException e
    then withEvent backend OnException \ev -> addField ev $ OnExceptionField req e
    else pure ()

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
