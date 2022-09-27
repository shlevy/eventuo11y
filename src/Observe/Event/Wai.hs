{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Observe.Event.Wai where

import Control.Exception
import Data.Aeson
import Data.CaseInsensitive
import Data.Text.Encoding
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Network.Socket
import Network.Wai
import Network.Wai.Internal
import Network.Wai.Handler.Warp

import Observe.Event
import Observe.Event.Render.JSON

data ServeRequest f where
  ServeRequest :: ServeRequest RequestField

renderServeRequest :: RenderSelectorJSON ServeRequest
renderServeRequest ServeRequest = ("serve-request", renderRequestField)

data RequestField
  = ReqField Request
  | ResField Response

renderRequest :: Request -> Value
renderRequest (Request {..}) = Object
  ( "remote-addr" .= Object case remoteHost of
      SockAddrInet port addr -> ( "port" .= toInteger port
                               <> "addr" .= hostAddressToTuple addr
                                )
      SockAddrInet6 port flow addr scope -> ( "port" .= toInteger port
                                           <> "flow" .= flow
                                           <> "addr" .= hostAddress6ToTuple addr
                                           <> "scope" .= scope
                                            )
      SockAddrUnix path -> "path" .= path
 <> "method" .= decodeUtf8 requestMethod
 <> "http-version" .= Object ("major" .= httpMajor httpVersion <> "minor" .= httpMinor httpVersion)
 <> "path" .= pathInfo
 <> "query" .= fmap (\(k, mv) -> Object ("param" .= decodeUtf8 k <> case mv of
                                           Just v -> "value" .= decodeUtf8 v
                                           Nothing -> mempty)) queryString
 <> (case requestBodyLength of
       ChunkedBody -> mempty
       KnownLength l -> "length" .= l)
 <> (if null requestHeaders
      then mempty
      else "headers" .= fmap (\(nm, val) -> Object ("name" .= decodeUtf8 (original nm) <> (if nm == "Authorization" then mempty else "val" .= decodeUtf8 val))) requestHeaders)
  )

renderRequestField :: RenderFieldJSON RequestField
renderRequestField (ReqField req) =
  ( "request"
  , renderRequest req
  )
renderRequestField (ResField res) = ("response-status" .= (statusCode $ responseStatus res))

application
  :: EventBackend IO r ServeRequest
  -> (r -> Application)
  -> Application
application backend app req respond = withEvent backend ServeRequest \ev -> do
  addField ev $ ReqField req
  app (reference ev) req \res -> do
    addField ev $ ResField res
    respond res

data OnException f where
  OnException :: OnException OnExceptionField

renderOnException :: (Exception stex) => RenderExJSON stex -> RenderSelectorJSON OnException
renderOnException renderEx OnException = ("on-exception", renderOnExceptionField renderEx)

data OnExceptionField = OnExceptionField (Maybe Request) SomeException

renderOnExceptionField :: (Exception stex) => RenderExJSON stex -> RenderFieldJSON OnExceptionField
renderOnExceptionField renderEx (OnExceptionField mreq e) =
  ( "uncaught-exception"
  , Object
      ( maybe mempty (("request" .=) . renderRequest) mreq
     <> maybe ("unstructured-exception" .= show e) (("structured-exception" .=) . renderEx) (fromException e) 
      )
  )

onException :: EventBackend IO r OnException -> Maybe Request -> SomeException -> IO ()
onException backend req e = if defaultShouldDisplayException e
  then withEvent backend OnException \ev -> addField ev $ OnExceptionField req e
  else pure ()
