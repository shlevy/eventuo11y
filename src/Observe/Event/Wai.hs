{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Observe.Event.Wai where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.CaseInsensitive
import Data.Text.Encoding
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Network.Socket
import Network.Wai
import Network.Wai.Internal
import Network.Wai.Handler.Warp
import Data.Void

import Observe.Event
import Observe.Event.Render.JSON

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

eventfulApplication
  :: IO (Event IO r RequestField)
  -> (r -> Application)
  -> Application
eventfulApplication mkE app req respond = withEvent mkE \ev -> do
  addField ev $ ReqField req
  app (ref ev) req \res -> do
    addField ev $ ResField res
    respond res

type ScheduleShutdown m r = Maybe r -> m ()

hoistScheduleShutdown
  :: (forall x . f x -> g x)
  -> ScheduleShutdown f r
  -> ScheduleShutdown g r
hoistScheduleShutdown nt s = nt . s

withScheduleShutdownHandler :: IO (Event IO r Void) -> (ScheduleShutdown IO r -> (IO () -> IO ()) -> IO a) -> IO a
withScheduleShutdownHandler mkEv go = do
  closeSocketChan <- newEmptyMVar
  shutdownChan <- newEmptyMVar
  let waitForShutdown = do
        (closeSocket, cause) <- concurrently (takeMVar closeSocketChan) (takeMVar shutdownChan)
        withEvent mkEv \ev ->
          maybe (pure ()) (addProximateCause ev) cause
        closeSocket
  withAsync waitForShutdown \_ ->
    go (void . tryPutMVar shutdownChan) (putMVar closeSocketChan)

data OnExceptionField = OnExceptionField (Maybe Request) SomeException

renderOnExceptionField :: (Exception stex) => RenderExJSON stex -> RenderFieldJSON OnExceptionField
renderOnExceptionField renderEx (OnExceptionField mreq e) =
  ( "uncaught-exception"
  , Object
      ( maybe mempty (("request" .=) . renderRequest) mreq
     <> maybe ("unstructured-exception" .= show e) (("structured-exception" .=) . renderEx) (fromException e) 
      )
  )

eventfulOnException :: IO (Event IO r OnExceptionField) -> Maybe Request -> SomeException -> IO ()
eventfulOnException mkE req e = if defaultShouldDisplayException e
  then withEvent mkE \ev -> addField ev $ OnExceptionField req e
  else pure ()
