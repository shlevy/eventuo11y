{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Instrument servant-client with eventuo11y
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- This module offers a variant of servant-client's 'S.ClientM' which instruments
-- all requests with 'Event's. It also has miscellaneous helpers for instrumenting
-- servant-client functionality in other ways.
module Observe.Event.Servant.Client
  ( -- * ClientM
    ClientM (..),
    runClientM,

    -- ** Instrumentation
    RunRequest (..),
    runRequestJSON,
    RunRequestField (..),
    runRequestFieldJSON,

    -- * Miscellaneous instrumentation
    clientErrorJSON,
    responseJSON,
  )
where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Binary.Builder
import Data.ByteString.Lazy hiding (null)
import Data.ByteString.Lazy.Internal (ByteString (..))
import Data.CaseInsensitive
import Data.Coerce
import Data.Functor.Alt
import Data.Map.Strict (mapKeys)
import Data.Text.Encoding
import GHC.Generics
import Network.HTTP.Media.MediaType
import Network.HTTP.Media.RenderHeader
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Observe.Event
import Observe.Event.Render.JSON
import Servant.Client hiding (ClientM, runClientM)
import Servant.Client.Core.Request
import Servant.Client.Core.RunClient hiding (RunRequest)
import Servant.Client.Internal.HttpClient hiding (ClientM, runClientM)
import qualified Servant.Client.Internal.HttpClient as S

-- | A monad to use in place of 'S.ClientM' to get instrumentation on requests.
newtype ClientM r a = ClientM (ReaderT (EventBackend S.ClientM r RunRequest) S.ClientM a)
  deriving newtype (Monad, Functor, Applicative, MonadIO, MonadThrow, MonadCatch, MonadError ClientError, MonadBase IO, MonadReader (EventBackend S.ClientM r RunRequest))
  deriving stock (Generic)

instance MonadBaseControl IO (ClientM r) where
  type StM (ClientM r) a = Either ClientError a

  liftBaseWith go = ClientM $ ReaderT \backend -> liftBaseWith (\run -> go (run . flip runReaderT backend . coerce))
  restoreM = ClientM . lift . restoreM

instance Alt (ClientM r) where
  x <!> y = ClientM $ ReaderT \backend -> (runReaderT (coerce x) backend) <!> (runReaderT (coerce y) backend)

  some x = ClientM $ ReaderT \backend -> some (runReaderT (coerce x) backend)
  many x = ClientM $ ReaderT \backend -> many (runReaderT (coerce x) backend)

instance RunClient (ClientM r) where
  -- ClientM internals needed pending a release with https://github.com/haskell-servant/servant/commit/658585a7cd2191d1387786d236b8b64cd4a13cb6
  runRequestAcceptStatus stats req = ClientM $ ReaderT \backend -> S.ClientM $
    withEvent (hoistEventBackend unClientM backend) RunRequest \ev -> do
      addField ev $ ReqField req
      res <- unClientM $ runRequestAcceptStatus stats req
      addField ev $ ResField res
      pure res
  throwClientError = ClientM . lift . throwClientError

-- | Instrumented version of 'S.runClientM'
runClientM :: EventBackend S.ClientM r RunRequest -> ClientM r a -> ClientEnv -> IO (Either ClientError a)
runClientM backend c = S.runClientM (runReaderT (coerce c) backend)

-- | Selector for events in 'ClientM'
data RunRequest f where
  RunRequest :: RunRequest RunRequestField

-- | Render a 'RunRequest' and the fields of its selected events as JSON
runRequestJSON :: RenderSelectorJSON RunRequest
runRequestJSON RunRequest = ("run-request", runRequestFieldJSON)

-- | A field for v'RunRequest' events.
data RunRequestField
  = ReqField Request
  | ResField Response

-- | Render a 'RunRequestField' as JSON.
runRequestFieldJSON :: RenderFieldJSON RunRequestField
runRequestFieldJSON (ReqField Request {..}) =
  ( "request",
    Object
      ( "path" .= decodeUtf8 (toStrict $ toLazyByteString requestPath)
          <> ( if null requestQueryString
                 then mempty
                 else
                   "query"
                     .= ( ( \(k, mv) ->
                              Object
                                ( "key" .= decodeUtf8 k
                                    <> maybe mempty (("value" .=) . decodeUtf8) mv
                                )
                          )
                            <$> requestQueryString
                        )
             )
          <> case requestBody of
            Nothing -> mempty
            Just (body, ty) ->
              ( "content-type" .= decodeUtf8 (renderHeader ty)
                  <> case body of
                    RequestBodyBS bs -> "body" .= decodeUtf8 bs
                    RequestBodyLBS Empty -> "body" .= False
                    RequestBodyLBS (Chunk bs Empty) -> "body" .= decodeUtf8 bs
                    _ -> mempty
              )
          <> ( if null requestAccept
                 then mempty
                 else "accept" .= fmap (decodeUtf8 . renderHeader) requestAccept
             )
          <> ( if null requestHeaders
                 then mempty
                 else "headers" .= fmap (\(nm, val) -> Object ("name" .= decodeUtf8 (original nm) <> (if nm == "Authorization" then mempty else "val" .= decodeUtf8 val))) requestHeaders
             )
          <> "http-version" .= Object ("major" .= httpMajor requestHttpVersion <> "minor" .= httpMinor requestHttpVersion)
          <> "method" .= decodeUtf8 requestMethod
      )
  )
runRequestFieldJSON (ResField res) =
  ( "response",
    responseJSON res False
  )

-- | Render a 'ClientError', considered as an 'Event' field, as JSON
clientErrorJSON :: RenderFieldJSON ClientError
clientErrorJSON (FailureResponse _ res) = ("failure-response", responseJSON res True)
clientErrorJSON (DecodeFailure err res) = ("decode-failure", Object ("response" .= responseJSON res True <> "err" .= String err))
clientErrorJSON (UnsupportedContentType ty res) =
  ( "unsupported-content-type",
    Object
      ( "response" .= responseJSON res True
          <> "main-type" .= decodeUtf8 (original $ mainType ty)
          <> "sub-type" .= decodeUtf8 (original $ subType ty)
          <> "parameters" .= fmap (decodeUtf8 . original) (mapKeys (decodeUtf8 . original) $ parameters ty)
      )
  )
clientErrorJSON (InvalidContentTypeHeader res) = ("invalid-content-type-header", responseJSON res True)
clientErrorJSON (ConnectionError e) = ("connection-error", toJSON $ show e)

-- | Render a 'Servant.Client.Core.Response' as JSON, optionally forcing rendering the body even if it's large.
responseJSON :: Response -> Bool -> Value
responseJSON Response {..} forceBody =
  Object
    ( "status" .= statusCode responseStatusCode
        <> ( if null responseHeaders
               then mempty
               else "headers" .= fmap (\(nm, val) -> Object ("name" .= decodeUtf8 (original nm) <> (if nm == "Cookie" then mempty else "val" .= decodeUtf8 val))) responseHeaders
           )
        <> "http-version" .= Object ("major" .= httpMajor responseHttpVersion <> "minor" .= httpMinor responseHttpVersion)
        <> ( if forceBody
               then "body" .= (decodeUtf8 $ toStrict responseBody)
               else case responseBody of
                 Empty -> "body" .= False
                 Chunk bs Empty -> "body" .= decodeUtf8 bs
                 _ -> mempty
           )
    )
