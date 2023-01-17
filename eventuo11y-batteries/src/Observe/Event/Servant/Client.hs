{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    ClientM,
    ClientEnv (..),
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

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Binary.Builder
import Data.ByteString.Lazy hiding (null)
import Data.ByteString.Lazy.Internal (ByteString (..))
import Data.CaseInsensitive
import Data.Coerce
import Data.Map.Strict (mapKeys)
import Data.Text.Encoding
import Network.HTTP.Media.MediaType
import Network.HTTP.Media.RenderHeader
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Observe.Event
import Observe.Event.Render.JSON
import Servant.Client hiding (ClientEnv, ClientM, runClientM)
import qualified Servant.Client as S
import Servant.Client.Core.Request
import Servant.Client.Core.RunClient hiding (RunRequest)

-- | A monad to use in place of 'S.ClientM' to get instrumentation on requests.
type ClientM em r s = TransEventMonad (ReaderT (ClientEnv s)) (TransEventMonad (ExceptT ClientError) em) r s

-- | An instrumented 'S.ClientEnv'
data ClientEnv s = ClientEnv
  { env :: !S.ClientEnv,
    injectRunRequest :: !(InjectSelector RunRequest s)
  }

instance (MonadIO (em r s), MonadWithEvent em) => RunClient (ClientM em r s) where
  runRequestAcceptStatus stats req = do
    e <- ask
    injectRunRequest e RunRequest \runReq injField -> withEvent runReq \ev -> do
      addField ev . injField $ ReqField req
      res <- coerce $ const @_ @(ClientEnv s) (liftIO @(em r s) . flip S.runClientM (env e) $ runRequestAcceptStatus stats req)
      addField ev . injField $ ResField res
      pure res
  throwClientError = throwError

-- | Instrumented version of 'S.runClientM'
runClientM :: ClientM em r s a -> ClientEnv s -> em r s (Either ClientError a)
runClientM = coerce

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
