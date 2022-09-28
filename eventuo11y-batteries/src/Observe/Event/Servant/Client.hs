{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Observe.Event.Servant.Client where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Binary.Builder
import Data.ByteString.Lazy hiding (null)
import Data.ByteString.Lazy.Internal (ByteString (..))
import Data.CaseInsensitive
import Data.Coerce
import Data.Foldable
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
import Servant.Client hiding (ClientM)
import Servant.Client.Core.Request
import Servant.Client.Core.RunClient hiding (RunRequest)
import Servant.Client.Internal.HttpClient hiding (ClientM)
import qualified Servant.Client.Internal.HttpClient as S

data RunRequest f where
  RunRequest :: RunRequest RunRequestField

runRequestJSON :: RenderSelectorJSON RunRequest
runRequestJSON RunRequest = ("run-request", runRequestFieldJSON)

data RunRequestField
  = ReqField Request
  | ResField Response

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

data ClientState r = ClientState
  { backend :: !(EventBackend S.ClientM r RunRequest),
    parents :: !(Maybe r)
  }

newtype ClientM r a = ClientM (ReaderT (ClientState r) S.ClientM a)
  deriving newtype (Monad, Functor, Applicative, MonadIO, MonadThrow, MonadCatch, MonadError ClientError, MonadBase IO, MonadReader (ClientState r))
  deriving stock (Generic)

instance MonadBaseControl IO (ClientM r) where
  type StM (ClientM r) a = Either ClientError a

  liftBaseWith go = ClientM $ ReaderT \st -> liftBaseWith (\run -> go (run . flip runReaderT st . coerce))
  restoreM = ClientM . lift . restoreM

instance Alt (ClientM r) where
  x <!> y = ClientM $ ReaderT \st -> (runReaderT (coerce x) st) <!> (runReaderT (coerce y) st)

  some x = ClientM $ ReaderT \st -> some (runReaderT (coerce x) st)
  many x = ClientM $ ReaderT \st -> many (runReaderT (coerce x) st)

instance RunClient (ClientM r) where
  -- ClientM internals needed pending a release with https://github.com/haskell-servant/servant/commit/658585a7cd2191d1387786d236b8b64cd4a13cb6
  runRequestAcceptStatus stats req = ClientM $ ReaderT \st -> S.ClientM $
    withEvent (hoistEventBackend unClientM (backend st)) RunRequest \ev -> do
      addField ev $ ReqField req
      traverse_ (addParent ev) (parents st)
      res <- unClientM $ runRequestAcceptStatus stats req
      addField ev $ ResField res
      pure res
  throwClientError = ClientM . lift . throwClientError

runEventfulClientM :: EventBackend S.ClientM r RunRequest -> ClientM r a -> ClientEnv -> IO (Either ClientError a)
runEventfulClientM backend c = runClientM (runReaderT (coerce c) (ClientState backend Nothing))

withParent :: r -> ClientM r a -> ClientM r a
withParent parent = local (\st -> st {parents = Just parent})
