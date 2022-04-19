{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Observe.Event.Servant.Client where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Except
import Data.Aeson
import Data.CaseInsensitive
import Data.Coerce
import Data.Binary.Builder
import Data.ByteString.Lazy hiding (null)
import Data.Foldable
import Data.Functor.Alt
import Data.Text.Encoding
import Servant.Client.Core.RunClient
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control
import GHC.Generics
import Servant.Client
import Servant.Client.Internal.HttpClient
import Servant.Client.Core.Request
import Network.HTTP.Media.RenderHeader
import Network.HTTP.Media.MediaType
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Data.Map.Strict (mapKeys)

import Observe.Event
import Observe.Event.Render.JSON

data RunRequestField
  = ReqField Request
  | ResField Response

responseJSON :: Response -> Value
responseJSON Response {..} = Object
  ( "status" .= statusCode responseStatusCode
 <> (if null responseHeaders
      then mempty
      else "headers" .= fmap (\(nm, val) -> Object ("name" .= decodeUtf8 (original nm) <> (if nm == "Cookie" then mempty else "val" .= decodeUtf8 val))) responseHeaders)
 <> "http-version" .= Object ("major" .= httpMajor responseHttpVersion <> "minor" .= httpMinor responseHttpVersion)
  )

runRequestFieldJSON :: RenderFieldJSON RunRequestField
runRequestFieldJSON (ReqField Request {..}) =
  ( "request"
  , Object
      ( "path" .= decodeUtf8 (toStrict $ toLazyByteString requestPath)
     <> (if null requestQueryString
          then mempty
          else "query" .= ((\(k, mv) -> Object
                             ( "key" .= decodeUtf8 k
                            <> maybe mempty (("value" .=) . decodeUtf8) mv
                             )
                           ) <$> requestQueryString))
     <> case requestBody of
          Nothing -> mempty
          Just (body, ty) ->
            ( "content-type" .= decodeUtf8 (renderHeader ty)
           <> case body of
                RequestBodyBS bs -> "body" .= decodeUtf8 bs
                _ -> mempty
            )
     <> (if null requestAccept
          then mempty
          else "accept" .= fmap (decodeUtf8 . renderHeader) requestAccept)
     <> (if null requestHeaders
          then mempty
          else "headers" .= fmap (\(nm, val) -> Object ("name" .= decodeUtf8 (original nm) <> (if nm == "Authorization" then mempty else "val" .= decodeUtf8 val))) requestHeaders)
     <> "http-version" .= Object ("major" .= httpMajor requestHttpVersion <> "minor" .= httpMinor requestHttpVersion)
     <> "method" .= decodeUtf8 requestMethod
      )
  )
runRequestFieldJSON (ResField res) =
  ( "response"
  , responseJSON res
  )

clientErrorJSON :: RenderFieldJSON ClientError
clientErrorJSON (FailureResponse _ res) = ("failure-response", responseJSON res)
clientErrorJSON (DecodeFailure err res) = ("decode-failure", Object ("response" .= responseJSON res <> "err" .= String err))
clientErrorJSON (UnsupportedContentType ty res) =
  ("unsupported-content-type", Object
    ( "response" .= responseJSON res
   <> "main-type" .= decodeUtf8 (original $ mainType ty)
   <> "sub-type" .=  decodeUtf8 (original $ subType ty)
   <> "parameters" .=  fmap (decodeUtf8 . original) (mapKeys (decodeUtf8 . original) $ parameters ty)
    ))
clientErrorJSON (InvalidContentTypeHeader res) = ("invalid-content-type-header", responseJSON res)
clientErrorJSON (ConnectionError e) = ("connection-error", toJSON $ show e)

data EventfulClientState r = MkEventfulClientState
  { mkE :: !(ClientM (Event ClientM r RunRequestField))
  , refs :: !([ Reference r ])
  }

newtype EventfulClientM r a = MkEventfulClientM (ReaderT (EventfulClientState r) ClientM a) deriving newtype (Monad, Functor, Applicative, MonadIO, MonadThrow, MonadCatch, MonadError ClientError, MonadBase IO, MonadReader (EventfulClientState r)) deriving stock (Generic)

instance MonadBaseControl IO (EventfulClientM r) where
  type StM (EventfulClientM r) a = Either ClientError a

  liftBaseWith go = MkEventfulClientM $ ReaderT \st -> liftBaseWith (\run -> go (run . flip runReaderT st . coerce))
  restoreM = MkEventfulClientM . lift . restoreM

instance Alt (EventfulClientM r) where
  x <!> y = MkEventfulClientM $ ReaderT \st -> (runReaderT (coerce x) st) <!> (runReaderT (coerce y) st)

  some x = MkEventfulClientM $ ReaderT \st -> some (runReaderT (coerce x) st)
  many x = MkEventfulClientM $ ReaderT \st -> many (runReaderT (coerce x) st)

instance RunClient (EventfulClientM r) where
  -- ClientM internals needed pending a release with https://github.com/haskell-servant/servant/commit/658585a7cd2191d1387786d236b8b64cd4a13cb6
  runRequestAcceptStatus stats req = MkEventfulClientM $ ReaderT \st -> ClientM $ withEvent (hoistEvent unClientM <$> unClientM (mkE st)) \ev' -> unClientM $ do
    let ev = hoistEvent ClientM ev'
    addField ev $ ReqField req
    traverse_ (addReference ev) (refs st)
    res <- runRequestAcceptStatus stats req
    addField ev $ ResField res
    pure res
  throwClientError = MkEventfulClientM . lift . throwClientError

runEventfulClientM :: ClientM (Event ClientM r RunRequestField) -> EventfulClientM r a -> ClientEnv -> IO (Either ClientError a)
runEventfulClientM mkE c = runClientM (runReaderT (coerce c) (MkEventfulClientState mkE []))

withReferences :: [Reference r] -> EventfulClientM r a -> EventfulClientM r a
withReferences refs' = local (\st -> st { refs = refs' ++ refs st })

withParent :: r -> EventfulClientM r a -> EventfulClientM r a
withParent = withReferences . (: []) . MkReference Parent
