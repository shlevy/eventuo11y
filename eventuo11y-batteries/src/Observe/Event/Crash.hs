{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Observe.Event.Crash where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Void
import Observe.Event
import Observe.Event.Render.JSON

type ScheduleCrash m r = Maybe r -> m ()

type DoCrash m = m ()

hoistScheduleCrash ::
  (forall x. f x -> g x) ->
  ScheduleCrash f r ->
  ScheduleCrash g r
hoistScheduleCrash nt s = nt . s

data Crashing f where
  Crashing :: Crashing Void

renderCrashing :: RenderSelectorJSON Crashing
renderCrashing Crashing = ("crashing", absurd)

withScheduleCrash ::
  (MonadUnliftIO m) =>
  EventBackend m r Crashing ->
  DoCrash m ->
  (ScheduleCrash m r -> m a) ->
  m a
withScheduleCrash backend crash go = withRunInIO $ \runInIO -> do
  scheduleCrashChan <- newEmptyMVar
  let waitForCrash = do
        cause <- takeMVar scheduleCrashChan
        withEvent (hoistEventBackend runInIO backend) Crashing \ev ->
          maybe
            (pure ())
            (addProximate ev)
            cause
        runInIO crash
  withAsync waitForCrash \_ ->
    runInIO $ go $ void . liftIO . tryPutMVar scheduleCrashChan
