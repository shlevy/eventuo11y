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

-- | Function to schedule an application crash, perhaps caused by a referenced 'Event'.
type ScheduleCrash m r = Maybe r -> m ()

-- | Function to actually initiate the crash.
type DoCrash m = m ()

-- | Hoist a 'ScheduleCrash' along a given natural transformation into a new functor.
hoistScheduleCrash ::
  -- | Natural transformation from @f@ to @g@.
  (forall x. f x -> g x) ->
  ScheduleCrash f r ->
  ScheduleCrash g r
hoistScheduleCrash nt s = nt . s

-- | Run an action with a 'ScheduleCrash' to crash the application.
withScheduleCrash ::
  (MonadUnliftIO m) =>
  EventBackend m r Crashing ->
  -- | Actually perform the crash.
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

-- | Event selector for 'withScheduleCrash'.
data Crashing f where
  Crashing :: Crashing Void

-- | Render a 'Crashing' and its sub-events to JSON.
renderCrashing :: RenderSelectorJSON Crashing
renderCrashing Crashing = ("crashing", absurd)
