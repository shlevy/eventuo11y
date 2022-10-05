{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Description : Combine eventuo11y instrumentation with crash-only designs.
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- This module contains helpers to use eventuo11y to instrument crashes in a
-- crash-only application design, where it is insufficient to simply crash in
-- a top-level exception handler. For example, a "Network.Wai.Handler.Warp"
-- server may want to crash in its 'Network.Wai.Handler.Warp.setOnException'
-- callback, but only when the exception is due to a server-side issue and only
-- after all open requests have been serviced.
module Observe.Event.Crash
  ( withScheduleCrash,
    ScheduleCrash (..),
    DoCrash,
    hoistScheduleCrash,

    -- * Instrumentation
    Crashing (..),
    renderCrashing,
  )
where

import Control.Monad.Catch
import Data.Void
import Observe.Event
import Observe.Event.BackendModification
import Observe.Event.Render.JSON

-- | Run an action with a 'ScheduleCrash' that can be called to crash the application.
withScheduleCrash ::
  (MonadMask m) =>
  EventBackend m r Crashing ->
  -- | Actually perform the crash.
  DoCrash m ->
  (ScheduleCrash m r -> m a) ->
  m a
withScheduleCrash backend crash go =
  go $ ScheduleCrash \mods ->
    let backend' = modifyEventBackend mods backend
     in withEvent backend' Crashing $ const crash

-- | Function to schedule an application crash.
newtype ScheduleCrash m r = ScheduleCrash
  { -- | Schedule a crash
    schedule :: forall r'. EventBackendModifiers r r' -> m ()
  }

-- | Function to actually initiate the crash.
type DoCrash m = m ()

-- | Hoist a 'ScheduleCrash' along a given natural transformation into a new functor.
hoistScheduleCrash ::
  -- | Natural transformation from @f@ to @g@.
  (forall x. f x -> g x) ->
  ScheduleCrash f r ->
  ScheduleCrash g r
hoistScheduleCrash nt (ScheduleCrash {..}) = ScheduleCrash $ nt . schedule

-- | Event selector for 'withScheduleCrash'.
data Crashing f where
  Crashing :: Crashing Void

-- | Render a 'Crashing' and its sub-events to JSON.
renderCrashing :: RenderSelectorJSON Crashing
renderCrashing Crashing = ("crashing", absurd)
