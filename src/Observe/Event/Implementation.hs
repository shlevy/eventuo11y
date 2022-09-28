{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Description : Interface for implementing EventBackends
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- This is the primary module needed to write new 'EventBackend's.
module Observe.Event.Implementation
  ( EventBackend (..),
    EventImpl (..),

    -- * OnceFlags

    -- | Generic helper to make operations idempotent.
    OnceFlag (..),
    FlagState (..),
    runOnce,
    hoistOnceFlag,
    alwaysNewOnceFlag,
    newOnceFlagIO,
  )
where

import Control.Concurrent.MVar
import Control.Exception
import Data.Functor

-- | A backend for creating t'Observe.Event.Event's.
--
-- Different 'EventBackend's will be used to emit instrumentation to
-- different systems. Multiple backends can be combined with
-- 'Observe.Event.pairEventBackend'.
--
-- A simple 'EventBackend' for logging to a t'System.IO.Handle' can be
-- created with 'Observe.Event.Render.IO.JSON.jsonHandleBackend'.
--
-- Typically the entrypoint for some eventuo11y-instrumented code will
-- take an 'EventBackend', polymorphic in @r@ and possibly @m@. Calling
-- code can use 'Observe.Event.subEventBackend' to place the resulting
-- events in its hierarchy.
--
-- From an 'EventBackend', new events can be created via selectors
-- (of type @s f@ for some field type @f@), typically with the
-- [resource-safe allocation functions](Observe-Event.html#g:resourcesafe).
-- Selectors are values which designate the general category of event
-- being created, as well as the type of fields that can be added to it.
-- For example, a web service's selector type may have a @ServicingRequest@
-- constructor, whose field type includes a @ResponseCode@ constructor which
-- records the HTTP status code.
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see t'Observe.Event.Dynamic.DynamicEventSelector' for a generic option).
--
-- Implementations must ensure that 'EventBackend's and their underlying t'Observe.Event.Event's
-- are safe to use across threads.
--
-- [@m@]: The monad we're instrumenting in.
-- [@r@]: The type of event references used in this 'EventBackend'. See 'Observe.Event.reference'.
-- [@s@]: The type of event selectors.
data EventBackend m r s = EventBackend
  { -- | Create a new 'EventImpl' corresponding to the given selector.
    newEventImpl :: !(forall f. s f -> m (EventImpl m r f)),
    -- | Allocate a new 'OnceFlag' in our monad.
    newOnceFlag :: !(m (OnceFlag m))
  }

-- | The internal implementation of an t'Observe.Event.Event'.
--
-- All fields have corresponding [event manipulation functions](Observe-Event.html#g:eventmanip),
-- except that 'finalizeImpl' and 'failImpl' can assume that they will only ever be called
-- once (i.e., 'EventImpl' implementations do __not__ have to implement locking internally).
data EventImpl m r f = EventImpl
  { referenceImpl :: !r,
    addFieldImpl :: !(f -> m ()),
    addParentImpl :: !(r -> m ()),
    addProximateImpl :: !(r -> m ()),
    finalizeImpl :: !(m ()),
    failImpl :: !(Maybe SomeException -> m ())
  }

-- | The state of a 'OnceFlag'
data FlagState
  = -- | The flag was not set, but is now
    NewlySet
  | -- | The flag was already set
    AlreadySet

-- | A flag to ensure only one operation from some class is performed, once.
--
-- Typically consumed via 'runOnce'
newtype OnceFlag m = OnceFlag
  { -- | Get the state of the 'OnceFlag', and set the flag.
    --
    -- This operation should be atomic, and ideally would only
    -- return 'NewlySet' once. In monads that don't support it,
    -- at a minimum it must be monotonic (once one caller gets
    -- 'AlreadySet', all callers will).
    checkAndSet :: m FlagState
  }

-- | Run an operation if no other operations using this
-- 'OnceFlag' have run.
runOnce :: (Monad m) => OnceFlag m -> m () -> m ()
runOnce f go =
  checkAndSet f >>= \case
    NewlySet -> go
    AlreadySet -> pure ()

-- | A 'OnceFlag' in 'IO' using an 'MVar'.
newOnceFlagIO :: IO (OnceFlag IO)
newOnceFlagIO = do
  flag <- newEmptyMVar
  pure $
    OnceFlag $
      tryPutMVar flag () <&> \case
        False -> AlreadySet
        True -> NewlySet

-- | A 'OnceFlag' which is always 'NewlySet'.
--
-- Only safe to use if the operations to be guarded
-- by the flag are already idempotent.
alwaysNewOnceFlag :: (Applicative m) => OnceFlag m
alwaysNewOnceFlag = OnceFlag $ pure NewlySet

-- | Hoist a 'OnceFlag' along a given natural transformation into a new monad.
hoistOnceFlag ::
  -- | Natural transformation from @f@ to @g@
  (forall x. f x -> g x) ->
  OnceFlag f ->
  OnceFlag g
hoistOnceFlag nt (OnceFlag cs) = OnceFlag (nt cs)
