{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Description : Interface for implementing EventBackends
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- This is the primary module needed to write new 'EventBackend's.
module Observe.Event.Backend
  ( EventBackend (..),
    EventImpl (..),
    unitEventBackend,
    pairEventBackend,
    hoistEventBackend,
    hoistEventImpl,
    narrowEventBackend,
    narrowEventBackend',

    -- * OnceFlags

    -- | Generic helper to make operations idempotent.
    OnceFlag (..),
    FlagState (..),
    runOnce,
    hoistOnceFlag,
    alwaysNewOnceFlag,
    newOnceFlagMVar,
  )
where

import Control.Exception
import Control.Monad.Primitive
import Data.Functor
import Data.Primitive.MVar

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
    failImpl :: !(SomeException -> m ())
  }

-- | A no-op 'EventBackend'.
--
-- This can be used if calling instrumented code from an un-instrumented
-- context, or to purposefully ignore instrumentation from some call.
--
-- 'unitEventBackend' is the algebraic unit of 'pairEventBackend'.
unitEventBackend :: Applicative m => EventBackend m () s
unitEventBackend =
  EventBackend
    { newEventImpl = \_ ->
        pure $
          EventImpl
            { referenceImpl = (),
              addFieldImpl = const $ pure (),
              addParentImpl = const $ pure (),
              addProximateImpl = const $ pure (),
              finalizeImpl = pure (),
              failImpl = const $ pure ()
            },
      newOnceFlag = pure alwaysNewOnceFlag
    }

-- | An 'EventBackend' which sequentially generates 'Observe.Event.Event's in the two given 'EventBackend's.
--
-- This can be used to emit instrumentation in multiple ways (e.g. logs to grafana and metrics on
-- a prometheus HTML page).
pairEventBackend :: Applicative m => EventBackend m a s -> EventBackend m b s -> EventBackend m (a, b) s
pairEventBackend x y =
  EventBackend
    { newEventImpl = \sel -> do
        xImpl <- newEventImpl x sel
        yImpl <- newEventImpl y sel
        pure $
          EventImpl
            { referenceImpl = (referenceImpl xImpl, referenceImpl yImpl),
              addFieldImpl = \f -> addFieldImpl xImpl f *> addFieldImpl yImpl f,
              addParentImpl = \(px, py) -> addParentImpl xImpl px *> addParentImpl yImpl py,
              addProximateImpl = \(px, py) -> addProximateImpl xImpl px *> addProximateImpl yImpl py,
              finalizeImpl = finalizeImpl xImpl *> finalizeImpl yImpl,
              failImpl = \e -> failImpl xImpl e *> failImpl yImpl e
            },
      newOnceFlag = do
        xOnce <- newOnceFlag x
        yOnce <- newOnceFlag y
        pure $
          OnceFlag $ do
            xSet <- checkAndSet xOnce
            ySet <- checkAndSet yOnce
            pure $ case (xSet, ySet) of
              (NewlySet, NewlySet) -> NewlySet
              _ -> AlreadySet
    }

-- | Hoist an 'EventBackend' along a given natural transformation into a new monad.
hoistEventBackend ::
  (Functor m, Functor n) =>
  -- | Natural transformation from @m@ to @n@.
  (forall x. m x -> n x) ->
  EventBackend m r s ->
  EventBackend n r s
hoistEventBackend nt backend =
  EventBackend
    { newEventImpl = nt . fmap (hoistEventImpl nt) . newEventImpl backend,
      newOnceFlag = hoistOnceFlag nt <$> (nt $ newOnceFlag backend)
    }

-- | Hoist an 'EventImpl' along a given natural transformation into a new monad.
hoistEventImpl :: (forall x. m x -> n x) -> EventImpl m r f -> EventImpl n r f
hoistEventImpl nt (EventImpl {..}) =
  EventImpl
    { referenceImpl,
      addFieldImpl = nt . addFieldImpl,
      addParentImpl = nt . addParentImpl,
      addProximateImpl = nt . addProximateImpl,
      finalizeImpl = nt finalizeImpl,
      failImpl = nt . failImpl
    }

-- | Narrow an 'EventBackend' to a new selector type via a given injection function.
--
-- A typical usage, where component A calls component B, would be to have A's selector
-- type have a constructor to take any value of B's selector type (and preserve the field)
-- and then call 'narrowEventBackend' with that constructor when invoking functions in B.
--
-- See 'narrowEventBackend'' for a more general, if unweildy, variant.
narrowEventBackend ::
  (Functor m) =>
  -- | Inject a narrow selector into the wider selector type.
  (forall f. s f -> t f) ->
  EventBackend m r t ->
  EventBackend m r s
narrowEventBackend inj =
  narrowEventBackend'
    (\sel withInjField -> withInjField (inj sel) id)

-- | Narrow an 'EventBackend' to a new selector type via a given injection function.
--
-- See 'narrowEventBackend' for a simpler, if less general, variant.
narrowEventBackend' ::
  (Functor m) =>
  -- | Simultaneously inject a narrow selector into the wider selector type
  -- and the narrow selector's field into the wider selector's field type.
  (forall f. s f -> forall a. (forall g. t g -> (f -> g) -> a) -> a) ->
  EventBackend m r t ->
  EventBackend m r s
narrowEventBackend' inj backend =
  EventBackend
    { newEventImpl = \sel -> inj sel \sel' injField ->
        newEventImpl backend sel' <&> \case
          EventImpl {..} ->
            EventImpl
              { addFieldImpl = addFieldImpl . injField,
                ..
              },
      newOnceFlag = newOnceFlag backend
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

-- | A 'OnceFlag' using an 'MVar'.
newOnceFlagMVar :: (PrimMonad m) => m (OnceFlag m)
newOnceFlagMVar = do
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
