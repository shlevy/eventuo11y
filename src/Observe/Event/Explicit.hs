{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Description : Instrumentation with explicit 'EventBackend' passing
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- t'Observe.Event.MonadEvent' and 'Observe.Event.EventT'-based instrumentation
-- implicitly track the underlying 'EventBackend' for you. This module is for those
-- who would rather pass around 'EventBackend's explicitly.
module Observe.Event.Explicit
  ( Event,
    hoistEvent,

    -- * Event manipulation #eventmanip#
    addField,
    reference,

    -- * Resource-safe event allocation #resourcesafe#
    NewEventArgs (..),
    emitImmediateEvent,
    allocateEvent,
    allocateEventArgs,
    withEvent,
    withEventArgs,

    -- * 'EventBackend's
    EventBackend,

    -- ** Backend transformation
    subEventBackend,
    causedEventBackend,
    hoistEventBackend,
    narrowEventBackend,
    InjectSelector,
    injectSelector,
    idInjectSelector,
    setAncestorEventBackend,
    setInitialCauseEventBackend,

    -- ** Backend composition
    unitEventBackend,
    pairEventBackend,
    noopEventBackend,

    -- * Primitive 'Event' resource management.

    -- | Prefer the [resource-safe event allocation functions](#g:resourcesafe)
    -- to these when possible.
    finalize,
    newEvent,
  )
where

import Control.Monad.With
import Data.Exceptable
import Data.GeneralAllocate
import Observe.Event.Backend

-- | Allocate a new 'Event', selected by the given selector.
--
-- The selector specifies the category of new event we're creating, as well
-- as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see [DynamicEventSelector](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicEventSelector) for a generic option).
--
-- The 'Event' is automatically 'finalize'd on release.
allocateEvent ::
  (Monad m, Exceptable e) =>
  EventBackend m r s ->
  forall f.
  s f ->
  GeneralAllocate m e () releaseArg (Event m r f)
allocateEvent backend = allocateEventArgs backend . simpleNewEventArgs

-- | Allocate a new 'Event', based on given 'NewEventArgs'.
--
-- The 'Event' is automatically 'finalize'd on release.
allocateEventArgs ::
  (Monad m, Exceptable e) =>
  EventBackend m r s ->
  forall f.
  NewEventArgs r s f ->
  GeneralAllocate m e () releaseArg (Event m r f)
allocateEventArgs backend args = GeneralAllocate $ \restore -> do
  ev <- restore $ newEvent backend args
  let release (ReleaseFailure e) = finalize ev . Just $ toSomeException e
      release (ReleaseSuccess _) = finalize ev Nothing
  pure $ GeneralAllocated ev release

-- | Run an action with a new 'Event', selected by the given selector.
--
-- The selector specifies the category of new event we're creating, as well
-- as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see [DynamicEventSelector](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicEventSelector) for a generic option).
--
-- The 'Event' is automatically 'finalize'd at the end of the function it's passed to.
withEvent ::
  (MonadWithExceptable m) =>
  EventBackend m r s ->
  forall f.
  -- | The event selector.
  s f ->
  (Event m r f -> m a) ->
  m a
withEvent backend = generalWith . allocateEvent backend

-- | Run an action with a new 'Event', based on the given 'NewEventArgs'.
--
-- The 'Event' is automatically 'finalize'd at the end of the function it's passed to.
withEventArgs ::
  (MonadWithExceptable m) =>
  EventBackend m r s ->
  forall f.
  -- | The event selector.
  NewEventArgs r s f ->
  (Event m r f -> m a) ->
  m a
withEventArgs backend = generalWith . allocateEventArgs backend

-- | An 'EventBackend' where every otherwise parentless event will be marked
-- as a child of the given 'Event'.
subEventBackend ::
  (Functor m) =>
  -- | Bring selectors from the new backend into the parent event's backend.
  InjectSelector s t ->
  -- | The parent event.
  Event m r f ->
  EventBackend m r t ->
  EventBackend m r s
subEventBackend inj ev =
  narrowEventBackend inj
    . setAncestorEventBackend (reference ev)

-- | An 'EventBackend' where every otherwise causeless event will be marked
-- as caused by the given 'Event's.
causedEventBackend ::
  (Functor m) =>
  -- | Bring selectors from the new backend into the causing event's backend.
  InjectSelector s t ->
  -- | The causing events.
  [Event m r f] ->
  EventBackend m r t ->
  EventBackend m r s
causedEventBackend inj evs =
  narrowEventBackend inj
    . setInitialCauseEventBackend (map reference evs)
