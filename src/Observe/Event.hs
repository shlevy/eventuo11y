{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Core interface for instrumentation with eventuo11y
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- This is the primary module needed to instrument code with eventuo11y.
--
-- Instrumentors should first define selector types and field types
-- appropriate to the unit of code they're instrumenting:
--
-- Selectors are values which designate the general category of event
-- being created, parameterized by the type of fields that can be added to it.
-- For example, a web service's selector type may have a @ServicingRequest@
-- constructor, whose field type includes a @ResponseCode@ constructor which
-- records the HTTP status code. Selectors are intended to be of a domain-specific
-- type per unit of functionality within an instrumented codebase, implemented as a GADT
-- (but see t'Observe.Event.Dynamic.DynamicEventSelector' for a generic option).
--
-- Fields make up the basic data captured in an event. They should be added
-- to an 'Event' as the code progresses through various phases of work, and can
-- be both milestone markers ("we got this far in the process") or more detailed
-- instrumentation ("we've processed N records"). They are intended to be of a
-- domain-specific type per unit of functionality within an instrumented codebase
-- (but see [DynamicField](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicField) for a generic option).
--
-- Instrumentation then centers around 'Event's, populated using the
-- <#g:eventmanip event manipulation functions>. 'Event's are initialized
-- with 'EventBackend's, typically via the
-- <#g:resourcesafe resource-safe event allocation functions>.
--
-- Depending on which 'EventBackend's may end up consuming the 'Event's,
-- instrumentors will also need to define renderers for their selectors
-- and fields. For example, they may need to implement values of types
-- [RenderSelectorJSON](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Render-JSON.html#t:RenderSelectorJSON)
-- to use JSON rendering 'EventBackend's.
module Observe.Event
  ( Event,
    hoistEvent,

    -- * Event manipulation #eventmanip#
    addField,
    reference,
    addParent,
    addProximate,
    addReference,
    Reference (..),
    ReferenceType (..),

    -- * Resource-safe event allocation #resourcesafe#
    allocateEvent,
    withEvent,
    withSubEvent,

    -- * 'EventBackend's
    EventBackend,

    -- ** Backend transformation
    subEventBackend,
    causedEventBackend,
    hoistEventBackend,
    narrowEventBackend,
    setDefaultReferenceEventBackend,
    setAncestorEventBackend,
    setInitialCauseEventBackend,
    setReferenceEventBackend,
    setParentEventBackend,
    setProximateEventBackend,
    narrowEventBackend',

    -- ** Backend composition
    unitEventBackend,
    pairEventBackend,
    noopEventBackend,

    -- * Primitive 'Event' resource management.

    -- | Prefer the [resource-safe event allocation functions](#g:resourcesafe)
    -- to these when possible.
    finalize,
    newEvent,
    newSubEvent,
  )
where

import Control.Monad.Primitive
import Control.Monad.With
import Data.Exceptable
import Data.GeneralAllocate
import Observe.Event.Backend

-- | Mark another 'Event' as a parent of this 'Event'.
addParent ::
  Event m r f ->
  r ->
  m ()
addParent ev = addReference ev . Reference Parent

-- | Mark another 'Event' as a proximate cause of this 'Event'.
addProximate ::
  Event m r f ->
  r ->
  m ()
addProximate ev = addReference ev . Reference Proximate

-- | Create a new 'Event' as a child of the given 'Event', selected by the given selector.
--
-- The selector specifies the category of new event we're creating, as well
-- as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see [DynamicEventSelector](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicEventSelector) for a generic option).
--
-- Consider the [resource-safe event allocation functions](#g:resourcesafe) instead
-- of calling this directly.
newSubEvent ::
  (Monad m) =>
  EventBackend m r s ->
  -- | The parent event.
  Event m r f ->
  forall f'.
  -- | The child event selector.
  s f' ->
  m (Event m r f')
newSubEvent backend ev sel = do
  child <- newEvent backend sel
  addParent child $ reference ev
  pure child

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
allocateEvent backend sel = GeneralAllocate $ \restore -> do
  ev <- restore $ newEvent backend sel
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

-- | Run an action with a new 'Event' as a child of the given 'Event', selected by the given selector.
--
-- The selector specifies the category of new event we're creating, as well
-- as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see [DynamicEventSelector](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicEventSelector) for a generic option).
--
-- The 'Event' is automatically 'finalize'd at the end of the function it's passed to.
withSubEvent ::
  (MonadWithExceptable m) =>
  EventBackend m r s ->
  -- | The parent 'Event'.
  Event m r f ->
  forall f'.
  -- | The child event selector.
  s f' ->
  (Event m r f' -> m a) ->
  m a
withSubEvent backend ev sel go = withEvent backend sel $ \child -> do
  addParent child $ reference ev
  go child

-- | An 'EventBackend' where every otherwise parentless event will be marked
-- as a child of the given 'Event'.
subEventBackend ::
  (PrimMonad m) =>
  -- | Bring selectors from the new backend into the parent event's backend.
  --
  -- See 'setAncestorEventBackend' and 'narrowEventBackend'' if you need a more
  -- general mapping between selector types.
  (forall f'. s f' -> t f') ->
  -- | The parent event.
  Event m r f ->
  EventBackend m r t ->
  EventBackend m r s
subEventBackend inj ev =
  narrowEventBackend inj
    . setAncestorEventBackend (reference ev)

-- | An 'EventBackend' where every otherwise causeless event will be marked
-- as caused by the given 'Event'.
causedEventBackend ::
  (PrimMonad m) =>
  -- | Bring selectors from the new backend into the causing event's backend.
  --
  -- See 'setInitialCauseEventBackend' and 'narrowEventBackend'' if you need a more
  -- general mapping between selector types.
  (forall f'. s f' -> t f') ->
  -- | The causing event.
  Event m r f ->
  EventBackend m r t ->
  EventBackend m r s
causedEventBackend inj ev =
  narrowEventBackend inj
    . setInitialCauseEventBackend (reference ev)
