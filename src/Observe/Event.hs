{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- with 'MonadEvent' functions, typically via the
-- <#g:resourcesafe resource-safe event allocation functions>. For an
-- explicit alternative to 'MonadEvent', see "Observe.Event.Explicit".
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

    -- * MonadEvent
    MonadEvent,
    EnvEvent,

    -- ** Resource-safe event allocation #resourcesafe#
    NewEventArgs (..),
    emitImmediateEvent',
    withEvent,
    withEventArgs,
    withNarrowingEvent,
    withNarrowingEventArgs,
    InjectSelector,
    injectSelector,
    idInjectSelector,
    MonadWithEvent,
    allocateEvent,
    allocateEventArgs,

    -- ** EventT
    EventT,
    runEventT,
    eventLift,

    -- ** TransEventMonad
    TransEventMonad (..),

    -- ** Primitives
    BackendMonad,
    EnvBackend,
    EventBackend,
    liftBackendMonad,
    backend,
    withModifiedBackend,

    -- * Primitive 'Event' resource management.

    -- | Prefer the [resource-safe event allocation functions](#g:resourcesafe)
    -- to these when possible.
    finalize,
    newEvent',
    newEventArgs,

    -- * Backend Events

    -- | 'Event's within the 'BackendMonad' of a 'MonadEvent'
    --
    -- These are low-level primitives that can be used if the
    -- existing higher-level event allocation/backend modification
    -- combinators are insufficient
    BackendEvent,
    hoistBackendEvent,
    allocateBackendEvent,
    withBackendEvent,
    newBackendEvent,
  )
where

import Control.Monad.With
import Data.Exceptable
import Data.GeneralAllocate
import Data.Kind
import Observe.Event.Backend
import Observe.Event.Class
import qualified Observe.Event.Explicit as Explicit

-- | An 'Event' in a 'MonadEvent'
type EnvEvent :: EventMonadKind -> ReferenceKind -> SelectorKind -> Type -> Type
type EnvEvent em r s = Event (em r s) r

-- | Create an event which has no duration and is immediately finalized successfully.
--
-- Returns a reference to the event.
emitImmediateEvent' :: (MonadEvent em) => NewEventArgs r s f -> em r s r
emitImmediateEvent' args = do
  b <- backend
  liftBackendMonad $ emitImmediateEvent b args

-- | Run an action with a new 'Event', selected by the given selector.
--
-- The selector specifies the category of new event we're creating, as well
-- as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see [DynamicEventSelector](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicEventSelector) for a generic option).
--
-- Within the nested action, all new parentless 'Event's will be
-- made children of the new 'Event'.
--
-- The 'Event' will be 'finalize'd at the end of the nested action.
withEvent ::
  (MonadWithEvent em) =>
  forall f.
  s f ->
  (EnvEvent em r s f -> em r s a) ->
  em r s a
withEvent = withNarrowingEvent idInjectSelector

-- | Run an action with a new 'Event', specified by the given 'NewEventArgs'
--
-- Within the nested action, all new parentless 'Event's will be
-- made children of the new 'Event'.
--
-- The 'Event' will be 'finalize'd at the end of the nested action.
withEventArgs ::
  (MonadWithEvent em) =>
  forall f.
  NewEventArgs r s f ->
  (EnvEvent em r s f -> em r s a) ->
  em r s a
withEventArgs = withNarrowingEventArgs idInjectSelector

-- | Run an action with a new 'Event' , selected by a given selector, with a narrower sub-selector type.
--
-- The selector specifies the category of new event we're creating, as well
-- as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see [DynamicEventSelector](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicEventSelector) for a generic option).
--
-- Within the nested action, all new parentless 'Event's will be
-- made children of the new 'Event', and all new 'Event's will
-- be selected by the narrower selector type.
--
-- The 'Event' will be 'finalize'd at the end of the nested action.
withNarrowingEvent ::
  (MonadWithEvent em) =>
  InjectSelector s t ->
  forall f.
  t f ->
  (EnvEvent em r s f -> em r s x) ->
  em r t x
withNarrowingEvent inj = withNarrowingEventArgs inj . simpleNewEventArgs

-- | Run an action with a new 'Event' , specified by the given 'NewEventArgs', with a narrower sub-selector type.
--
-- Within the nested action, all new parentless 'Event's will be
-- made children of the new 'Event', and all new 'Event's will
-- be selected by the narrower selector type.
--
-- The 'Event' will be 'finalize'd at the end of the nested action.
withNarrowingEventArgs ::
  (MonadWithEvent em) =>
  InjectSelector s t ->
  forall f.
  NewEventArgs r t f ->
  (EnvEvent em r s f -> em r s x) ->
  em r t x
withNarrowingEventArgs inj args go = withBackendEvent args $ \ev -> do
  let ev' = hoistBackendEvent ev
  withModifiedBackend (narrowEventBackend inj . setAncestorEventBackend (reference ev)) $ go ev'

-- | A 'MonadEvent' suitable for running the 'withEvent' family of functions
class (MonadEvent em, forall r s. MonadWithExceptable (em r s)) => MonadWithEvent em

instance (MonadEvent em, forall r s. MonadWithExceptable (em r s)) => MonadWithEvent em

-- | Allocate a new 'Event', selected by the given selector.
--
-- The selector specifies the category of new event we're creating, as well
-- as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see [DynamicEventSelector](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicEventSelector) for a generic option).
--
-- The 'Event' will be automatically 'finalize'd on release.
allocateEvent ::
  (MonadEvent em, Exceptable e) =>
  forall f.
  s f ->
  GeneralAllocate (em r s) e () releaseArg (EnvEvent em r s f)
allocateEvent = allocateEventArgs . simpleNewEventArgs

-- | Allocate a new 'Event', specified by the given 'NewEventArgs'.
--
-- The 'Event' will be automatically 'finalize'd on release.
allocateEventArgs ::
  (MonadEvent em, Exceptable e) =>
  forall f.
  NewEventArgs r s f ->
  GeneralAllocate (em r s) e () releaseArg (EnvEvent em r s f)
allocateEventArgs = fmap hoistBackendEvent . allocateBackendEvent

-- | Create a new 'Event', selected by the given selector.
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
newEvent' :: (MonadEvent em) => forall f. s f -> em r s (EnvEvent em r s f)
newEvent' = newEventArgs . simpleNewEventArgs

-- | Create a new 'Event', specified by the given 'NewEventArgs'.
--
-- Consider the [resource-safe event allocation functions](#g:resourcesafe) instead
-- of calling this directly.
newEventArgs :: (MonadEvent em) => forall f. NewEventArgs r s f -> em r s (EnvEvent em r s f)
newEventArgs = fmap hoistBackendEvent . newBackendEvent

-- | An 'Event' in the 'BackendMonad' of a 'MonadEvent'
type BackendEvent :: EventMonadKind -> ReferenceKind -> Type -> Type
type BackendEvent em = Event (BackendMonad em)

-- | Bring a 'BackendEvent' into the 'MonadEvent'
hoistBackendEvent :: (MonadEvent em) => BackendEvent em r f -> EnvEvent em r s f
hoistBackendEvent = hoistEvent liftBackendMonad

-- | A 'BackendMonad' variant of 'allocateEventArgs'.
allocateBackendEvent ::
  (MonadEvent em, Exceptable e) =>
  forall f.
  NewEventArgs r s f ->
  GeneralAllocate (em r s) e () releaseArg (BackendEvent em r f)
allocateBackendEvent args = GeneralAllocate $ \_ -> do
  ev <- newBackendEvent args
  let release (ReleaseFailure e) = liftBackendMonad . finalize ev . Just $ toSomeException e
      release (ReleaseSuccess _) = liftBackendMonad $ finalize ev Nothing
  pure $ GeneralAllocated ev release

-- | Run an action with a new 'BackendEvent'.
--
-- The 'Event' will be 'finalize'd upon completion.
withBackendEvent ::
  (MonadEvent em, MonadWithExceptable (em r s)) =>
  forall f.
  NewEventArgs r s f ->
  (BackendEvent em r f -> em r s a) ->
  em r s a
withBackendEvent = generalWith . allocateBackendEvent

-- | A 'BackendMonad' variant of 'newEventArgs'
newBackendEvent :: (MonadEvent em) => forall f. NewEventArgs r s f -> em r s (BackendEvent em r f)
newBackendEvent args = do
  b <- backend
  liftBackendMonad $ Explicit.newEvent b args
