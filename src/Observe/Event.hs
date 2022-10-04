{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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
-- (but see t'Observe.Event.Dynamic.DynamicField' for a generic option).
--
-- Instrumentation then centers around 'Event's, populated using the
-- <#g:eventmanip event manipulation functions>. 'Event's are initialized
-- with 'EventBackend's, typically via the
-- <#g:resourcesafe resource-safe event allocation functions>.
--
-- Depending on which 'EventBackend's may end up consuming the 'Event's,
-- instrumentors will also need to define renderers for their selectors
-- and fields. For example, they may need to implement values of types
--  t'Observe.Event.Render.JSON.RenderSelectorJSON' and
--  t'Observe.Event.Render.JSON.RenderFieldJSON' to use JSON rendering 'EventBackend's.
module Observe.Event
  ( Event,
    hoistEvent,

    -- * Event manipulation #eventmanip#
    reference,
    addField,
    addParent,
    addProximate,

    -- * Resource-safe event allocation #resourcesafe#
    withEvent,
    withSubEvent,

    -- ** Acquire/MonadResource variants
    acquireEvent,
    acquireSubEvent,

    -- * 'EventBackend's
    EventBackend,
    subEventBackend,
    causedEventBackend,
    unitEventBackend,
    pairEventBackend,
    hoistEventBackend,
    narrowEventBackend,
    narrowEventBackend',

    -- * Primitive 'Event' resource management.

    -- | Prefer the [resource-safe event allocation functions](#g:resourcesafe)
    -- to these when possible.
    finalize,
    failEvent,
    newEvent,
    newSubEvent,
  )
where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Data.Acquire
import Observe.Event.Backend
import Observe.Event.BackendModification

-- | An instrumentation event.
--
-- 'Event's are the core of the instrumenting user's interface
-- to eventuo11y. Typical usage would be to create an 'Event'
-- from an 'EventBackend' with 'withEvent', or as a child of
-- an another 'Event' with 'withSubEvent', and add fields to
-- the 'Event' at appropriate points in your code with
-- 'addField'.
--
-- [@m@]: The monad we're instrumenting in.
-- [@r@]: The type of event references. See 'reference'.
-- [@s@]: The type of event selectors for child events. See 'EventBackend'.
-- [@f@]: The type of fields on this event. See 'addField'.
data Event m r s f = Event
  { -- | The 'EventBackend' this 'Event' was generated from.
    backend :: !(EventBackend m r s),
    -- | The underlying 'EventImpl' implementing the event functionality.
    impl :: !(EventImpl m r f),
    -- | A 'OnceFlag' to ensure we only finish ('finalize' or 'failEvent') once.
    finishFlag :: !(OnceFlag m)
  }

-- | Hoist an 'Event' along a given natural transformation into a new monad.
hoistEvent :: (Functor m, Functor n) => (forall x. m x -> n x) -> Event m r s f -> Event n r s f
hoistEvent nt Event {..} =
  Event
    { backend = hoistEventBackend nt backend,
      impl = hoistEventImpl nt impl,
      finishFlag = hoistOnceFlag nt finishFlag
    }

-- | Obtain a reference to an 'Event'.
--
-- References are used to link 'Event's together, either in
-- parent-child relationships with 'addParent' or in
-- cause-effect relationships with 'addProximate'.
--
-- References can live past when an event has been 'finalize'd or
-- 'failEvent'ed.
--
-- Code being instrumented should always have @r@ as an unconstrained
-- type parameter, both because it is an implementation concern for
-- 'EventBackend's and because references are backend-specific and it
-- would be an error to reference an event in one backend from an event
-- in a different backend.
reference :: Event m r s f -> r
reference (Event {..}) = referenceImpl impl

-- | Add a field to an 'Event'.
--
-- Fields make up the basic data captured in an event. They should be added
-- to an 'Event' as the code progresses through various phases of work, and can
-- be both milestone markers ("we got this far in the process") or more detailed
-- instrumentation ("we've processed N records").
--
-- They are intended to be of a domain specific type per unit of functionality
-- within an instrumented codebase (but see t'Observe.Event.Dynamic.DynamicField'
-- for a generic option).
addField ::
  Event m r s f ->
  -- | The field to add to the event.
  f ->
  m ()
addField (Event {..}) = addFieldImpl impl

-- | Mark another 'Event' as a parent of this 'Event'.
addParent ::
  Event m r s f ->
  -- | A reference to the parent, obtained via 'reference'.
  r ->
  m ()
addParent (Event {..}) = addParentImpl impl

-- | Mark another 'Event' as a proximate cause of this 'Event'.
addProximate ::
  Event m r s f ->
  -- | A reference to the proximate cause, obtained via 'reference'.
  r ->
  m ()
addProximate (Event {..}) = addProximateImpl impl

-- | Mark an 'Event' as finished.
--
-- In normal usage, this should be automatically called via the use of
-- the [resource-safe event allocation functions](#g:resourcesafe).
--
-- This is a no-op if the 'Event' has already been 'finalize'd or
-- 'failEvent'ed. As a result, it is likely pointless to call
-- 'addField', 'addParent', or 'addProximate' after this call,
-- though it still may be reasonable to call 'reference'.
finalize :: (Monad m) => Event m r s f -> m ()
finalize (Event {..}) = runOnce finishFlag $ finalizeImpl impl

-- | Mark an 'Event' as having failed, possibly due to an 'Exception'.
--
-- In normal usage, this should be automatically called via the use of
-- the [resource-safe event allocation functions](#g:resourcesafe).
--
-- This is a no-op if the 'Event' has already been 'finalize'd or
-- 'failEvent'ed. As a result, it is likely pointless to call
-- 'addField', 'addParent', or 'addProximate' after this call,
-- though it still may be reasonable to call 'reference'.
failEvent :: (Monad m) => Event m r s f -> Maybe SomeException -> m ()
failEvent (Event {..}) = runOnce finishFlag . failImpl impl

-- | Create a new 'Event', selected by the given selector.
--
-- The selector specifies the category of new event we're creating, as well
-- as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see t'Observe.Event.Dynamic.DynamicEventSelector' for a generic option).
--
-- Consider the [resource-safe event allocation functions](#g:resourcesafe) instead
-- of calling this directly.
newEvent ::
  (Applicative m) =>
  EventBackend m r s ->
  forall f.
  -- | The event selector.
  s f ->
  m (Event m r s f)
newEvent backend@(EventBackend {..}) sel = do
  impl <- newEventImpl sel
  finishFlag <- newOnceFlag
  pure Event {..}

-- | Create a new 'Event' as a child of the given 'Event', selected by the given selector.
--
-- The selector specifies the category of new event we're creating, as well
-- as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see t'Observe.Event.Dynamic.DynamicEventSelector' for a generic option).
--
-- Consider the [resource-safe event allocation functions](#g:resourcesafe) instead
-- of calling this directly.
newSubEvent ::
  (Monad m) =>
  -- | The parent event.
  Event m r s f ->
  forall f'.
  -- | The child event selector.
  s f' ->
  m (Event m r s f')
newSubEvent (Event {..}) sel = do
  child <- newEvent backend sel
  addParent child $ referenceImpl impl
  pure child

-- | Run an action with a new 'Event', selected by the given selector.
--
-- The selector specifies the category of new event we're creating, as well
-- as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see t'Observe.Event.Dynamic.DynamicEventSelector' for a generic option).
--
-- The 'Event' is automatically 'finalize'd (or, if appropriate, 'failEvent'ed)
-- at the end of the function it's passed to.
withEvent ::
  (MonadMask m) =>
  EventBackend m r s ->
  forall f.
  -- | The event selector.
  s f ->
  (Event m r s f -> m a) ->
  m a
withEvent backend sel go = do
  (res, ()) <- generalBracket (newEvent backend sel) release go
  pure res
  where
    release ev (ExitCaseSuccess _) = finalize ev
    release ev (ExitCaseException e) = failEvent ev $ Just e
    release ev ExitCaseAbort = failEvent ev Nothing

-- | Run an action with a new 'Event' as a child of the given 'Event', selected by the given selector.
--
-- The selector specifies the category of new event we're creating, as well
-- as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see t'Observe.Event.Dynamic.DynamicEventSelector' for a generic option).
--
-- The 'Event' is automatically 'finalize'd (or, if appropriate, 'failEvent'ed)
-- at the end of the function it's passed to.
withSubEvent ::
  (MonadMask m) =>
  -- | The parent 'Event'.
  Event m r s f ->
  forall f'.
  -- | The child event selector.
  s f' ->
  (Event m r s f' -> m a) ->
  m a
withSubEvent (Event {..}) sel go = withEvent backend sel $ \child -> do
  addParent child $ referenceImpl impl
  go child

-- | An 'Acquire' variant of 'withEvent', usable in a t'Control.Monad.Trans.Resource.MonadResource' with 'allocateAcquire'.
--
-- Until [snoyberg/conduit#460](https://github.com/snoyberg/conduit/issues/460) is addressed, exception
-- information will not be captured.
acquireEvent ::
  (MonadUnliftIO m) =>
  EventBackend m r s ->
  forall f.
  -- | The event selector.
  s f ->
  m (Acquire (Event m r s f))
acquireEvent backend sel = withRunInIO $ \runInIO ->
  pure $
    mkAcquireType
      (runInIO $ newEvent backend sel)
      (release runInIO)
  where
    release runInIO ev ReleaseException = runInIO $ failEvent ev Nothing
    release runInIO ev _ = runInIO $ finalize ev

-- | An 'Acquire' variant of 'withSubEvent', usable in a t'Control.Monad.Trans.Resource.MonadResource' with 'allocateAcquire'.
--
-- Until [snoyberg/conduit#460](https://github.com/snoyberg/conduit/issues/460) is addressed, exception
-- information will not be captured.
acquireSubEvent ::
  (MonadUnliftIO m) =>
  -- | The parent event.
  Event m r s f ->
  forall f'.
  -- | The child event selector.
  s f' ->
  m (Acquire (Event m r s f'))
acquireSubEvent (Event {..}) sel = do
  childAcq <- acquireEvent backend sel
  withRunInIO $ \runInIO -> pure $ do
    child <- childAcq
    liftIO . runInIO . addParent child $ referenceImpl impl
    pure child

-- | An 'EventBackend' where every otherwise parentless event will be marked
-- as a child of the given 'Event'.
subEventBackend ::
  (Monad m) =>
  -- | The parent event.
  Event m r s f ->
  EventBackend m r s
subEventBackend Event {..} = modifyEventBackend (setAncestor $ referenceImpl impl) backend

-- | An 'EventBackend' where every otherwise causeless event will be marked
-- as caused by the given 'Event'.
causedEventBackend ::
  (Monad m) =>
  -- | The parent event.
  Event m r s f ->
  EventBackend m r s
causedEventBackend Event {..} = modifyEventBackend (setInitialCause $ referenceImpl impl) backend
