{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Observe.Event
  ( EventBackend,
    Event,

    -- * Event manipulation #eventmanip#
    reference,
    addField,
    addParent,
    addProximate,
    finalize,
    failEvent,
    newEvent,
    newSubEvent,

    -- * Resource safe #resourcesafe#
    withEvent,
    withSubEvent,
    acquireEvent,
    acquireSubEvent,
    subEventBackend,
    unitEventBackend,
    pairEventBackend,
    hoistEventBackend,
    narrowEventBackend,
    narrowEventBackend',
  )
where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Data.Acquire
import Data.Functor
import Observe.Event.Implementation

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
-- the [resource-safe allocation functions](#g:resourcesafe).
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
-- the [resource-safe allocation functions](#g:resourcesafe).
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
-- Consider the [resource-safe allocation functions](#g:resourcesafe) instead
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
-- Consider the [resource-safe allocation functions](#g:resourcesafe) instead
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
subEventBackend ev@(Event {..}) =
  EventBackend
    { newEventImpl = \sel -> do
        EventImpl {..} <- newEventImpl backend sel
        parentAdded <- newOnceFlag backend
        pure $
          EventImpl
            { addParentImpl = \r -> do
                _ <- checkAndSet parentAdded
                addParentImpl r,
              finalizeImpl = do
                runOnce parentAdded (addParentImpl $ reference ev)
                finalizeImpl,
              failImpl = \e -> do
                runOnce parentAdded (addParentImpl $ reference ev)
                failImpl e,
              ..
            },
      newOnceFlag = newOnceFlag backend
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

-- | An 'EventBackend' which sequentially generates 'Event's in the two given 'EventBackend's.
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
    { newEventImpl = nt . fmap hoistEventImpl . newEventImpl backend,
      newOnceFlag = hoistOnceFlag nt <$> (nt $ newOnceFlag backend)
    }
  where
    hoistEventImpl (EventImpl {..}) =
      EventImpl
        { referenceImpl,
          addFieldImpl = nt . addFieldImpl,
          addParentImpl = nt . addParentImpl,
          addProximateImpl = nt . addProximateImpl,
          finalizeImpl = nt $ finalizeImpl,
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
