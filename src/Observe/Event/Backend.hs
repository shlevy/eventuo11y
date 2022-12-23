{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Description : Interface for implementing EventBackends
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- This is the primary module needed to write new 'EventBackend's.
module Observe.Event.Backend
  ( -- * Core interface
    EventBackend (..),
    Event (..),
    Reference (..),
    ReferenceType (..),

    -- * Backend composition
    unitEventBackend,
    pairEventBackend,
    noopEventBackend,

    -- * Backend transformation
    hoistEventBackend,
    hoistEvent,
    InjectSelector,
    injectSelector,
    idInjectSelector,
    narrowEventBackend,
    setDefaultReferenceEventBackend,
    setAncestorEventBackend,
    setInitialCauseEventBackend,
    setReferenceEventBackend,
    setParentEventBackend,
    setProximateEventBackend,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Data.Functor
import Data.Primitive.MVar

-- | An instrumentation event.
--
-- 'Event's are the core of the instrumenting user's interface
-- to eventuo11y. Typical usage would be to create an 'Event'
-- using v'Observe.Event.withEvent' and add fields to the 'Event' at appropriate
-- points in your code with 'addField'.
--
-- [@m@]: The monad we're instrumenting in.
-- [@r@]: The type of event references. See 'reference'.
-- [@f@]: The type of fields on this event. See 'addField'.
data Event m r f = Event
  { -- | Obtain a reference to an 'Event'.
    --
    -- References are used to link 'Event's together, via 'addReference'.
    --
    -- References can live past when an event has been 'finalize'd.
    --
    -- Code being instrumented should always have @r@ as an unconstrained
    -- type parameter, both because it is an implementation concern for
    -- 'EventBackend's and because references are backend-specific and it
    -- would be an error to reference an event in one backend from an event
    -- in a different backend.
    reference :: !r,
    -- | Add a field to an 'Event'.
    --
    -- Fields make up the basic data captured in an event. They should be added
    -- to an 'Event' as the code progresses through various phases of work, and can
    -- be both milestone markers ("we got this far in the process") or more detailed
    -- instrumentation ("we've processed N records").
    --
    -- They are intended to be of a domain specific type per unit of functionality
    -- within an instrumented codebase (but see [DynamicField](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicField)
    -- for a generic option).
    addField :: !(f -> m ()),
    -- | Relate another 'Event' to this 'Event' in the specified way
    addReference :: !(Reference r -> m ()),
    -- | Mark an 'Event' as finished, perhaps due to an 'Exception'.
    --
    -- In normal usage, this should be automatically called via the use of
    -- the [resource-safe event allocation functions](Observe-Event.html#g:resourcesafe).
    --
    -- This is a no-op if the 'Event' has already been 'finalize'd.
    -- As a result, it is likely pointless to call
    -- 'addField' or 'addReference' (or v'Observe.Event.addParent' / v'Observe.Event.addProximate')
    -- after this call, though it still may be reasonable to call 'reference'.
    finalize :: !(Maybe SomeException -> m ())
  }

-- | Hoist an 'Event' along a given natural transformation into a new monad.
hoistEvent :: (forall x. m x -> n x) -> Event m r f -> Event n r f
hoistEvent nt ev =
  ev
    { addField = nt . addField ev,
      addReference = nt . addReference ev,
      finalize = nt . finalize ev
    }

-- | Ways in which 'Event's can 'Reference' each other.
data ReferenceType
  = -- | The 'Reference'd 'Event' is a parent of this 'Event'.
    Parent
  | -- | The 'Reference'd 'Event' is a proximate cause of this 'Event'.
    Proximate
  deriving stock (Eq)

-- | A reference to another 'Event'
data Reference r = Reference !ReferenceType !r

-- | A backend for creating t'Event's.
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
-- (but see [DynamicEventSelector](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicEventSelector) for a generic option).
--
-- Implementations must ensure that 'EventBackend's and their underlying t'Observe.Event.Event's
-- are safe to use across threads.
--
-- [@m@]: The monad we're instrumenting in.
-- [@r@]: The type of event references used in this 'EventBackend'. See 'Observe.Event.reference'.
-- [@s@]: The type of event selectors. See 'newEvent'.
newtype EventBackend m r s = EventBackend
  { -- | Create a new 'Event', selected by the given selector.
    --
    -- The selector specifies the category of new event we're creating, as well
    -- as the type of fields that can be added to it (with 'addField').
    --
    -- Selectors are intended to be of a domain specific type per unit of
    -- functionality within an instrumented codebase, implemented as a GADT
    -- (but see [DynamicEventSelector](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicEventSelector) for a generic option).
    --
    -- Consider the [resource-safe event allocation functions](Observe-Event.html#g:resourcesafe) instead
    -- of calling this directly.
    newEvent ::
      forall f.
      -- The event selector.
      s f ->
      m (Event m r f)
  }

-- | A no-op 'EventBackend'.
--
-- This can be used if calling instrumented code from an un-instrumented
-- context, or to purposefully ignore instrumentation from some call.
--
-- 'unitEventBackend' is the algebraic unit of 'pairEventBackend'.
unitEventBackend :: Applicative m => EventBackend m () s
unitEventBackend = noopEventBackend ()

-- | An 'EventBackend' which sequentially generates 'Observe.Event.Event's in the two given 'EventBackend's.
--
-- This can be used to emit instrumentation in multiple ways (e.g. logs to grafana and metrics on
-- a prometheus HTML page).
pairEventBackend :: Applicative m => EventBackend m a s -> EventBackend m b s -> EventBackend m (a, b) s
pairEventBackend x y =
  EventBackend
    { newEvent = \sel -> do
        xEv <- newEvent x sel
        yEv <- newEvent y sel
        pure $
          Event
            { reference = (reference xEv, reference yEv),
              addField = \f -> addField xEv f *> addField yEv f,
              addReference = \(Reference ty (rx, ry)) ->
                addReference xEv (Reference ty rx) *> addReference yEv (Reference ty ry),
              finalize = \me -> finalize xEv me *> finalize yEv me
            }
    }

-- | A no-op 'EventBackend' that can be integrated with other backends.
--
-- This can be used to purposefully ignore instrumentation from some call.
--
-- All events will have the given reference, so can be connected to appropriate
-- events in non-no-op backends, but not in a way that can distinguish between
-- different events from the same no-op backend.
noopEventBackend :: Applicative m => r -> EventBackend m r s
noopEventBackend r =
  EventBackend
    { newEvent = \_ ->
        pure $
          Event
            { reference = r,
              addField = const $ pure (),
              addReference = const $ pure (),
              finalize = const $ pure ()
            }
    }

-- | Hoist an 'EventBackend' along a given natural transformation into a new monad.
hoistEventBackend ::
  (Functor m) =>
  (forall x. m x -> n x) ->
  EventBackend m r s ->
  EventBackend n r s
hoistEventBackend nt backend =
  EventBackend
    { newEvent = nt . fmap (hoistEvent nt) . newEvent backend
    }

-- | Inject a narrower selector and its fields into a wider selector.
--
-- See 'injectSelector' for a simple way to construct one of these.
type InjectSelector s t = forall f. s f -> forall a. (forall g. t g -> (f -> g) -> a) -> a

-- | Construct an 'InjectSelector' with a straightforward injection from @s@ to @t@
injectSelector :: (forall f. s f -> t f) -> InjectSelector s t
injectSelector inj sel withInjField = withInjField (inj sel) id

-- | The identity 'InjectSelector'
idInjectSelector :: InjectSelector s s
idInjectSelector s go = go s id

-- | Narrow an 'EventBackend' to a new selector type via a given injection function.
--
-- A typical usage, where component A calls component B, would be to have A's selector
-- type have a constructor to take any value of B's selector type (and preserve the field)
-- and then call 'narrowEventBackend' with that constructor when invoking functions in B.
narrowEventBackend ::
  (Functor m) =>
  InjectSelector s t ->
  EventBackend m r t ->
  EventBackend m r s
narrowEventBackend inj backend =
  EventBackend
    { newEvent = \sel -> inj sel \sel' injField ->
        newEvent backend sel' <&> \ev ->
          ev
            { addField = addField ev . injField
            }
    }

-- | Transform an 'EventBackend' so all of its 'Event's have a given 'Reference'.
--
-- You likely want 'setDefaultReferenceEventBackend', if your monad supports it.
setReferenceEventBackend :: (Monad m) => Reference r -> EventBackend m r s -> EventBackend m r s
setReferenceEventBackend r backend =
  EventBackend
    { newEvent = \sel -> do
        ev <- newEvent backend sel
        addReference ev r
        pure ev
    }

-- | Transform an 'EventBackend' so all of its 'Event's have a given parent.
--
-- You likely want 'setAncestorEventBackend', if your monad supports it.
setParentEventBackend :: (Monad m) => r -> EventBackend m r s -> EventBackend m r s
setParentEventBackend = setReferenceEventBackend . Reference Parent

-- | Transform an 'EventBackend' so all of its 'Event's have a given proximate cause.
--
-- You likely want 'setInitialCauseEventBackend', if your monad supports it.
setProximateEventBackend :: (Monad m) => r -> EventBackend m r s -> EventBackend m r s
setProximateEventBackend = setReferenceEventBackend . Reference Proximate

-- | Transform an 'EventBackend' so all of its 'Event's have a given 'Reference', if they
-- haven't been given a 'Reference' of the same 'ReferenceType' by the time they are 'finalize'd.
--
-- See 'setReferenceEventBackend' if the 'Reference' should be applied unconditionally.
setDefaultReferenceEventBackend :: (PrimMonad m) => Reference r -> EventBackend m r s -> EventBackend m r s
setDefaultReferenceEventBackend ref@(Reference ty _) backend =
  EventBackend
    { newEvent = \sel -> do
        flag <- newEmptyMVar
        ev <- newEvent backend sel
        pure $
          ev
            { addReference = \ref'@(Reference ty' _) -> do
                when (ty' == ty) . void $ tryPutMVar flag ()
                addReference ev ref',
              finalize = \me -> do
                tryPutMVar flag () >>= \case
                  False -> pure ()
                  True -> addReference ev ref
                finalize ev me
            }
    }

-- | Transform an 'EventBackend' so all of its 'Event's have a given parent, if they
-- are not given another parent by the time they are 'finalize'd.
--
-- See 'setParentEventBackend' if the parent should be set unconditionally.
setAncestorEventBackend :: (PrimMonad m) => r -> EventBackend m r s -> EventBackend m r s
setAncestorEventBackend = setDefaultReferenceEventBackend . Reference Parent

-- | Transform an 'EventBackend' so all of its 'Event's have a given proximate cause,
-- if they are not given another proximate cause by the time they are 'finalize'd.
--
-- See 'setProximateEventBackend' if the proximate cause should be set unconditionally.
setInitialCauseEventBackend :: (PrimMonad m) => r -> EventBackend m r s -> EventBackend m r s
setInitialCauseEventBackend = setDefaultReferenceEventBackend . Reference Proximate
