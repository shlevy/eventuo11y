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
    NewEventArgs (..),
    simpleNewEventArgs,

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
    setAncestorEventBackend,
    setInitialCauseEventBackend,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad.Zip
import Data.Functor

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
    -- References are used to link 'Event's together, via the 'newEventParent'
    -- and 'newEventCauses' fields of 'NewEventArgs'.
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
    -- | Mark an 'Event' as finished, perhaps due to an 'Exception'.
    --
    -- In normal usage, this should be automatically called via the use of
    -- the [resource-safe event allocation functions](Observe-Event.html#g:resourcesafe).
    --
    -- This is a no-op if the 'Event' has already been 'finalize'd.
    -- As a result, it is likely pointless to call
    -- 'addField' after this call, though it still may be reasonable to call
    -- 'reference'.
    finalize :: !(Maybe SomeException -> m ())
  }

-- | Hoist an 'Event' along a given natural transformation into a new monad.
hoistEvent :: (forall x. m x -> n x) -> Event m r f -> Event n r f
hoistEvent nt ev =
  ev
    { addField = nt . addField ev,
      finalize = nt . finalize ev
    }

-- | A backend for creating t'Event's.
--
-- Different 'EventBackend's will be used to emit instrumentation to
-- different systems. Multiple backends can be combined with
-- 'Observe.Event.pairEventBackend'.
--
-- A simple 'EventBackend' for logging to a t'System.IO.Handle' can be
-- created with [jsonHandleBackend](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Render-JSON-Handle.html#v:jsonHandleBackend).
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
-- Implementations must ensure that 'EventBackend's and their underlying t'Event's
-- are safe to use across threads.
--
-- [@m@]: The monad we're instrumenting in.
-- [@r@]: The type of event references used in this 'EventBackend'. See 'reference'.
-- [@s@]: The type of event selectors. See 'newEventSelector'.
data EventBackend m r s = EventBackend
  { -- | Create a new 'Event', specified by the given arguments.
    --
    -- Consider the [resource-safe event allocation functions](Observe-Event.html#g:resourcesafe) instead
    -- of calling this directly.
    newEvent :: forall f. NewEventArgs r s f -> m (Event m r f),
    -- | Create an event which has no duration and is immediately finalized
    -- successfully.
    --
    -- Returns a reference to the event.
    emitImmediateEvent :: forall f. NewEventArgs r s f -> m r
  }

-- | Arguments specifying how an 'Event' should be created.
--
-- See 'simpleNewEventArgs' for a simple case.
data NewEventArgs r s f = NewEventArgs
  { -- | The selector specifying the category of new 'Event' we're creating,
    -- as well as the type of fields that can be added to it (with 'addField').
    --
    -- Selectors are intended to be of a domain specific type per unit of
    -- functionality within an instrumented codebase, implemented as a GADT
    -- (but see [DynamicEventSelector](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicEventSelector) for a generic option).
    newEventSelector :: !(s f),
    -- | The parent of the new 'Event', if any.
    --
    -- Typically handled automatically via v'Observe.Event.withEvent'.
    newEventParent :: !(Maybe r),
    -- | The proximate causes of the new 'Event', if any.
    newEventCauses :: ![r],
    -- | Fields set at the creation of the 'Event'.
    --
    -- See 'addField'.
    newEventInitialFields :: ![f]
  }

-- | 'NewEventArgs' from a given selector, with no initial fields or explicit references.
--
-- The selector specifies the category of new 'Event' we're creating,
-- as well as the type of fields that can be added to it (with 'addField').
--
-- Selectors are intended to be of a domain specific type per unit of
-- functionality within an instrumented codebase, implemented as a GADT
-- (but see [DynamicEventSelector](https://hackage.haskell.org/package/eventuo11y-json/docs/Observe-Event-Dynamic.html#t:DynamicEventSelector) for a generic option).
simpleNewEventArgs :: s f -> NewEventArgs r s f
simpleNewEventArgs sel =
  NewEventArgs
    { newEventSelector = sel,
      newEventParent = Nothing,
      newEventCauses = [],
      newEventInitialFields = []
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
    { newEvent = \args -> do
        let (xArgs, yArgs) = unzipArgs args
        xEv <- newEvent x xArgs
        yEv <- newEvent y yArgs
        pure $
          Event
            { reference = (reference xEv, reference yEv),
              addField = \f -> addField xEv f *> addField yEv f,
              finalize = \me -> finalize xEv me *> finalize yEv me
            },
      emitImmediateEvent = \args -> do
        let (xArgs, yArgs) = unzipArgs args
        xRef <- emitImmediateEvent x xArgs
        yRef <- emitImmediateEvent y yArgs
        pure $ (xRef, yRef)
    }
  where
    unzipArgs args =
      ( args
          { newEventParent = xParent,
            newEventCauses = xCauses
          },
        args
          { newEventParent = yParent,
            newEventCauses = yCauses
          }
      )
      where
        (xParent, yParent) = munzip $ newEventParent args
        (xCauses, yCauses) = munzip $ newEventCauses args

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
              finalize = const $ pure ()
            },
      emitImmediateEvent = \_ -> pure r
    }

-- | Hoist an 'EventBackend' along a given natural transformation into a new monad.
hoistEventBackend ::
  (Functor m) =>
  (forall x. m x -> n x) ->
  EventBackend m r s ->
  EventBackend n r s
hoistEventBackend nt backend =
  EventBackend
    { newEvent = nt . fmap (hoistEvent nt) . newEvent backend,
      emitImmediateEvent = nt . emitImmediateEvent backend
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
    { newEvent = \args -> inj (newEventSelector args) \sel' injField ->
        newEvent backend (transformArgs args sel' injField) <&> \ev ->
          ev
            { addField = addField ev . injField
            },
      emitImmediateEvent = \args -> inj (newEventSelector args) \sel' injField ->
        emitImmediateEvent backend $ transformArgs args sel' injField
    }
  where
    transformArgs args sel' injField =
      args
        { newEventSelector = sel',
          newEventInitialFields = injField <$> newEventInitialFields args
        }

-- | Transform an 'EventBackend' so all of its 'Event's have a given parent, if they
-- are not given another parent.
setAncestorEventBackend :: r -> EventBackend m r s -> EventBackend m r s
setAncestorEventBackend parent backend =
  EventBackend
    { newEvent = newEvent backend . transformArgs,
      emitImmediateEvent = emitImmediateEvent backend . transformArgs
    }
  where
    transformArgs args =
      args
        { newEventParent = newEventParent args <|> pure parent
        }

-- | Transform an 'EventBackend' so all of its 'Event's have the given causes,
-- if they are not given another set of causes.
setInitialCauseEventBackend :: [r] -> EventBackend m r s -> EventBackend m r s
setInitialCauseEventBackend causes backend =
  EventBackend
    { newEvent = newEvent backend . transformArgs,
      emitImmediateEvent = emitImmediateEvent backend . transformArgs
    }
  where
    transformArgs args =
      args
        { newEventCauses = case newEventCauses args of
            [] -> causes
            l -> l
        }
