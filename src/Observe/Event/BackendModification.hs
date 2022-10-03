{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Description : Domain-specific language for modifying the behavior of EventBackends
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- A domain-specific language for modifying the behavior of t'Observe.Event.EventBackend's, needed when
-- the caller can't specify the t'Observe.Event.EventBackend' to use directly.
--
-- = The instrumented capability problem
--
-- A common approach for polymorphic effect management in Haskell is the "capability pattern",
-- where a function polymorphic in some monad @m@ takes as an argument a value also polymorphic
-- in @m@ that can be used to run a constrained set of effects in @m@. One example of such a
-- "capability" type would be t'Observe.Event.EventBackend', which for example enables running
-- 'Observe.Event.newEvent' in whatever @m@ it's instantiated in. These capabilities are often
-- themselves implemented in terms of other capabilities, and are ultimately concretely
-- instantiated in some base monad (typically `IO`, or perhaps t`Control.Monad.ST.ST` for a pure
-- mock) and then @hoist@ed to the application's monadic context (e.g. 'Observe.Event.hoistEventBackend').
--
-- Normally this compose + hoist approach works fine, since any capabilities that are dependencies of the
-- the capability we're hoisting are hidden in its closure. But if a capability depends on an `EventBackend`
-- for instrumentation, closing over it at creation time causes a problem: at the call-site of the various
-- effects enabled by the capability, we have no way to modify the t'Observe.Event.EventBackend' to e.g. be a noop (because
-- we don't need the details of this effect's actions to instrument the calling function effectively) or to
-- have its t'Observe.Event.Event's descend from some current 'Observe.Event.Event'. Thus, the DSL defined
-- in this module: effects which take some polymorphic capability can *also* take an 'EventBackendModifier'
-- and the capability can modify its captured t'Observe.Event.EventBackend' with 'modifyEventBackend' accordingly.
--
-- An alternative would be to have each effect in the capability take an t'Observe.Event.EventBackend' at the call site.
-- This would foreclose @hoist@ing along an arbitrary natural transformation, since the t'Observe.Event.EventBackend' would
-- be in negative position, but constrained @hoist@ing might be possible with @MonadUnliftIO@ or @MonadUnlift@
-- or @MonadBaseControl@ if we share a base monad, or if we implemented t'Observe.Event.EventBackend's in a separate base monad
-- that appears in the type of our capabilities and ensure it's liftable to both our application monad and the
-- capability's base instantiation.
module Observe.Event.BackendModification
  ( EventBackendModifier (..),
    EventBackendModifiers,
    modifyEventBackend,

    -- * Simple EventBackendModifiers
    unmodified,
    silence,
    setAncestor,
    setInitialCause,
  )
where

import Control.Category
import Observe.Event.Backend
import Prelude hiding (id, (.))

-- | Modify an t'Observe.Event.EventBackend', chaging its reference type from @r@ to @r'@
data EventBackendModifier r r' where
  -- | Ignore all instrumentation using the t'Observe.Event.EventBackend'
  Silence :: forall r. EventBackendModifier r ()
  -- | Mark every parentless event as the child of a known t'Observe.Event.Event'.
  SetAncestor ::
    forall r.
    -- | A 'Observe.Event.reference' to the parent t'Observe.Event.Event'.
    r ->
    EventBackendModifier r r
  -- | Mark every causeless event as proximately caused by a known t'Observe.Event.Event'.
  SetInitialCause ::
    forall r.
    -- | A 'Observe.Event.reference' to the causing t'Observe.Event.Event'.
    r ->
    EventBackendModifier r r

-- | A sequence of 'EventBackendModifier's
--
-- The free 'Category' over 'EventBackendModifier'
data EventBackendModifiers r r' where
  Nil :: forall r. EventBackendModifiers r r
  Cons :: forall r r' r''. EventBackendModifier r' r'' -> EventBackendModifiers r r' -> EventBackendModifiers r r''

instance Category EventBackendModifiers where
  id = Nil
  Nil . f = f
  (Cons hd tl) . f = Cons hd (tl . f)

-- | Modify an t'Observe.Event.EventBackend' according to the given 'EventBackendModifiers'.
--
-- This is a right fold, e.g. @modifyEventBackend (a . b . id) backend@ first
-- modifies @backend@ with @b@ and then modifies the result with @a@.
modifyEventBackend :: Monad m => EventBackendModifiers r r' -> EventBackend m r s -> EventBackend m r' s
modifyEventBackend Nil backend = backend
modifyEventBackend (Cons Silence _) _ = unitEventBackend
modifyEventBackend (Cons (SetAncestor parent) rest) backend' =
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
                runOnce parentAdded (addParentImpl parent)
                finalizeImpl,
              failImpl = \e -> do
                runOnce parentAdded (addParentImpl parent)
                failImpl e,
              ..
            },
      newOnceFlag = newOnceFlag backend
    }
  where
    backend = modifyEventBackend rest backend'
modifyEventBackend (Cons (SetInitialCause proximate) rest) backend' =
  EventBackend
    { newEventImpl = \sel -> do
        EventImpl {..} <- newEventImpl backend sel
        proximateAdded <- newOnceFlag backend
        pure $
          EventImpl
            { addProximateImpl = \r -> do
                _ <- checkAndSet proximateAdded
                addParentImpl r,
              finalizeImpl = do
                runOnce proximateAdded (addProximateImpl proximate)
                finalizeImpl,
              failImpl = \e -> do
                runOnce proximateAdded (addProximateImpl proximate)
                failImpl e,
              ..
            },
      newOnceFlag = newOnceFlag backend
    }
  where
    backend = modifyEventBackend rest backend'

-- | A single-element 'EventBackendModifiers'
singleton :: EventBackendModifier r r' -> EventBackendModifiers r r'
singleton = flip Cons Nil

-- | An 'EventBackendModifiers' that does nothing.
unmodified :: EventBackendModifiers r r
unmodified = id

-- | An 'EventBackendModifiers' that silences events.
silence :: EventBackendModifiers r ()
silence = singleton Silence

-- | An 'EventBackendModifiers' that marks every parentless event as the child
-- of a known t'Observe.Event.Event'.
setAncestor :: r -> EventBackendModifiers r r
setAncestor = singleton . SetAncestor

-- | An 'EventBackendModifiers' that marks every causeless event as proximately caused
-- by a known t'Observe.Event.Event'.
setInitialCause :: r -> EventBackendModifiers r r
setInitialCause = singleton . SetInitialCause
