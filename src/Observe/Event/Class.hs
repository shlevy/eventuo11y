{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Typeclass for implicit 'EventBackend' passing
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- Core typeclass for an mtl-style handling of 'EventBackend's.
-- See "Observe.Event.Explicit" for an explicit, function-based
-- interface.
module Observe.Event.Class
  ( MonadEvent (..),
    EnvBackend,
    TransEventMonad (..),

    -- * EventT
    EventT (..),
    runEventT,
    eventLift,

    -- * Kind aliases
    EventMonadKind,
    ReferenceKind,
    SelectorKind,
    FunctorKind,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Allocate
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Unlift
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Control
import Control.Monad.With
import Control.Monad.Writer.Class
import Control.Monad.Zip
import Control.Natural.Control
import Data.Coerce
import Data.Functor
import Data.Functor.Contravariant
import Data.GeneralAllocate
import Data.Kind
import Observe.Event.Backend

-- | Monads suitable for 'Event'-based instrumentation, with implicit 'EventBackend' management.
--
-- See "Observe.Event.Explicit" for 'Event'-based instrumentation with explicit 'EventBackend'
-- passing.
--
-- Note that @em@ is an indexed monad of 'EventMonadKind'.
type MonadEvent :: EventMonadKind -> Constraint
class (forall r s. Monad (em r s), Monad (BackendMonad em)) => MonadEvent em where
  -- | The monad of the implicitly carried 'EventBackend'
  type BackendMonad em :: Type -> Type

  -- |
  liftBackendMonad :: BackendMonad em a -> em r s a

  -- | Access the implicitly carried 'EventBackend'
  backend :: em r s (EnvBackend em r s)

  -- | Run an instrumented action with a modified 'EventBackend'
  withModifiedBackend ::
    -- | Modify the 'EventBackend'
    --
    -- Note that the modification may change
    -- the reference and selector types.
    (EnvBackend em r s -> EnvBackend em r' s') ->
    -- | Action to run with the modified backend available.
    em r' s' a ->
    em r s a

-- | The type of the implicit 'EventBackend' of a 'MonadEvent'
type EnvBackend :: EventMonadKind -> ReferenceKind -> SelectorKind -> Type
type EnvBackend em = EventBackend (BackendMonad em)

-- | Make a monad into a 'MonadEvent'.
newtype EventT m r s a = EventT (ReaderT (EventBackend m r s) m a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError e,
      MonadState s',
      MonadWriter w,
      MonadFail,
      MonadFix,
      MonadIO,
      MonadZip,
      Contravariant,
      Alternative,
      MonadPlus,
      MonadCont,
      MonadUnliftIO,
      MonadBase b,
      MonadBaseControl b,
      MonadAllocate,
      MonadCatch,
      MonadThrow,
      MonadMask,
      PrimMonad
    )

-- | Run an 'EventT' with an initial 'EventBackend'.
runEventT :: (Monad m) => EventT m r s a -> EventBackend m r s -> m a
runEventT = coerce

-- | Lift @m@ into 'EventT' @m@.
eventLift :: forall m r s. Applicative m => StatelessControlTransformation m (EventT m r s)
eventLift = statelessControlTransformation $ \useRunInM ->
  coerce @(EventBackend m r s -> _) $ \b -> useRunInM $ flip coerce b

instance MonadReader r m => MonadReader r (EventT m ref s) where
  ask = toNatural eventLift ask
  local modR go = statelessTransWith eventLift $ \runInM ->
    local modR (runInM go)
  reader = toNatural eventLift . reader

instance (MonadWith m) => MonadWith (EventT m r s) where
  type WithException (EventT m r s) = WithException m
  stateThreadingGeneralWith ::
    forall a b releaseReturn.
    GeneralAllocate (EventT m r s) (WithException m) releaseReturn b a ->
    (a -> EventT m r s b) ->
    EventT m r s (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = coerce $ \b -> do
    let allocA' :: (forall x. m x -> m x) -> m (GeneralAllocated m (WithException m) releaseReturn b a)
        allocA' restore = do
          let restore' :: forall x. EventT m r s x -> EventT m r s x
              restore' mx = coerce @(EventBackend m r s -> m x) $ restore . coerce @_ @(EventBackend m r s -> _) mx
          GeneralAllocated a releaseA <- coerce (allocA restore') b
          let releaseA' relTy = coerce @(EventT m r s releaseReturn) @(EventBackend m r s -> m releaseReturn) (releaseA relTy) b
          pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip coerce b . go)

instance (Monad m) => MonadEvent (EventT m) where
  type BackendMonad (EventT m) = m
  liftBackendMonad :: forall a r s. m a -> EventT m r s a
  liftBackendMonad = coerce @(EventBackend m r s -> _) . const

  backend :: forall r s. EventT m r s (EventBackend m r s)
  backend = coerce @(EventBackend m r s -> m _) pure

  withModifiedBackend :: forall r s r' s' a. (EventBackend m r s -> EventBackend m r' s') -> EventT m r' s' a -> EventT m r s a
  withModifiedBackend modBackend e'ma =
    let e'ma' :: EventBackend m r' s' -> m a
        e'ma' = coerce e'ma
     in coerce (e'ma' . modBackend)

-- | Apply a 'MonadTrans'former to an 'EventMonadKind' to get a transformed 'EventMonadKind'
--
-- When @t@ is 'MonadTransControl' and @em@ is 'MonadEvent', 'TransEventMonad' @t@ @em@ is
-- 'MonadEvent' and has all of the relevant instances conferred by @t@.
type TransEventMonad :: (FunctorKind -> FunctorKind) -> EventMonadKind -> EventMonadKind
newtype TransEventMonad t em r s a = TransEventMonad {unTransEventMonad :: t (em r s) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError e,
      MonadState s',
      MonadWriter w,
      MonadFail,
      MonadFix,
      MonadIO,
      MonadZip,
      Contravariant,
      Alternative,
      MonadPlus,
      MonadCont,
      MonadUnliftIO,
      MonadBase b,
      MonadBaseControl b,
      MonadAllocate,
      MonadCatch,
      MonadThrow,
      MonadMask,
      PrimMonad,
      MonadReader r'
    )

instance MonadWith (t (em r s)) => MonadWith (TransEventMonad t em r s) where
  type WithException (TransEventMonad t em r s) = WithException (t (em r s))
  stateThreadingGeneralWith ::
    forall a b releaseReturn.
    GeneralAllocate (TransEventMonad t em r s) (WithException (t (em r s))) releaseReturn b a ->
    (a -> TransEventMonad t em r s b) ->
    TransEventMonad t em r s (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = coerce $ do
    stateThreadingGeneralWith (GeneralAllocate allocA') $ coerce go
    where
      allocA' :: (forall x. t (em r s) x -> t (em r s) x) -> t (em r s) (GeneralAllocated (t (em r s)) (WithException (t (em r s))) releaseReturn b a)
      allocA' restore =
        coerce (allocA restore') <&> \case
          GeneralAllocated a releaseA -> GeneralAllocated a $ coerce @(GeneralReleaseType (WithException (t (em r s))) b -> TransEventMonad t em r s releaseReturn) releaseA
        where
          restore' :: forall x. TransEventMonad t em r s x -> TransEventMonad t em r s x
          restore' = coerce @(t (em r s) x -> t (em r s) x) restore

instance (MonadEvent em, MonadTransControl t, forall r s. Monad (t (em r s))) => MonadEvent (TransEventMonad t em) where
  type BackendMonad (TransEventMonad t em) = BackendMonad em
  liftBackendMonad :: forall a r s. BackendMonad em a -> TransEventMonad t em r s a
  liftBackendMonad = coerce @(_ -> t (em r s) a) $ lift . liftBackendMonad

  backend :: forall r s. TransEventMonad t em r s (EnvBackend em r s)
  backend = coerce @(t (em r s) _) $ lift backend

  withModifiedBackend :: forall r s r' s' a. (EnvBackend em r s -> EnvBackend em r' s') -> TransEventMonad t em r' s' a -> TransEventMonad t em r s a
  withModifiedBackend modBackend go = coerce @(t (em r s) _) $
    controlT @_ @_ @a $ \runInEm ->
      withModifiedBackend modBackend . runInEm $ coerce @_ @(t (em r' s') a) go

-- | The kind of indexed monads aware of 'Event' instrumentation
--
-- See 'MonadEvent'.
type EventMonadKind = ReferenceKind -> SelectorKind -> FunctorKind

-- | The kind of 'Event' references
type ReferenceKind = Type

-- | The kind of 'Event' selectors
type SelectorKind = Type -> Type

-- | The kind of 'Functor's
type FunctorKind = Type -> Type
