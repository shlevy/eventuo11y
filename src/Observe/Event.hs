{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
module Observe.Event
  ( EventBackend

  , Event
  , reference
  , addField
  , addParent
  , addProximate
  , finalize
  , failEvent

  , newEvent
  , newSubEvent

  , withEvent
  , withSubEvent

  , acquireEvent
  , acquireSubEvent

  , subEventBackend
  , unitEventBackend
  , pairEventBackend
  , hoistEventBackend
  , narrowEventBackend'
  , narrowEventBackend
  ) where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Data.Acquire
import Data.Functor

import Observe.Event.Implementation

data Event m r s f = Event
  { backend :: !(EventBackend m r s)
  , impl :: !(EventImpl m r f)
  , finishFlag :: !(OnceFlag m)
  }

reference :: Event m r s f -> r
reference (Event {..}) = referenceImpl impl

addField :: Event m r s f -> f -> m ()
addField (Event {..}) = addFieldImpl impl

addParent :: Event m r s f -> r -> m ()
addParent (Event {..}) = addParentImpl impl

addProximate :: Event m r s f -> r -> m ()
addProximate (Event {..}) = addProximateImpl impl

finalize :: (Monad m) => Event m r s f -> m ()
finalize (Event {..}) = runOnce finishFlag $ finalizeImpl impl

failEvent :: (Monad m) => Event m r s f -> Maybe SomeException -> m ()
failEvent (Event {..}) = runOnce finishFlag . failImpl impl

newEvent :: (Applicative m) => EventBackend m r s -> forall f . s f -> m (Event m r s f)
newEvent backend@(EventBackend {..}) sel = do
  impl <- newEventImpl sel
  finishFlag <- newOnceFlag
  pure Event {..}

newSubEvent :: (Monad m) => Event m r s f -> forall f' . s f' -> m (Event m r s f')
newSubEvent (Event {..}) sel = do
  child <- newEvent backend sel
  addParent child $ referenceImpl impl
  pure child

withEvent :: (MonadMask m) => EventBackend m r s -> forall f . s f -> (Event m r s f -> m a) -> m a
withEvent backend sel go = do
    (res, ()) <- generalBracket (newEvent backend sel) release go
    pure res
  where
    release ev (ExitCaseSuccess _) = finalize ev
    release ev (ExitCaseException e) = failEvent ev $ Just e
    release ev ExitCaseAbort = failEvent ev Nothing

withSubEvent :: (MonadMask m) => Event m r s f -> forall f' . s f' -> (Event m r s f' -> m a) -> m a
withSubEvent (Event {..}) sel go = withEvent backend sel $ \child -> do
  addParent child $ referenceImpl impl
  go child

-- No exception logging pending https://github.com/snoyberg/conduit/issues/460
acquireEvent :: (MonadUnliftIO m)
             => EventBackend m r s
             -> forall f . s f
             -> m (Acquire (Event m r s f))
acquireEvent backend sel = withRunInIO $ \runInIO ->
    pure $ mkAcquireType
      (runInIO $ newEvent backend sel)
      (release runInIO)
  where
    release runInIO ev ReleaseException = runInIO $ failEvent ev Nothing
    release runInIO ev _ = runInIO $ finalize ev

acquireSubEvent :: (MonadUnliftIO m)
                => Event m r s f
                -> forall f' . s f'
                -> m (Acquire (Event m r s f'))
acquireSubEvent (Event {..}) sel = do
  childAcq <- acquireEvent backend sel
  withRunInIO $ \runInIO -> pure $ do
    child <- childAcq
    liftIO . runInIO . addParent child $ referenceImpl impl
    pure child

subEventBackend :: (Monad m) => Event m r s f -> EventBackend m r s
subEventBackend ev@(Event {..}) = EventBackend
  { newEventImpl = \sel -> do
      EventImpl {..} <- newEventImpl backend sel
      parentAdded <- newOnceFlag backend
      pure $ EventImpl
        { addParentImpl = \r -> do
            _ <- checkAndSet parentAdded
            addParentImpl r
        , finalizeImpl = do
            runOnce parentAdded (addParentImpl $ reference ev)
            finalizeImpl
        , failImpl = \e -> do
            runOnce parentAdded (addParentImpl $ reference ev)
            failImpl e
        , ..
        }
  , newOnceFlag = newOnceFlag backend
  }

unitEventBackend :: Applicative m => EventBackend m () s
unitEventBackend = EventBackend
  { newEventImpl = \_ -> pure $ EventImpl
    { referenceImpl = ()
    , addFieldImpl = const $ pure ()
    , addParentImpl = const $ pure ()
    , addProximateImpl = const $ pure ()
    , finalizeImpl = pure ()
    , failImpl = const $ pure ()
    }
  , newOnceFlag = pure . OnceFlag $ pure NewlySet
  }

pairEventBackend :: Applicative m => EventBackend m a s -> EventBackend m b s -> EventBackend m (a, b) s
pairEventBackend x y = EventBackend
  { newEventImpl = \sel -> do
      xImpl <- newEventImpl x sel
      yImpl <- newEventImpl y sel
      pure $ EventImpl
        { referenceImpl = (referenceImpl xImpl, referenceImpl yImpl)
        , addFieldImpl = \f -> addFieldImpl xImpl f *> addFieldImpl yImpl f
        , addParentImpl = \(px, py) -> addParentImpl xImpl px *> addParentImpl yImpl py
        , addProximateImpl = \(px, py) -> addProximateImpl xImpl px *> addProximateImpl yImpl py
        , finalizeImpl = finalizeImpl xImpl *> finalizeImpl yImpl
        , failImpl = \e -> failImpl xImpl e *> failImpl yImpl e
        }
  , newOnceFlag = do
      xOnce <- newOnceFlag x
      yOnce <- newOnceFlag y
      pure $ OnceFlag $ do
        xSet <- checkAndSet xOnce
        ySet <- checkAndSet yOnce
        pure $ case (xSet, ySet) of
          (NewlySet, NewlySet) -> NewlySet
          _ -> AlreadySet
  }

hoistEventBackend :: (Functor m, Functor n) => (forall x . m x -> n x) -> EventBackend m r s -> EventBackend n r s
hoistEventBackend nt backend = EventBackend
    { newEventImpl = nt . fmap hoistEventImpl . newEventImpl backend
    , newOnceFlag = hoistOnceFlag nt <$> (nt $ newOnceFlag backend)
    }
  where
    hoistEventImpl (EventImpl {..}) = EventImpl
      { referenceImpl
      , addFieldImpl = nt . addFieldImpl
      , addParentImpl = nt . addParentImpl
      , addProximateImpl = nt . addProximateImpl
      , finalizeImpl = nt $ finalizeImpl
      , failImpl = nt . failImpl
      }

narrowEventBackend' :: (Functor m)
                    => (forall f . s f -> forall a . (forall g . t g -> (f -> g) -> a) -> a)
                    -> EventBackend m r t
                    -> EventBackend m r s
narrowEventBackend' inj backend = EventBackend
  { newEventImpl = \sel -> inj sel \sel' injField -> newEventImpl backend sel' <&> \case
      EventImpl {..} -> EventImpl
        { addFieldImpl = addFieldImpl . injField
        , ..
        }
  , newOnceFlag = newOnceFlag backend
  }

narrowEventBackend :: (Functor m)
                   => (forall f . s f -> t f)
                   -> EventBackend m r t
                   -> EventBackend m r s
narrowEventBackend inj = narrowEventBackend'
  (\sel withInjField -> withInjField (inj sel) id)
