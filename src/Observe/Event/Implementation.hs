{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Observe.Event.Implementation where

import Control.Concurrent.MVar
import Control.Exception
import Data.Functor

data EventImpl m r f = EventImpl
  { referenceImpl :: !r,
    addFieldImpl :: !(f -> m ()),
    addParentImpl :: !(r -> m ()),
    addProximateImpl :: !(r -> m ()),
    finalizeImpl :: !(m ()),
    failImpl :: !(Maybe SomeException -> m ())
  }

data FlagState
  = NewlySet
  | AlreadySet

newtype OnceFlag m = OnceFlag
  { checkAndSet :: m FlagState
  }

runOnce :: (Monad m) => OnceFlag m -> m () -> m ()
runOnce f go =
  checkAndSet f >>= \case
    NewlySet -> go
    AlreadySet -> pure ()

newOnceFlagIO :: IO (OnceFlag IO)
newOnceFlagIO = do
  flag <- newEmptyMVar
  pure $
    OnceFlag $
      tryPutMVar flag () <&> \case
        False -> AlreadySet
        True -> NewlySet

hoistOnceFlag :: (forall x. f x -> g x) -> OnceFlag f -> OnceFlag g
hoistOnceFlag nt (OnceFlag cs) = OnceFlag (nt cs)

data EventBackend m r s = EventBackend
  { newEventImpl :: !(forall f. s f -> m (EventImpl m r f)),
    newOnceFlag :: !(m (OnceFlag m))
  }
