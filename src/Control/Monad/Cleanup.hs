{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Description : Monads that can cleanup within a single monaadic scope
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
module Control.Monad.Cleanup where

import Control.Exception.Safe
import Control.Monad.Catch (ExitCase (..))
import Control.Monad.ST
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import qualified Control.Monad.Trans.State.Lazy as LazyS
import qualified Control.Monad.Trans.State.Strict as StrictS
import qualified Control.Monad.Trans.Writer.Lazy as LazyW
import qualified Control.Monad.Trans.Writer.Strict as StrictW
import Data.Coerce
import Data.Functor.Identity

-- | Monads that can cleanup within a single monadic scope.
--
-- 'MonadCleanup's allow for acquiring some resource and
-- guaranteeing that it will be released,
-- __if computation might continue within that monad__.
--
-- This is very similar to 'MonadMask', with the following differences:
--
-- 1. 'MonadCleanup's may not be able to throw or catch exceptions (no 'MonadCatch'
--    superclass) or mask exceptions.
-- 2. The guarantee of 'generalCleanup' is not as absolute as 'generalBracket' (though the latter
--    always has the @SIGKILL@/power goes out exception). If we can't handle exceptions
--    at all in the monad (at least not without @unsafePerformIO@), then sometimes the cleanup
--    function won't be called, but only in cases where the entire computation the monad is
--    running is going to be aborted.
--
-- This allows 'MonadCleanup' to be used in pure contexts (see 'CleanupNoException') and still
-- provide meaningful semantics.
class (Monad m) => MonadCleanup m where
  -- | Acquire some resource, use it, and clean it up.
  --
  -- cleanup is guaranteed to run
  -- __if computation in the surrounding monadic scope might continue__.
  --
  -- Similar to 'generalBracket', see documentation of 'MonadCleanup' for the differences.
  generalCleanup ::
    -- | Acquire some resource
    m a ->
    -- | Release the resource, observing the outcome of the inner action
    (a -> ExitCase b -> m c) ->
    -- | Inner action to perform with the resource
    (a -> m b) ->
    m (b, c)

-- | An 'Exception' corresponding to the 'ExitCaseAbort' exit case.
data AbortException = AbortException
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Acquire some resource, use it, and clean it up.
--
-- This is to 'bracketWithError' as 'generalCleanup' is to 'generalBracket', see documentation
-- of 'generalCleanup' for more details.
withCleanup ::
  (MonadCleanup m) =>
  m a ->
  (Maybe SomeException -> a -> m b) ->
  (a -> m c) ->
  m c
withCleanup acquire cleanup go = fst <$> generalCleanup acquire release go
  where
    release x (ExitCaseSuccess _) = cleanup Nothing x
    release x ExitCaseAbort = cleanup (Just (toException AbortException)) x
    release x (ExitCaseException e) = cleanup (Just e) x

-- | A [DerivingVia](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html) helper for deriving 'MonadCleanup' from 'MonadMask'.
newtype CleanupFromMask m a = CleanupFromMask (m a) deriving newtype (Functor, Applicative, Monad)

instance (MonadMask m) => MonadCleanup (CleanupFromMask m) where
  generalCleanup :: forall a b c. CleanupFromMask m a -> (a -> ExitCase b -> CleanupFromMask m c) -> (a -> CleanupFromMask m b) -> CleanupFromMask m (b, c)
  generalCleanup = coerce $ generalBracket @m @a @b @c

deriving via CleanupFromMask IO instance MonadCleanup IO

-- | A [DerivingVia](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html) for deriving 'MonadCleanup' in a 'Monad' which
-- __can't__ handle exceptions.
--
-- If cleanup runs at all, it will run in 'ExitCaseSuccess'
--
-- __Note that the associated 'MonadCleanup' instance is invalid if it is possible to catch exceptions in the monad!__
newtype CleanupNoException m a = CleanupNoException (m a) deriving newtype (Functor, Applicative, Monad)

instance (Monad m) => MonadCleanup (CleanupNoException m) where
  generalCleanup acquire release go = do
    x <- acquire
    res <- go x
    carry <- release x (ExitCaseSuccess res)
    pure (res, carry)

deriving via CleanupNoException (ST s) instance MonadCleanup (ST s)

deriving via CleanupNoException Identity instance MonadCleanup Identity

instance (MonadCleanup m) => MonadCleanup (IdentityT m) where
  generalCleanup acquire release use =
    IdentityT $
      generalCleanup
        (runIdentityT acquire)
        (\resource exitCase -> runIdentityT (release resource exitCase))
        (\resource -> runIdentityT (use resource))

instance MonadCleanup m => MonadCleanup (LazyS.StateT s m) where
  generalCleanup acquire release use = LazyS.StateT $ \s0 -> do
    ((b, _s2), (c, s3)) <-
      generalCleanup
        (LazyS.runStateT acquire s0)
        ( \(resource, s1) exitCase -> case exitCase of
            ExitCaseSuccess (b, s2) -> LazyS.runStateT (release resource (ExitCaseSuccess b)) s2
            -- In the two other cases, the base monad overrides @use@'s state
            -- changes and the state reverts to @s1@.
            ExitCaseException e -> LazyS.runStateT (release resource (ExitCaseException e)) s1
            ExitCaseAbort -> LazyS.runStateT (release resource ExitCaseAbort) s1
        )
        (\(resource, s1) -> LazyS.runStateT (use resource) s1)
    return ((b, c), s3)

instance MonadCleanup m => MonadCleanup (StrictS.StateT s m) where
  generalCleanup acquire release use = StrictS.StateT $ \s0 -> do
    ((b, _s2), (c, s3)) <-
      generalCleanup
        (StrictS.runStateT acquire s0)
        ( \(resource, s1) exitCase -> case exitCase of
            ExitCaseSuccess (b, s2) -> StrictS.runStateT (release resource (ExitCaseSuccess b)) s2
            -- In the two other cases, the base monad overrides @use@'s state
            -- changes and the state reverts to @s1@.
            ExitCaseException e -> StrictS.runStateT (release resource (ExitCaseException e)) s1
            ExitCaseAbort -> StrictS.runStateT (release resource ExitCaseAbort) s1
        )
        (\(resource, s1) -> StrictS.runStateT (use resource) s1)
    return ((b, c), s3)

instance MonadCleanup m => MonadCleanup (ReaderT r m) where
  generalCleanup acquire release use = ReaderT $ \r ->
    generalCleanup
      (runReaderT acquire r)
      (\resource exitCase -> runReaderT (release resource exitCase) r)
      (\resource -> runReaderT (use resource) r)

instance (MonadCleanup m, Monoid w) => MonadCleanup (StrictW.WriterT w m) where
  generalCleanup acquire release use = StrictW.WriterT $ do
    ((b, _w12), (c, w123)) <-
      generalCleanup
        (StrictW.runWriterT acquire)
        ( \(resource, w1) exitCase -> case exitCase of
            ExitCaseSuccess (b, w12) -> do
              (c, w3) <- StrictW.runWriterT (release resource (ExitCaseSuccess b))
              return (c, mappend w12 w3)
            -- In the two other cases, the base monad overrides @use@'s state
            -- changes and the state reverts to @w1@.
            ExitCaseException e -> do
              (c, w3) <- StrictW.runWriterT (release resource (ExitCaseException e))
              return (c, mappend w1 w3)
            ExitCaseAbort -> do
              (c, w3) <- StrictW.runWriterT (release resource ExitCaseAbort)
              return (c, mappend w1 w3)
        )
        ( \(resource, w1) -> do
            (a, w2) <- StrictW.runWriterT (use resource)
            return (a, mappend w1 w2)
        )
    return ((b, c), w123)

instance (MonadCleanup m, Monoid w) => MonadCleanup (LazyW.WriterT w m) where
  generalCleanup acquire release use = LazyW.WriterT $ do
    ((b, _w12), (c, w123)) <-
      generalCleanup
        (LazyW.runWriterT acquire)
        ( \(resource, w1) exitCase -> case exitCase of
            ExitCaseSuccess (b, w12) -> do
              (c, w3) <- LazyW.runWriterT (release resource (ExitCaseSuccess b))
              return (c, mappend w12 w3)
            -- In the two other cases, the base monad overrides @use@'s state
            -- changes and the state reverts to @w1@.
            ExitCaseException e -> do
              (c, w3) <- LazyW.runWriterT (release resource (ExitCaseException e))
              return (c, mappend w1 w3)
            ExitCaseAbort -> do
              (c, w3) <- LazyW.runWriterT (release resource ExitCaseAbort)
              return (c, mappend w1 w3)
        )
        ( \(resource, w1) -> do
            (a, w2) <- LazyW.runWriterT (use resource)
            return (a, mappend w1 w2)
        )
    return ((b, c), w123)

instance (MonadCleanup m, Monoid w) => MonadCleanup (LazyRWS.RWST r w s m) where
  generalCleanup acquire release use = LazyRWS.RWST $ \r s0 -> do
    ((b, _s2, _w12), (c, s3, w123)) <-
      generalCleanup
        (LazyRWS.runRWST acquire r s0)
        ( \(resource, s1, w1) exitCase -> case exitCase of
            ExitCaseSuccess (b, s2, w12) -> do
              (c, s3, w3) <- LazyRWS.runRWST (release resource (ExitCaseSuccess b)) r s2
              return (c, s3, mappend w12 w3)
            -- In the two other cases, the base monad overrides @use@'s state
            -- changes and the state reverts to @s1@ and @w1@.
            ExitCaseException e -> do
              (c, s3, w3) <- LazyRWS.runRWST (release resource (ExitCaseException e)) r s1
              return (c, s3, mappend w1 w3)
            ExitCaseAbort -> do
              (c, s3, w3) <- LazyRWS.runRWST (release resource ExitCaseAbort) r s1
              return (c, s3, mappend w1 w3)
        )
        ( \(resource, s1, w1) -> do
            (a, s2, w2) <- LazyRWS.runRWST (use resource) r s1
            return (a, s2, mappend w1 w2)
        )
    return ((b, c), s3, w123)

instance (MonadCleanup m, Monoid w) => MonadCleanup (StrictRWS.RWST r w s m) where
  generalCleanup acquire release use = StrictRWS.RWST $ \r s0 -> do
    ((b, _s2, _w12), (c, s3, w123)) <-
      generalCleanup
        (StrictRWS.runRWST acquire r s0)
        ( \(resource, s1, w1) exitCase -> case exitCase of
            ExitCaseSuccess (b, s2, w12) -> do
              (c, s3, w3) <- StrictRWS.runRWST (release resource (ExitCaseSuccess b)) r s2
              return (c, s3, mappend w12 w3)
            -- In the two other cases, the base monad overrides @use@'s state
            -- changes and the state reverts to @s1@ and @w1@.
            ExitCaseException e -> do
              (c, s3, w3) <- StrictRWS.runRWST (release resource (ExitCaseException e)) r s1
              return (c, s3, mappend w1 w3)
            ExitCaseAbort -> do
              (c, s3, w3) <- StrictRWS.runRWST (release resource ExitCaseAbort) r s1
              return (c, s3, mappend w1 w3)
        )
        ( \(resource, s1, w1) -> do
            (a, s2, w2) <- StrictRWS.runRWST (use resource) r s1
            return (a, s2, mappend w1 w2)
        )
    return ((b, c), s3, w123)

instance MonadCleanup m => MonadCleanup (MaybeT m) where
  generalCleanup acquire release use = MaybeT $ do
    (eb, ec) <-
      generalCleanup
        (runMaybeT acquire)
        ( \resourceMay exitCase -> case resourceMay of
            Nothing -> return Nothing -- nothing to release, acquire didn't succeed
            Just resource -> case exitCase of
              ExitCaseSuccess (Just b) -> runMaybeT (release resource (ExitCaseSuccess b))
              ExitCaseException e -> runMaybeT (release resource (ExitCaseException e))
              _ -> runMaybeT (release resource ExitCaseAbort)
        )
        ( \resourceMay -> case resourceMay of
            Nothing -> return Nothing
            Just resource -> runMaybeT (use resource)
        )
    -- The order in which we perform those two 'Maybe' effects doesn't matter,
    -- since the error message is the same regardless.
    return ((,) <$> eb <*> ec)

instance MonadCleanup m => MonadCleanup (ExceptT e m) where
  generalCleanup acquire release use = ExceptT $ do
    (eb, ec) <-
      generalCleanup
        (runExceptT acquire)
        ( \eresource exitCase -> case eresource of
            Left e -> return (Left e) -- nothing to release, acquire didn't succeed
            Right resource -> case exitCase of
              ExitCaseSuccess (Right b) -> runExceptT (release resource (ExitCaseSuccess b))
              ExitCaseException e -> runExceptT (release resource (ExitCaseException e))
              _ -> runExceptT (release resource ExitCaseAbort)
        )
        (either (return . Left) (runExceptT . use))
    return $ do
      -- The order in which we perform those two 'Either' effects determines
      -- which error will win if they are both 'Left's. We want the error from
      -- 'release' to win.
      c <- ec
      b <- eb
      return (b, c)
