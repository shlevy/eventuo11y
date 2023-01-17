{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- |
-- Description : EventBackend for rendering events as Haskell values
-- Copyright   : Copyright 2023 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
module Observe.Event.Render.InMemory
  ( listInMemoryBackend,
    timelessListInMemoryBackend,
    inMemoryBackend,

    -- * MemoryEvent
    MemoryEvent (..),
    TimedEventAction (..),
    EventAction (..),

    -- * Effects

    -- ** InMemoryEffects
    InMemoryEffects (..),
    hoistInMemoryEffects,
    listInMemoryEffects,
    timelessListInMemoryEffects,

    -- ** AppendVectorEffects
    AppendVectorEffects (..),
    hoistAppendVectorEffects,
    ListAppendVector (..),
    listAppendVectorEffects,

    -- ** TimestampEffects
    TimestampEffects (..),
    hoistTimestampEffects,
    dummyTimestampEffects,
    ioTimestampEffects,
  )
where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Foldable
import Data.Kind
import Data.Primitive.MutVar
import Data.Time.Clock
import Observe.Event.Backend

-- | An 'EventBackend' whose 'Event's are essentially plain Haskell values.
--
-- The 'reference' of an 'Event' from this 'EventBackend' will be a 'MemoryEvent',
-- which can be examined to extract information about the 'Event'.
listInMemoryBackend :: (PrimMonad m, MonadIO m) => EventBackend m (MemoryEvent m (ListAppendVector m) UTCTime s) s
listInMemoryBackend = inMemoryBackend listInMemoryEffects

-- | An 'EventBackend' whose 'Event's are essentially plain Haskell values.
--
-- The 'reference' of an 'Event' from this 'EventBackend' will be a 'MemoryEvent',
-- which can be examined to extract information about the 'Event'.
timelessListInMemoryBackend :: (PrimMonad m) => EventBackend m (MemoryEvent m (ListAppendVector m) () s) s
timelessListInMemoryBackend = inMemoryBackend timelessListInMemoryEffects

-- | An 'EventBackend' whose 'Event's are essentially plain Haskell values.
--
-- The 'reference' of an 'Event' from this 'EventBackend' will be a 'MemoryEvent',
-- which can be examined to extract information about the 'Event'.
--
-- [@appvec@]: An append-only vector type, see 'AppendVectorEffects'
-- [@ts@]: A timestamp, see 'TimestampEffects'
inMemoryBackend :: (Monad m) => InMemoryEffects m appvec ts -> EventBackend m (MemoryEvent m appvec ts s) s
inMemoryBackend InMemoryEffects {..} =
  EventBackend
    { newEvent = \initArgs -> do
        start <- getTimestamp
        dynamicValues <- newVector
        childEvents <- newVector
        causedEvents <- newVector
        let reference = MemoryEvent {dynamicValues = Just dynamicValues, ..}
        traverse_ (\(MemoryEvent {childEvents = cevs}) -> appendVector cevs reference) $
          newEventParent initArgs
        traverse_ (\(MemoryEvent {causedEvents = cevs}) -> appendVector cevs reference) $
          newEventCauses initArgs
        pure $
          Event
            { addField = \f -> do
                when <- getTimestamp
                appendVector dynamicValues $ TimedEventAction {act = AddField f, ..},
              finalize = \me -> do
                when <- getTimestamp
                appendVector dynamicValues $ TimedEventAction {act = Finalize me, ..},
              ..
            },
      emitImmediateEvent = \initArgs -> do
        start <- getTimestamp
        let dynamicValues = Nothing
        childEvents <- newVector
        causedEvents <- newVector
        pure $ MemoryEvent {..}
    }
  where
    AppendVectorEffects {..} = appendVectorEffects
    TimestampEffects {..} = timestampEffects

-- | An plain data type representing an 'Event'
--
-- [@appvec@]: An append-only vector type, see 'AppendVectorEffects'
-- [@ts@]: A timestamp, see 'TimestampEffects'
type MemoryEvent :: (Type -> Type) -> (Type -> Type) -> Type -> (Type -> Type) -> Type
data MemoryEvent m appvec ts s = forall f.
  MemoryEvent
  { -- | The arguments the 'Event' was initialized with
    initArgs :: !(NewEventArgs (MemoryEvent m appvec ts s) s f),
    -- | The start time of the 'Event'
    start :: !ts,
    -- | Event information added during the event's lifecycle
    --
    -- 'Nothing' if this was the result of 'emitImmediateEvent' and thus had no lifecycle
    dynamicValues :: !(Maybe (appvec (TimedEventAction ts f))),
    -- | Direct children of this event
    childEvents :: !(appvec (MemoryEvent m appvec ts s)),
    -- | Events directly caused by this event
    causedEvents :: !(appvec (MemoryEvent m appvec ts s))
  }

-- | An action that occurred during an 'Event' at some time.
--
-- [@ts@]: A timestamp, see 'TimestampEffects'
data TimedEventAction ts f = TimedEventAction
  { -- | When the event occurred
    when :: !ts,
    -- | The action that occurred
    act :: !(EventAction f)
  }

-- | An action on an 'Event'
data EventAction f
  = -- | A field was added with 'addField'
    AddField !f
  | -- | The 'Event' was finalized with 'finalize'
    Finalize !(Maybe SomeException)

-- | Monadic effects needed to construct an 'inMemoryBackend'
--
-- [@appvec@]: An append-only vector type, see 'AppendVectorEffects'
-- [@ts@]: A timestamp, see 'TimestampEffects'
data InMemoryEffects m appvec ts = InMemoryEffects
  { -- | Manipulate append-only vectors
    appendVectorEffects :: !(AppendVectorEffects m appvec),
    -- | Get timestamps
    timestampEffects :: !(TimestampEffects m ts)
  }

-- | Hoist 'InMemoryEffects' along a given natural transformation into a new monad
hoistInMemoryEffects :: (forall x. m x -> n x) -> InMemoryEffects m appvec ts -> InMemoryEffects n appvec ts
hoistInMemoryEffects nt (InMemoryEffects {..}) =
  InMemoryEffects
    { appendVectorEffects = hoistAppendVectorEffects nt appendVectorEffects,
      timestampEffects = hoistTimestampEffects nt timestampEffects
    }

-- | 'InMemoryEffects' based on 'listAppendVectorEffects' and 'ioTimestampEffects'.
listInMemoryEffects :: (PrimMonad m, MonadIO m) => InMemoryEffects m (ListAppendVector m) UTCTime
listInMemoryEffects =
  InMemoryEffects
    { appendVectorEffects = listAppendVectorEffects,
      timestampEffects = hoistTimestampEffects liftIO ioTimestampEffects
    }

-- | 'InMemoryEffects' based on 'listAppendVectorEffects' with meaningless timestamps.
timelessListInMemoryEffects :: (PrimMonad m) => InMemoryEffects m (ListAppendVector m) ()
timelessListInMemoryEffects =
  InMemoryEffects
    { appendVectorEffects = listAppendVectorEffects,
      timestampEffects = dummyTimestampEffects
    }

-- | Monadic effects to manipulate append-only vectors.
data AppendVectorEffects m appvec = AppendVectorEffects
  { -- | Create a new vector
    newVector :: !(forall a. m (appvec a)),
    -- | Append a value to a vector
    appendVector :: !(forall a. appvec a -> a -> m ())
  }

-- | Hoist 'AppendVectorEffects' along a given natural transformation into a new monad
hoistAppendVectorEffects :: (forall x. m x -> n x) -> AppendVectorEffects m appvec -> AppendVectorEffects n appvec
hoistAppendVectorEffects nt (AppendVectorEffects {..}) =
  AppendVectorEffects
    { newVector = nt $ newVector,
      appendVector = \v -> nt . appendVector v
    }

-- | An append-only vector in some 'PrimMonad' based on lists.
newtype ListAppendVector m a = ListAppendVector (MutVar (PrimState m) [a])

-- | 'AppendVectorEffects' in some 'PrimMonad' based on lists.
listAppendVectorEffects :: (PrimMonad m) => AppendVectorEffects m (ListAppendVector m)
listAppendVectorEffects =
  AppendVectorEffects
    { newVector = ListAppendVector <$> newMutVar [],
      appendVector = \(ListAppendVector v) x -> atomicModifyMutVar v (\l -> (x : l, ()))
    }

-- | Monadic effects to manage timestamps.
newtype TimestampEffects m ts = TimestampEffects
  { getTimestamp :: m ts
  }

-- | Hoist 'TimestampEffects' along a given natural transformation into a new monad
hoistTimestampEffects :: (forall x. m x -> n x) -> TimestampEffects m ts -> TimestampEffects n ts
hoistTimestampEffects nt (TimestampEffects {..}) = TimestampEffects $ nt getTimestamp

-- | 'TimestampEffects' with meaningless timestamps.
dummyTimestampEffects :: (Applicative m) => TimestampEffects m ()
dummyTimestampEffects = TimestampEffects $ pure ()

-- | 'TimestampEffects' using the system clock
ioTimestampEffects :: TimestampEffects IO UTCTime
ioTimestampEffects = TimestampEffects getCurrentTime
