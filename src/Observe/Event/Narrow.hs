{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Observe.Event.Narrow where

import Data.Kind

import Observe.Event.Core

type NarrowSpec :: EventSpec -> EventSpec -> Type
data NarrowSpec wide narrow = MkNarrowSpec
  { narrowField :: !(Field narrow -> Field wide)
  , narrowSub ::
      !( forall narrow'
         . SubEventSelector narrow narrow'
        -> forall a
         . (forall wide' . NarrowSpec wide' narrow' -> SubEventSelector wide wide' -> a)
        -> a
       )
  }

narrowEvent :: (Functor m) => NarrowSpec wide narrow -> Event m r wide -> Event m r narrow
narrowEvent (MkNarrowSpec {..}) e = MkEvent
  { ref = ref e
  , modify = modify e . fmap (mapEventModification id narrowField)
  , subEvent = \sel -> narrowSub sel \subNarrow sel' ->
      narrowEvent subNarrow <$> subEvent e sel'
  }
