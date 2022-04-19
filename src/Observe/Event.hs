{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Observe.Event where

import Control.Applicative
import Control.Exception
import Control.Monad.Catch

newtype EventBackend m r s = MkEventBackend
  { newEvent :: forall f . s f -> m (Event m r f)
  }

data Event m r f = MkEvent
  { ref :: !r
  , addField :: !(f -> m ())
  , addReference :: !(Reference r -> m ())
  , finalize :: !(m ())
  , failEvent :: !(Maybe SomeException -> m ())
  }

data Reference r = MkReference
  { referenceType :: !ReferenceType
  , otherRef :: !r
  }

addParent :: Event m r f -> r -> m ()
addParent e = addReference e . MkReference Parent

addProximateCause :: Event m r f -> r -> m ()
addProximateCause e = addReference e . MkReference Proximate

data ReferenceType
  = Parent
  | Proximate

noopEvent :: Applicative m => r -> Event m r f
noopEvent ref = MkEvent
  { ref
  , addField = \_ -> pure ()
  , addReference = \_ -> pure ()
  , finalize = pure ()
  , failEvent = \_ -> pure ()
  }

noopEventBackend :: Applicative m => r -> EventBackend m r s
noopEventBackend ref = MkEventBackend
  { newEvent = \_ -> pure $ noopEvent ref
  }

concatEvent :: Applicative m => Event m a f -> Event m b f -> Event m (a, b) f
concatEvent x y = MkEvent
  { ref = (ref x,  ref y)
  , addField = \f -> addField x f *> addField y f
  , addReference = \(MkReference ty (rx, ry)) -> addReference x (MkReference ty rx) *> addReference y (MkReference ty ry)
  , finalize = finalize x *> finalize y
  , failEvent = \e -> failEvent x e *> failEvent y e
  }

concatEventBackend :: Applicative m => EventBackend m a s -> EventBackend m b s -> EventBackend m (a, b) s
concatEventBackend x y = MkEventBackend
  { newEvent = \sel -> liftA2 concatEvent (newEvent x sel) (newEvent y sel)
  }

hoistEvent :: (Functor m) => (forall x . m x -> n x) -> Event m r f -> Event n r f
hoistEvent nt (MkEvent {..}) = MkEvent
  { ref
  , addField = nt . addField
  , addReference = nt . addReference
  , finalize = nt $ finalize
  , failEvent = nt . failEvent
  }

hoistEventBackend :: (Functor m) => (forall x . m x -> n x) -> EventBackend m r s -> EventBackend n r s
hoistEventBackend nt eb = MkEventBackend
  { newEvent = nt . fmap (hoistEvent nt) . newEvent eb
  }

narrowEvent :: (f -> g) -> Event m r g -> Event m r f
narrowEvent inj (MkEvent {..}) = MkEvent
  { addField = addField . inj
  , ..
  }

narrowEventBackend' :: (Functor m) => (forall f . s f -> forall a . (forall g . t g -> (f -> g) -> a) -> a) -> EventBackend m r t -> EventBackend m r s
narrowEventBackend' inj eb = MkEventBackend
  { newEvent = \sel -> inj sel \sel' inj' -> narrowEvent inj' <$> (newEvent eb sel')
  }

narrowEventBackend :: (forall f . s f -> t f) -> EventBackend m r t -> EventBackend m r s
narrowEventBackend inj eb = MkEventBackend
  { newEvent = newEvent eb . inj
  }

withEvent :: (MonadMask m) => m (Event m r f) -> (Event m r f -> m a) -> m a
withEvent makeEv go = do
    (res, ()) <- generalBracket makeEv release go
    pure res
  where
    release ev (ExitCaseSuccess _) = finalize ev
    release ev (ExitCaseException e) = failEvent ev $ Just e
    release ev ExitCaseAbort = failEvent ev Nothing

withSubEvent :: (MonadMask m) => EventBackend m r s -> r -> s f -> (Event m r f -> m a) -> m a
withSubEvent eb p sel go = withEvent (newEvent eb sel) \ev -> do
  addParent ev p
  go ev
