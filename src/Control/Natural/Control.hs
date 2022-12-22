{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description : Natural transformations that can lift control operations
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- Natural transformations can lift monadic actions from the source to the
-- target, but something stronger is needed to lift general higher-order control
-- operations like @catch@.
--
-- This module relates to [Control.Natural](https://hackage.haskell.org/package/natural-transformation/docs/Control-Natural.html)
-- in a similar way to how [Control.Monad.Trans.Control](https://hackage.haskell.org/package/monad-control/docs/Control-Monad-Trans-Control.html)
-- relates to "Control.Monad.Trans.Class".
module Control.Natural.Control where

import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Identity

-- | A transformation from @m@ to @n@ that can lift control operations.
--
-- The @st@ functor is needed to track the higher monad's state
-- in the lower monad. See 'StatelessControlTransformation' for the case
-- where no state tracking is needed.
data ControlTransformation st m n = ControlTransformation
  { -- | Lift an action in @m@, defined in a context where @n@ actions
    -- can be lowered into @m@ with state tracking, into @n@.
    transWith :: !(forall a. ((forall x. n x -> Compose m st x) -> m a) -> n a),
    -- | Restore the state captured by 'transWith'
    restoreState :: !(forall a. st a -> n a)
  }

-- | Extract a natural transformation from a 'ControlTransformation'
toNatural :: ControlTransformation st m n -> (forall x. m x -> n x)
toNatural ct mx = transWith ct $ const mx

-- | A transformation from @m@ to @n@ that can lift control operations.
--
-- This type is only appropriate for the case where the higher monad
-- does not have any additional state that must be accounted for when
-- running within the lower monad. For the more general case, see
-- 'ControlTransformation'.
--
-- I'm told this is a right kan extension.
type StatelessControlTransformation = ControlTransformation Identity

-- | Create a 'StatelessControlTransformation'
statelessControlTransformation ::
  forall m n.
  (Functor m, Applicative n) =>
  -- | Lift an action in @m@, defined in a context where @n@
  -- actions can be lowered into @m@, into @n@.
  (forall a. ((forall x. n x -> m x) -> m a) -> n a) ->
  StatelessControlTransformation m n
statelessControlTransformation transWith' = ControlTransformation {..}
  where
    transWith :: forall a. ((forall x. n x -> Compose m Identity x) -> m a) -> n a
    transWith useRunInM = transWith' $ \runInM -> useRunInM (Compose . fmap Identity . runInM)
    restoreState :: forall a. Identity a -> n a
    restoreState = pure . coerce

-- | Lift an action in @m@, defined in a context where @n@
-- actions can be lowered into @m@, into @n@.
statelessTransWith ::
  (Functor m) =>
  StatelessControlTransformation m n ->
  (((forall x. n x -> m x) -> m a) -> n a)
statelessTransWith (ControlTransformation {..}) useRunInM = transWith $ \runInM -> useRunInM (fmap runIdentity . getCompose . runInM)
