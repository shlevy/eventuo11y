{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- |
-- Description : EventBackend for rendering events as Prometheus metrics
-- Copyright   : Copyright 2023 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
module Observe.Event.Render.Prometheus where

import Control.Monad.IO.Class
import Data.Kind
import Observe.Event.Backend
import System.Metrics.Prometheus.Concurrent.Registry

-- | An 'EventBackend' that populates a 'Registry'
prometheusEventBackend :: (MonadIO m) => Registry -> RenderSelectorPrometheus s -> m (EventBackend m PrometheusReference s)
prometheusEventBackend registry = \case {}

-- | Render @s@ to prometheus metrics
type RenderSelectorPrometheus :: (Type -> Type) -> Type
data RenderSelectorPrometheus s

-- | Reference type for 'prometheusEventBackend'
--
-- Prometheus can't make use of references, so this carries no information.
data PrometheusReference = PrometheusReference
