{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : EventBackend for rendering events as Prometheus metrics
-- Copyright   : Copyright 2023 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
module Observe.Event.Render.Prometheus where

import Control.Monad.IO.Class
import Data.Map
import Data.Traversable
import Observe.Event.Backend
import System.Metrics.Prometheus.Concurrent.Registry
import System.Metrics.Prometheus.Metric.Histogram (UpperBound)
import System.Metrics.Prometheus.MetricId

-- | An 'EventBackend' that populates a 'Registry'.
--
-- All metrics are registered before the backend is returned.
prometheusEventBackend :: forall m es s. (MonadIO m, EventMetrics es) => Registry -> RenderSelectorPrometheus s es -> m (EventBackend m PrometheusReference s)
prometheusEventBackend registry render = do
  _counters <- fmap fromAscList . for [minBound @(Counter es) ..] $ \cId -> do
    c <- liftIO $ registerCounter (metricName cId) (metricLabels cId) registry
    pure (cId, c)
  _gauges <- fmap fromAscList . for [minBound @(Gauge es) ..] $ \gId -> do
    g <- liftIO $ registerGauge (metricName gId) (metricLabels gId) registry
    pure (gId, g)
  _histograms <- fmap fromAscList . for [minBound @(Histogram es) ..] $ \hId -> do
    h <- liftIO $ registerHistogram (metricName hId) (metricLabels hId) (metricBounds hId) registry
    pure (hId, h)
  pure $
    EventBackend
      { newEvent = \(NewEventArgs {..}) -> case render newEventSelector of {},
        emitImmediateEvent = \(NewEventArgs {..}) -> case render newEventSelector of {}
      }

-- | Render all events selectable by @s@ to prometheus metrics according to 'EventMetrics' @es@
type RenderSelectorPrometheus s es = forall f. s f -> PrometheusRendered f es

-- | How to render a specific 'Event' according to 'EventMetrics' @es@
data PrometheusRendered f es

-- | A specification of a collection of prometheus metrics.
--
-- Note that due to limitations in the underlying prometheus client library, summaries are not yet supported.
class (EventMetric (Counter es), EventMetric (Gauge es), EventHistogram (Histogram es)) => EventMetrics es where
  -- | The [counters](https://prometheus.io/docs/concepts/metric_types/#counter)
  type Counter es

  -- | The [gauges](https://prometheus.io/docs/concepts/metric_types/#gauge)
  type Gauge es

  -- | The [histograms](https://prometheus.io/docs/concepts/metric_types/#histogram)
  type Histogram es

-- | A specification of a single prometheus metric of any type
--
-- Must satisfy @∀ x : a, x \`elem\` [minBound .. maxBound]@
class (Ord a, Enum a, Bounded a) => EventMetric a where
  -- | The [name](https://prometheus.io/docs/practices/naming/#metric-names) of the metric
  metricName :: a -> Name

  -- | The [labels](https://prometheus.io/docs/practices/naming/#labels) of the metric
  metricLabels :: a -> Labels

-- | A specification of a prometheus [histogram](https://prometheus.io/docs/concepts/metric_types/#histogram)
class (EventMetric h) => EventHistogram h where
  -- | The upper bounds of the histogram buckets.
  metricBounds :: h -> [UpperBound]

-- | Reference type for 'prometheusEventBackend'
--
-- Prometheus can't make use of references, so this carries no information.
data PrometheusReference = PrometheusReference
