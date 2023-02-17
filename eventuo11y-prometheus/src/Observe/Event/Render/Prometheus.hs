{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Exception
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
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
  let performModification :: MetricModification es -> m ()
      performModification = \case {}
      performModifications = traverse_ performModification
  pure $
    EventBackend
      { newEvent = \(NewEventArgs {..}) -> do
          let PrometheusRendered {..} = render newEventSelector
          performModifications $ onStart newEventInitialFields Extended
          fieldsRef <- liftIO $ newIORef []
          pure $
            Event
              { reference = PrometheusReference,
                addField = \f -> do
                  performModifications $ onField f
                  liftIO . atomicModifyIORef' fieldsRef $ \fields ->
                    (f : fields, ()),
                finalize = \e -> do
                  fields <- liftIO $ readIORef fieldsRef
                  performModifications $ onFinalize e (newEventInitialFields ++ (reverse fields))
              },
        emitImmediateEvent = \(NewEventArgs {..}) -> do
          let PrometheusRendered {..} = render newEventSelector
          traverse_ performModification $ onStart newEventInitialFields Immediate
          pure PrometheusReference
      }

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

-- | Render all events selectable by @s@ to prometheus metrics according to 'EventMetrics' @es@
type RenderSelectorPrometheus s es = forall f. s f -> PrometheusRendered f es

-- | How to render a specific 'Event' according to 'EventMetrics' @es@
data PrometheusRendered f es = PrometheusRendered
  { -- | Modify metrics at event start
    --
    -- Passed the 'newEventInitialFields'.
    onStart :: !([f] -> EventDuration -> [MetricModification es]),
    -- | Modify metrics when a field is added
    --
    -- Only called for events added with 'addField'
    onField :: !(f -> [MetricModification es]),
    -- | Modify metrics when an event finishes.
    --
    -- Passed all event fields (both initial fields and those added
    -- during the event lifetime).
    --
    -- This is not called if the event is 'Immediate'.
    onFinalize :: !(Maybe SomeException -> [f] -> [MetricModification es])
  }

-- | DSL for modifying metrics specified in 'EventMetrics' @es@
data MetricModification es

-- | What duration event is this?
data EventDuration
  = -- | A immediately finalized event
    Immediate
  | -- | An event with an extended lifetime
    Extended

-- | Reference type for 'prometheusEventBackend'
--
-- Prometheus can't make use of references, so this carries no information.
data PrometheusReference = PrometheusReference
