{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Description : EventBackend for rendering events as OpenTelemetry traces
-- Copyright   : Copyright 2023 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
module Observe.Event.Render.OpenTelemetry where

import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Observe.Event.Backend
import OpenTelemetry.Context
import OpenTelemetry.Context.ThreadLocal
import OpenTelemetry.Trace.Core hiding (Event)
import OpenTelemetry.Trace.Id

-- | An 'EventBackend' built on a 'Tracer'.
--
-- When no explicit parent is set, the backend will try to find a parent in the "OpenTelemetry.Context.ThreadLocal" 'Context'.
-- However, it will never update that 'Context', as the primitive 'EventBackend' API has no way to determine if it's being
-- consumed in a scoped context or one allowing for general interleaving.
--
-- When possible, events created with 'emitImmediateEvent' will use the span event API. However, this requires a parent event
-- (explicitly specified or found in the thread-local 'Context'), so the backend will fallback to creating and 'finalize'ing a new
-- 'Span'. If a span event is created, the resulting 'reference' will be to its parent, as span events cannot be parents/links. Span
-- events do not allow for non-parent links, so any `newEventCauses` are dropped; in the future, we may either add them as custom
-- 'Attribute's or fall back to a full span if any are specified.
--
-- Event 'Link's are currently not given any attributes. In the future, arbitrary link metadata could be added to the core 'EventBackend'
-- API, in which case we could add a renderer for the link metadata type.
--
-- Currently the backend lets the underlying 'Tracer' set all timestamps. In the future, 'RenderSelectorOTel' could be allowed to run in
-- @m@ and have a timestamp field.
--
-- Exceptions passed to 'finalize' are 'recordException'ed without any custom attributes. In the future, an @Exception -> [Text, Attribute]@
-- argument could be added, or arbitrary exception metadata added to 'finalize'.
tracerEventBackend :: (MonadIO m) => Tracer -> RenderSelectorOTel s -> EventBackend m Span s
tracerEventBackend tracer render = backend
  where
    backend =
      EventBackend
        { newEvent = \args@(NewEventArgs {..}) -> do
            ctx <- maybe empty id <$> lookupContext
            let ctx' = case newEventParent of
                  Just s -> insertSpan s ctx
                  Nothing -> ctx
                OTelRendered {..} = render newEventSelector
            links <- traverse (fmap (flip NewLink []) . getSpanContext) newEventCauses
            s <-
              createSpanWithoutCallStack tracer ctx' eventName $
                SpanArguments
                  { kind = eventKind,
                    attributes = concatMap renderField newEventInitialFields,
                    links = links,
                    startTime = Nothing
                  }
            pure $
              Event
                { reference = s,
                  addField = addAttributes s . renderField,
                  finalize = \me -> do
                    let recordError e = do
                          recordException s [("exception.escaped", toAttribute True)] Nothing e
                          setStatus s . Error . pack $ show e
                    maybe (setStatus s Ok) recordError me
                    endSpan s Nothing
                },
          emitImmediateEvent = \args@(NewEventArgs {..}) -> case newEventParent of
            Nothing -> do
              m_ctx <- lookupContext
              case m_ctx >>= lookupSpan of
                Just s ->
                  emitImmediateEvent backend $
                    args
                      { newEventParent = Just s
                      }
                Nothing -> do
                  ev <- newEvent backend args
                  finalize ev Nothing
                  pure $ reference ev
            Just s -> do
              let OTelRendered {..} = render newEventSelector
              addEvent s $
                NewEvent
                  { newEventName = eventName,
                    newEventAttributes = concatMap renderField newEventInitialFields,
                    newEventTimestamp = Nothing
                  }
              pure s
        }

-- | Render a given selector (and all of its fields) to OpenTelemetry
type RenderSelectorOTel s = forall f. s f -> OTelRendered f

-- | The result of rendering a specific selector with field type @f@
data OTelRendered f = OTelRendered
  { -- | The name of the event. See section on "span name" at <https://opentelemetry.io/docs/reference/specification/trace/api/#span>
    eventName :: !Text,
    -- | See the specification on [SpanKind](https://opentelemetry.io/docs/reference/specification/trace/api/#spankind)
    eventKind :: !SpanKind,
    -- | Render a field to a set of span [attributes](https://opentelemetry.io/docs/reference/specification/common/#attribute).
    --
    -- Note especially the [attribute naming guidelines](https://opentelemetry.io/docs/reference/specification/common/attribute-naming/)
    renderField :: !(f -> [(Text, Attribute)])
  }
