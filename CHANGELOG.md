# Revision history for eventuo11y

## 0.7.1.0 -- 2023-01-14

- Add `Observe.Event.Render.InMemory`

## 0.7.0.0 -- 2023-01-13

- Use `NewEventArgs` as the basic way to initialize a new `Event`.

  This replaces `addReference`, requiring the parent and any
  proximate causes to be specified up-front. If a use case for
  multiple parents, or specifying parents/causes only after an
  `Event` is live, is found, we can add it back.
- Add `emitImmediateEvent` and `emitImmediateEvent'` as primitives for
  events with no duration.

## 0.6.0.0 -- 2022-12-23

- Add `MonadEvent` for implicit backend management
- Simplify `EventBackend` and move to backend-modifying functions instead of `BackendModification`
- Move from `MonadCleanup` to `general-allocate`

## 0.5.0.0 -- 2022-10-23

- Add MonadCleanup
- Break out eventuo11y-json
- Include narrowing in subEventBackend and causedEventBackend

## 0.3.2.0 -- 2022-10-04

Add hoistEvent

## 0.3.1.0 -- 2022-10-04

Add causedEventBackend

## 0.3.0.0 -- 2022-10-03

Add single-modifier exports from BackendModification

## 0.2.0.0 -- 2022-10-03

Relax text upper and lower bounds

## 0.2.0.0 -- 2022-10-03

- Add Observe.Event.BackendModification DSL
- Generalize newOnceFlagIO to newOnceFlagMVar

## 0.1.0.1 -- 2022-09-30

Relax aeson lower bound

## 0.1.0.0 -- 2022-09-28

Initial release
