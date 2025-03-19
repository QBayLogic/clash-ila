module Probes where

import Clash.Prelude
import RingBuffer

ilaCore ::
  forall dom size a.
  (HiddenClockResetEnable dom, NFDataX a, Eq a, KnownNat size, 1 <= size) =>
  SNat size ->
  -- | Capture
  Signal dom Bool ->
  -- | Trigger predicate
  (a -> Bool) ->
  -- | Trigger reset
  Signal dom Bool ->
  -- | The input signal to probe
  Signal dom a ->
  -- | The buffer
  (Signal dom (Index size) -> (Signal dom a, Signal dom (Index (size + 1))))
ilaCore size capture trigger triggerRst i = record
  where
    oldTriggered :: Signal dom Bool
    oldTriggered = register False triggered
    triggered :: Signal dom Bool
    triggered = mux triggerRst (pure False) oldTriggered .||. (trigger <$> i)

    shouldSample :: Signal dom Bool
    shouldSample = (not <$> triggered) .&&. capture

    buffer :: Signal dom (Index size) -> (Signal dom a, Signal dom (Index (size + 1)))
    buffer = ringBuffer size undefined triggerRst (mux shouldSample (Just <$> i) (pure Nothing))

    record = buffer
