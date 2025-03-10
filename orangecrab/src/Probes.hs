
module Probes where

import Clash.Prelude
import RingBuffer

ilaCore ::
  forall dom size a . (HiddenClockResetEnable dom, NFDataX a, Eq a, KnownNat size, 1 <= size) =>
  SNat size ->
  -- | Capture
  Signal dom Bool ->
  -- | Trigger predicate
  (a -> Bool) ->
  -- | Trigger reset
  Signal dom Bool ->
  -- | The input signal to probe
  Signal dom a ->
  (
    -- | The buffer
    (Signal dom (Index size) -> (Signal dom (Maybe a), Signal dom (Index (size + 1))))
    -- | Same as the input signal
  , Signal dom a
  )
ilaCore size capture trigger triggerRst i = record
  where
    triggered :: Signal dom Bool
    triggered = register False $ mux triggerRst (pure False) triggered .||. (trigger <$> i)

    shouldSample :: Signal dom Bool
    shouldSample = (not <$> triggered) .&&. capture

    buffer :: Signal dom (Index size) -> (Signal dom (Maybe a), Signal dom (Index (size + 1)))
    buffer = ringBuffer size Nothing (not <$> shouldSample) (mux capture (Just . Just <$> i) (pure Nothing))

    record = (buffer, i)

