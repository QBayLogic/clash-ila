
module Probes where

import Clash.Prelude
import RingBuffer

ilaProbe ::
  forall dom size a . (HiddenClockResetEnable dom, NFDataX a, KnownNat size, 1 <= size) =>
  SNat size ->
  -- | Capture
  (Signal dom Bool) ->
  -- | The input signal to probe
  (Signal dom a) ->
  (
    -- | The buffer
    (Signal dom (Index size) -> (Signal dom (Maybe a), Signal dom (Index size)))
    -- | Same as the input signal
  , (Signal dom a)
  )
ilaProbe size capture i = record
  where
    buffer :: Signal dom (Index size) -> (Signal dom (Maybe a), Signal dom (Index size))
    buffer = ringBuffer size Nothing (not <$> capture) (mux capture (Just . Just <$> i) (pure Nothing))

    record = (buffer, i)

