{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Ila where

import Clash.Prelude
import Packet
import RingBuffer

import Data.Data

import Protocols
import Protocols.PacketStream

ilaCore ::
  forall dom size a.
  ( HiddenClockResetEnable dom
  , NFDataX a
  , KnownNat size
  , 1 <= size
  ) =>
  SNat size ->
  -- | Capture
  Signal dom Bool ->
  -- | The input signal to probe
  Signal dom a ->
  -- | Wether or not to freeze the buffer
  Signal dom Bool ->
  -- | Clear the buffer
  Signal dom Bool ->
  -- | The buffer
  (Signal dom (Index size) -> (Signal dom a, Signal dom (Index (size + 1))))
ilaCore size capture i freeze bufClear = record
 where
  buffer :: Signal dom (Index size) -> (Signal dom a, Signal dom (Index (size + 1)))
  buffer =
    ringBuffer
      size
      undefined
      bufClear
      (mux (freeze .&&. capture) (Just <$> i) (pure Nothing))

  record = buffer

triggerController ::
  forall dom size a.
  ( HiddenClockResetEnable dom
  , NFDataX a
  , BitPack a
  , KnownNat size
  , 1 <= BitSize a `DivRU` 8
  , 1 <= size
  ) =>
  -- | Trigger predicate
  (a -> Bool) ->
  -- | The input signal to probe
  Signal dom a ->
  -- | The ILA Core
  ( Signal dom a ->
    Signal dom Bool ->
    Signal dom Bool ->
    (Signal dom (Index size) -> (Signal dom a, Signal dom (Index (size + 1))))
  ) ->
  -- | Circuit outputting the content of the buffer whenever the predicate has been triggered
  -- The input signal is to clear the trigger
  Circuit
    (CSignal dom Bool)
    (PacketStream dom (BitSize a `DivRU` 8) IlaDataPacket)
triggerController predicate i core = Circuit exposeIn
 where
  exposeIn (triggerRst, backpressure) = out
   where
    oldTriggered :: Signal dom Bool
    oldTriggered = register False triggered
    triggered :: Signal dom Bool
    triggered = mux triggerRst (pure False) oldTriggered .||. (predicate <$> i)

    shouldSample :: Signal dom Bool
    shouldSample = not <$> triggered

    injectId oldMeta = (0, oldMeta)

    buffer = core i shouldSample triggerRst
    Circuit packet = dataPacket (Proxy :: Proxy a) <| mapMeta injectId <| ringBufferReaderPS buffer

    out = (pure (), snd $ packet (triggered, backpressure))

ila ::
  forall dom size a.
  ( HiddenClockResetEnable dom
  , NFDataX a
  , BitPack a
  , KnownNat size
  , 1 <= BitSize a `DivRU` 8
  , 1 <= size
  ) =>
  -- | Maximum sample count
  SNat size ->
  -- | Trigger predicate
  (a -> Bool) ->
  -- | Signal to probe
  Signal dom a ->
  -- | Circuit outputting the content of the buffer whenever the predicate has been triggered
  -- The input signal is to clear the trigger
  Circuit
    (CSignal dom Bool)
    (PacketStream dom (BitSize a `DivRU` 8) IlaFinalizedPacket)
ila size predicate sig = finalizePacket <| (triggerController predicate sig $ ilaCore size (pure True))
