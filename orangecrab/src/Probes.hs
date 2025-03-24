{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Probes where

import Clash.Prelude
import Packet
import RingBuffer

import Control.Monad qualified as CM
import Data.Data
import Data.Maybe qualified as DM

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

    buffer = core i shouldSample triggerRst
    Circuit packet = dataPacket (Proxy :: Proxy a) <| ringBufferReaderPS buffer

    out = (pure (), snd $ packet (triggered, backpressure))

ilaProbe ::
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
ilaProbe size predicate sig = finalizePacket <| (triggerController predicate sig $ ilaCore size (pure True))

ila ::
  forall dom size n a.
  ( HiddenClockResetEnable dom
  , NFDataX a
  , BitPack a
  , KnownNat size
  , KnownNat n
  , 1 <= BitSize a `DivRU` 8
  , 1 <= size
  , 1 <= n
  ) =>
  -- | Maximum sample count
  SNat size ->
  -- | Signals to probe
  Vec
    n
    ( -- \| Predicate
      (a -> Bool)
    , -- \| The signal itself
      Signal dom a
    ) ->
  -- | Circuit outputting the content of the buffer whenever the predicate has been triggered
  -- The input signal is to clear the trigger
  Circuit
    (CSignal dom Bool)
    (PacketStream dom (BitSize a `DivRU` 8) IlaFinalizedPacket)
ila size tracked = Circuit exposeIn
 where
  exposeIn (clearAll, sysBackpressure) = out
   where
    signals :: Signal dom (Vec n (a -> Bool, a))
    signals = zip (fst <$> tracked) <$> (bundle (snd <$> tracked))

    -- \| The predicates on each signal
    triggeredSignals :: Signal dom (Vec n Bool)
    triggeredSignals = fmap (\(predicate, a) -> predicate a) <$> signals

    -- \| Signal to indicate when all buffers should be frozen
    -- For now, if any predicate on any signal triggers, it should freeze all buffers
    freezeAll :: Signal dom Bool
    freezeAll = any (== True) <$> triggeredSignals

    -- \| Signal to indicate when all buffers should be cleared
    -- clearAll :: Signal dom Bool
    -- clearAll = pure False

    -- \| The buffers for each signal
    buffers ::
      Vec n (Signal dom (Index size) -> (Signal dom a, Signal dom (Index (size + 1))))
    buffers = (\signal -> ilaCore size (pure True) signal freezeAll clearAll) <$> (snd <$> tracked)

    -- \| The PacketStream buffer readers
    bufferReaders = ringBufferReaderPS <$> buffers

    -- \| Logic for selecting the currently active buffer
    bufferSelect :: Signal dom (Index n)
    bufferSelect = register 0 bufferSelect

    -- \| A vec of signals indicating which buffer is active
    bufferActive :: Vec n (Signal dom Bool)
    bufferActive = (\n -> ((== n) <$> bufferSelect) .&&. freezeAll) <$> indicesI

    -- \| Decomposes a `Vec n` of circuits into it's raw signals, makes processing them easier
    decomposeCircuit ::
      Signal dom Bool ->
      (Bwd (PacketStream dom (BitSize a `DivRU` 8) (Index (size + 1)))) ->
      ( Circuit
          (CSignal dom Bool)
          (PacketStream dom (BitSize a `DivRU` 8) (Index (size + 1)))
      ) ->
      (Fwd (PacketStream dom (BitSize a `DivRU` 8) (Index (size + 1))))
    decomposeCircuit active backpressure (Circuit circ) = snd $ circ (active, backpressure)

    -- \| Outputs the output of the actively selected buffer
    bufferOutput backpressure =
      CM.msum
        <$> (bundle $ liftA3 decomposeCircuit bufferActive (repeat backpressure) bufferReaders)

    -- \| Same as `bufferOutput` but then wrapped neatly in a `Circuit`
    outputCircuit ::
      Circuit
        ()
        (PacketStream dom (BitSize a `DivRU` 8) (Index (size + 1)))
    outputCircuit = Circuit (\(_, bwdIn) -> ((), bufferOutput bwdIn))

    -- \| Construct a packet from the buffer output
    Circuit packetConstruction = finalizePacket <| dataPacket (Proxy :: Proxy a) <| outputCircuit

    out = (pure (), snd $ packetConstruction ((), sysBackpressure))
