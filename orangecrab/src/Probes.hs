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

    injectId oldMeta = (0, oldMeta)

    buffer = core i shouldSample triggerRst
    Circuit packet = dataPacket (Proxy :: Proxy a) <| mapMeta injectId <| ringBufferReaderPS buffer

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
    oldFreezeAll = register False freezeAll
    freezeAll :: Signal dom Bool
    freezeAll =
      mux
        clearAll
        (pure False)
        $ oldFreezeAll
          .||. (any (== True) <$> triggeredSignals)

    -- \| The buffers for each signal
    buffers ::
      Vec n (Signal dom (Index size) -> (Signal dom a, Signal dom (Index (size + 1))))
    buffers =
      (\signal -> ilaCore size (pure True) signal (not <$> freezeAll) clearAll)
        <$> (snd <$> tracked)

    -- \| The PacketStream buffer readers
    bufferReaders = ringBufferReaderPS <$> buffers

    shouldIncrement (Just packet) = DM.isJust $ _last packet
    shouldIncrement Nothing = False

    -- \| Logic for selecting the currently active buffer
    bufferSelect :: Signal dom (Index n)
    bufferSelect =
      register 0 $
        liftA3
          bufferIncrement
          clearAll
          (shouldIncrement <$> (snd $ packetConstruction ((), sysBackpressure)))
          bufferSelect

    bufferIncrement :: Bool -> Bool -> Index n -> Index n
    bufferIncrement True _ _ = 0
    bufferIncrement _ True c
      | c == maxBound = maxBound
      | otherwise = c + 1
    bufferIncrement _ False c = c

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
        -- (PacketStream dom (BitSize a `DivRU` 8) (Index (size + 1)))
        (PacketStream dom (BitSize a `DivRU` 8) (BitVector 16, Index (size + 1)))
    outputCircuit = mapMetaS withIndex <| Circuit (\(_, bwdIn) -> ((), bufferOutput bwdIn))

    -- \| Function map the meta data of the packet to contain the index of the signal
    withIndex = (\idx -> (\metaIn -> (resize $ pack idx, metaIn))) <$> bufferSelect

    -- \| Construct a packet from the buffer output
    Circuit packetConstruction = finalizePacket <| dataPacket (Proxy :: Proxy a) <| outputCircuit

    out = (pure (), snd $ packetConstruction ((), sysBackpressure))
