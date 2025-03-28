{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Ila where

import Clash.Prelude
import Packet
import RingBuffer
import ConfigGen

import Data.Data

import Protocols
import Protocols.PacketStream

{- | The buffer of the ila, with signals to control wether or not the signals get captured

This function exposes the barebones of the ILA capturing logic and it will only accept boolean
values to determine wether or not to capture samples. For more advanced trigger logic, check out
`triggerController`
-}
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
ilaCore size capture i freeze bufClear = buffer
 where
  buffer :: Signal dom (Index size) -> (Signal dom a, Signal dom (Index (size + 1)))
  buffer =
    ringBuffer
      size
      undefined
      bufClear
      (mux (freeze .&&. capture) (Just <$> i) (pure Nothing))

{- | The trigger handler of the ILA

Given a predicate, it will test it against incoming samples and tell the buffer to stop sampling
new data or not.
-}
triggerController ::
  forall dom size a.
  ( HiddenClockResetEnable dom
  , NFDataX a
  , BitPack a
  , KnownNat size
  , 1 <= BitSize a `DivRU` 8
  , 1 <= size
  ) =>
  -- | The configuration of the ILA
  IlaConfig size (Signal dom a) ->
  -- | Trigger predicate
  (a -> Bool) ->
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
    (PacketStream dom (BitSize a `DivRU` 8) IlaDataHeader)
triggerController predicate i core = Circuit exposeIn
 where
  exposeIn (triggerRst, backpressure) = out
   where
    oldTriggered :: Signal dom Bool
    oldTriggered = register False triggered
    triggered :: Signal dom Bool
    triggered = mux triggerRst (pure False) oldTriggered .||. (predicate <$> config.tracing)

    shouldSample :: Signal dom Bool
    shouldSample = not <$> triggered

    injectId oldMeta = (config.hash, oldMeta)

    buffer = core config.tracing shouldSample triggerRst
    Circuit packet = dataPacket (Proxy :: Proxy a) <| mapMeta injectId <| ringBufferReaderPS buffer

    out = (pure (), snd $ packet (triggered, backpressure))

{- | The ILA component itself

Given any signal and a trigger condition, it will be capable of sampling the signal up until
it is triggered. At which point it will proceed to send out all the samples it captured via the
`PacketStream` protocol. This data can than be forwarded to an external port and the ILA CLI
will display this data.
-}
ila ::
  forall dom size a.
  ( HiddenClockResetEnable dom
  , NFDataX a
  , BitPack a
  , KnownNat size
  , 1 <= BitSize a `DivRU` 8
  , 1 <= size
  ) =>
  -- | The configuration of the ILA
  IlaConfig size (Signal dom a) ->
  -- | Trigger predicate
  (a -> Bool) ->
  -- | Circuit outputting the content of the buffer whenever the predicate has been triggered
  -- The input signal is to clear the trigger
  Circuit
    (CSignal dom Bool)
    (PacketStream dom (BitSize a `DivRU` 8) IlaFinalHeader)
ila size predicate sig = finalizePacket <| (triggerController predicate sig $ ilaCore size (pure True))
