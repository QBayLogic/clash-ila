{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Ila where

import Clash.Prelude
import ConfigGen
import Packet
import RingBuffer

import Data.Data
import Data.Maybe as DM

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
  exposeIn ((newTrigPoint, triggerRst), backpressure) = out
   where
    triggerPoint = register config.triggerPoint $ liftA2 DM.fromMaybe triggerPoint newTrigPoint

    postTriggerSampled :: Signal dom (Index size)
    postTriggerSampled = register 0 $ mux triggered (satAdd SatBound 1 <$> postTriggerSampled) 0

    triggered :: Signal dom Bool
    triggered =
      register False $ mux triggerRst (pure False) triggered .||. (predicate <$> config.tracing)

    shouldSample :: Signal dom Bool
    shouldSample = not <$> triggered .||. postTriggerSampled .<. triggerPoint

    injectId oldMeta = (config.hash, oldMeta)

    buffer = core config.tracing shouldSample triggerRst
    Circuit packet = dataPacket (Proxy :: Proxy a) <| mapMeta injectId <| ringBufferReaderPS buffer

    out = ((pure (), pure ()), snd $ packet (triggered, backpressure))

-- | A signal fanout, simply duplicates a signal n times
fanoutCSig ::
  forall dom n a.
  (KnownNat n) =>
  -- | How many times to 'duplicate' the signal
  SNat n ->
  -- | The circuit, where the input will be duplicated n times and returned as the output
  Circuit
    (CSignal dom a)
    (Vec n (CSignal dom a))
fanoutCSig _ = Circuit go
 where
  go (fwd, _) = (pure (), repeat fwd)

-- | Checks if an incoming packet should re-arm the the trigger (reset the trigger)
rearmTrigger ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | The circuit, input is the incoming packet (if there is one) and the output returns a bool
  -- indicating wether or not it should re-arm the trigger
  Circuit
    (CSignal dom (Maybe IlaIncomingPacket))
    (CSignal dom Bool)
rearmTrigger = Circuit exposeIn
 where
  exposeIn (fwdIn, _) = out
   where
    rearm (Just IlaResetTrigger) = True
    rearm _ = False

    out = (pure (), rearm <$> fwdIn)

{- | Checks if an incoming packet should change the trigger point of the ILA, and if it should
what index should it reset at?

NOTE: due to the `IlaChangeTriggerPoint` using a bv32, values bigger than the buffer `size` itself
will be truncated on a bit level. That may result in some unexpected values, the simple solution
is to not send out values bigger than `size`
-}
changeTriggerPoint ::
  forall dom size.
  ( HiddenClockResetEnable dom
  , KnownNat size
  , 1 <= size
  ) =>
  -- The circuit, input is an incoming packet (if there is one) and the output is the new trigger
  -- point, if the packet is a `IlaChangeTriggerPoint`, otherwise returns `Nothing`
  Circuit
    (CSignal dom (Maybe IlaIncomingPacket))
    (CSignal dom (Maybe (Index size)))
changeTriggerPoint = Circuit exposeIn
 where
  exposeIn (fwdIn, _) = out
   where
    newTrigger (Just (IlaChangeTriggerPoint new)) = Just $ unpack . resize $ pack new
    newTrigger _ = Nothing

    out = (pure (), newTrigger <$> fwdIn)

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
  -- | The ILA circuit, the input needs to be connected to some sort of byte input and the output
  -- is a PacketStream containing bytes to be routed to the PC.
  --
  -- TODO: example? Not added one yet due to possible change in API
  Circuit
    (CSignal dom (Maybe (BitVector 8)))
    (PacketStream dom (BitSize a `DivRU` 8) IlaFinalizedPacket)
ila config predicate = circuit $ \rxByte -> do
  [dec0, dec1] <- fanoutCSig d2 <| deserializeToPacket -< rxByte

  triggerReset <- rearmTrigger -< dec0
  hasNewTrigger <- changeTriggerPoint -< dec1

  controller <-
    (triggerController config predicate $ ilaCore config.size (pure True))
      -< (hasNewTrigger, triggerReset)
  packets <- finalizePacket -< controller

  idC -< packets
