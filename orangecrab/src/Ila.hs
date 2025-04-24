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

{- |
# ILA memory mapped interface
All values are assumed to be big endian, both in bit and byte endianness.

## OFFSET 0x0000_0000 - Register Space

All addresses on this offset are registers intended to control the ILA itself.

### Addr 0x0000_0000 - ILA Control Registers

`| 31..24 | 23..16 |  15..8 |  7..0  |`
`| ______ | ______ |  ____  | Capt   |`


- Capt: if the right most bit is set, the ILA is considered active and will monitor incoming
signals

### Addr 0x0000_0001 - ILA Trigger Registers

`| 31..24 | 23..16 |  15..8 |  7..0  |`
`| ______ | TrgOp  | TrgMsk | TrgRst |`


- TrgRst: if the right most bit is set, it will clear the buffer and re-arm the trigger.
- TrgMsk: if the right most bit is set, it will enable the `ILA Trigger Mask`
-  TrgOp: these bits indicate how the ILA trigger should operate, possible values are:
    - 0x0000 -> Use predefined trigger
    - 0x0001 -> Trigger if the sample matches `ILA Compare Value`
    - 0x0002 -> Trigger if the sample __DOES NOT__ match `ILA Compare Value`
    - 0x0003 -> Trigger if the sample is less than specified in `ILA Compare Value`
    - 0x0004 -> Trigger if the sample is less than or equal to specified in `ILA Compare Value`
    - 0x0005 -> Trigger if the sample is more than specified in `ILA Compare Value`
    - 0x0006 -> Trigger if the sample is more than or equal to specified in `ILA Compare Value`

### Addr 0x0000_0002 - Trigger point

`| 31..24 | 23..16 |  15..8 |  7..0  |`
`| TrgPt3 | TrgPt2 | TrgPt1 | TrgPt0 |`

- TrgPt: an 32 bit unsigned value, indicating how many samples it should capture *post* trigger.
A value of 0 would include everything up until the trigger itself, a value bigger or equal to 
the size of the buffer would cause the system to never trigger.

## OFFSET 0x1000_0000 - ILA Trigger Mask

This mask is applied before data is given to the trigger, this can be used to ignore certain bits
in the trigger. The mask starts at address 0x0000_0000 and is valid for the next x amount of bits,
where x is the bit length of the combined signals being monitored.

## OFFSET 0x2000_0000 - ILA Compare Value

Depending on `TrgOp`, this value gets used to compare the incoming samples against. The value starts
at address 0x0000_0000 and is valid for the next x amount of bits, where x is the bit length of the
combined signals being monitored.

## OFFSET 0x3000_0000 - ILA Buffer

Reading values with this offset will return data captured in the buffer at the address, subtracted
by the address offset. So reading from `0x3000_0000` will return the first sample in the buffer.
`0x3000_0001` will return the second sample, etc.

-}




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
      (mux (not <$> freeze .&&. capture) (Just <$> i) (pure Nothing))

{- | Get a specific word of `a`, which may be several words wide
The width of `word` is defined by the user of the function
-}
getWord ::
  forall word input n.
  ( BitPack input
  , KnownNat word
  , KnownNat n
  , n ~ BitSize input `DivRU` word
  ) =>
  -- | The input
  input ->
  -- | The index of the word we want
  Index n ->
  -- | The output word
  BitVector word
getWord a i = (unpack . resize $ pack a :: Vec n (BitVector word)) !! i

-- | Retrieves a specific word from the ILA buffer. It will only listen to addresses within the
-- range of 0x3000_0000 and 0x3fff_ffff. Each address refers to a specific word (=32 bits) in the
-- buffer.
--
-- Samples may not precisely equal 32 bits. They may be smaller or bigger than 32 bits. However, in
-- this function each sample is assigned one or more addresses for each word it's width takes up.
-- So a 10 bit sample will only take up one address, and a 50 bit one would take up 2 addresses.
-- Different samples never share the same memory space.
--
-- The memory remains consecutive
ilaBufferManager ::
  forall dom depth a addrW.
  ( HiddenClockResetEnable dom
  , BitPack a
  , KnownNat depth
  , KnownNat addrW
  , 1 <= BitSize a `DivRU` 32
  , 1 <= depth
  ) =>
  -- | The ILA buffer
  (Signal dom (Index depth) -> (Signal dom a, Signal dom (Index (depth + 1)))) ->
  -- | The incoming Wishbone M2S signal
  Signal dom (WishboneM2S addrW 4 (BitVector 32)) ->
  -- | The specific word associated with the requested address, if the current cycle is a read cycle
  -- it will return `0` instead.
  Signal dom (BitVector 32)
ilaBufferManager buffer incoming = bufferData
 where
  -- \| Returns the index and word index of the current address provided
  getBufferIndex ::
    WishboneM2S addrW 4 (BitVector 32) -> (Index depth, Index (BitSize a `DivRU` 32))
  getBufferIndex m2s =
    let
      (bIndex, wIndex) = (m2s.addr - 0x3000_0000) `divMod` (natToNum @(BitSize a `DivRU` 32))
     in
      (unpack $ resize bIndex, unpack $ resize wIndex)
  (bufferIndex, wordIndex) = unbundle $ getBufferIndex <$> incoming

  -- \| If in a read cycle, it will contain the output of the buffer, otherwise it will return 0
  bufferData =
    mux
      (not . writeEnable <$> incoming .&&. compareAddrRange 0x3000_0000 0x3fff_ffff incoming)
      (liftA2 (getWord @32) (fst $ buffer bufferIndex) wordIndex)
      (pure 0)

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
  -- | Trigger signal
  Signal dom Bool ->
  -- | The ILA Core
  ( Signal dom a ->
    Signal dom Bool ->
    Signal dom Bool ->
    (Signal dom (Index size) -> (Signal dom a, Signal dom (Index (size + 1))))
  ) ->
  -- | Circuit outputting the content of the buffer whenever the predicate has been triggered
  -- The input signal is to clear the trigger
  Circuit
    (CSignal dom (Maybe (Index size)), CSignal dom Bool)
    (PacketStream dom (BitSize a `DivRU` 8) IlaDataHeader)
triggerController config predicate core = Circuit exposeIn
 where
  exposeIn ((newTrigPoint, triggerRst), backpressure) = out
   where
    triggerPoint = register config.triggerPoint $ liftA2 DM.fromMaybe triggerPoint newTrigPoint

    postTriggerSampled :: Signal dom (Index size)
    postTriggerSampled = register 0 $ mux triggered (satAdd SatBound 1 <$> postTriggerSampled) 0

    triggered :: Signal dom Bool
    triggered =
      register False $ mux triggerRst (pure False) triggered .||. predicate

    shouldSample :: Signal dom Bool
    shouldSample = not <$> triggered .||. postTriggerSampled .<. triggerPoint

    injectId oldMeta = (config.hash, oldMeta)

    buffer = core config.tracing (not <$> shouldSample) triggerRst
    Circuit packet = dataPacket <| mapMeta injectId <| ringBufferReaderPS buffer

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
  -- | Trigger
  Signal dom Bool ->
  -- | Capture
  Signal dom Bool ->
  -- | The ILA circuit, the input needs to be connected to some sort of byte input and the output
  -- is a PacketStream containing bytes to be routed to the PC.
  -- TODO: example? Not added one yet due to possible change in API
  Circuit
    (CSignal dom (Maybe (BitVector 8)))
    (PacketStream dom (BitSize a `DivRU` 8) IlaFinalHeader)
ila config trigger capture = circuit $ \rxByte -> do
  [dec0, dec1] <- fanoutCSig d2 <| deserializeToPacket -< rxByte

  triggerReset <- rearmTrigger -< dec0
  hasNewTrigger <- changeTriggerPoint -< dec1

  controller <-
    (triggerController config trigger $ ilaCore config.size capture)
      -< (hasNewTrigger, triggerReset)
  packets <- finalizePacket -< controller

  idC -< packets
