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

{- | ILA Wishbone interface

This circuit can be used to instantiate and configure an ILA using a wishbone interface. Changing
ILA behaviour is done by writing to specific addresses and selecting the right bytes using busSelect.

A few useful registers are:
|   Address   | Bus Select |      Description      | Operation |
|-------------|------------|-----------------------|-----------|
| 0x0000_0000 | 0b0001     | Capture               | WriteOnly |
| 0x0000_0000 | 0b0010     | Trigger reset         | WriteOnly |
| 0x0000_0000 | 0b0100     | Trigger operation     | WriteOnly |
| 0x0000_0001 | 0b1111     | Trigger point         | WriteOnly |
| 0x3000_0000 | 0b1111     | Buffer samples        | Read Only |
-}
ilaWb ::
  forall dom depth a.
  ( HiddenClockResetEnable dom
  , BitPack a
  , NFDataX a
  , KnownNat depth
  , 1 <= BitSize a `DivRU` 32
  , 1 <= depth
  ) =>
  -- | Initial ILA configuration
  IlaConfig depth (Signal dom a) ->
  -- | Signal indicating if the predicate has triggered
  Signal dom Bool ->
  -- | The ILA wishbone interface
  Circuit
    (Wishbone dom Standard 32 (BitVector 32))
    ()
ilaWb config predicate = Circuit exposeIn
 where
  exposeIn (fwdM2S, _) = out
   where
    -- In theory, I could respond with `err` when an inproper address is given. However I don't
    -- quite know how etherboneC reacts to `err` and I doubt it will propagate the error back to
    -- the host.

    -- All of this could be written as a struct and a mealy machine
    -- I think that'd be nicer too...

    postTriggerSampled :: Signal dom (Index depth)
    postTriggerSampled = register 0 $ mux triggered (satAdd SatBound 1 <$> postTriggerSampled) 0

    capture :: Signal dom Bool
    capture =
      register True $
        addrMux
          0x0000_0000
          0b0001
          (bitToBool . lsb <$> incomingData)
          capture
          fwdM2S

    triggerMode :: Signal dom (Index 7)
    triggerMode =
      register 0 $
        addrMux
          0x0000_0000
          0b0100
          (unpack . resize <$> incomingData)
          triggerMode
          fwdM2S

    triggerReset :: Signal dom Bool
    triggerReset =
      register False $
        addrMux
          0x0000_0000
          0b0010
          (bitToBool . lsb <$> incomingData)
          (pure False)
          fwdM2S

    triggered :: Signal dom Bool
    triggered =
      register False $
        addrMux
          0x0000_0000
          0b0010
          (not <$> triggerReset)
          (triggered .||. predicate)
          fwdM2S

    triggerPoint :: Signal dom (Index depth)
    triggerPoint =
      register config.triggerPoint $
        addrMux
          0x0000_0001
          0b1111
          (unpack . resize <$> incomingData)
          triggerPoint
          fwdM2S

    shouldSample :: Signal dom Bool
    shouldSample = not <$> triggered .||. postTriggerSampled .<. triggerPoint

    incomingData = pack . writeData <$> fwdM2S

    bufferOutput =
      ilaBufferManager
        (ilaCore config.size capture config.tracing (not <$> shouldSample) triggerReset)
        fwdM2S

    -- \| Generates the wishbone reply
    reply ::
      -- \| Wether or not we're in a cycle
      Bool ->
      -- \| The data to reply with (if in a read cycle)
      BitVector 32 ->
      -- \| The wishbone response
      WishboneS2M (BitVector 32)
    reply inCyc dat =
      WishboneS2M
        { readData = dat
        , acknowledge = inCyc
        , err = False
        , stall = False
        , retry = False
        }

    -- \| The first clock cycle shouldn't ack according to wishbone
    delayedAck = inWbCycle fwdM2S .&&. (register False $ inWbCycle fwdM2S)

    -- Writes are done in one clock cycle, but wishbone timing requires us to delay it by one clock cycle
    out = (register emptyWishboneS2M $ liftA2 reply delayedAck bufferOutput, ())

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
