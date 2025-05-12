{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Ila where

import Clash.Prelude
import ConfigGen
import RingBuffer
import WishboneUtils

import Data.Data
import Data.Maybe as DM

import Clash.Cores.Etherbone (etherboneC)
import Clash.Cores.UART (ValidBaud)
import Communication
import Protocols
import Protocols.PacketStream
import Protocols.Wishbone
import SignalFieldSelectors

{- | The buffer of the ila, with signals to control wether or not the signals get captured

This function exposes the barebones of the ILA capturing logic and it will only accept boolean
values to determine wether or not to capture samples. For more advanced trigger logic, check out
`triggerController`
-}
ilaCore ::
  forall dom size a.
  ( HiddenClockResetEnable dom
  , NFDataX a
  , BitPack a
  , KnownNat size
  , 1 <= BitSize a `DivRU` 32
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

{- | Sets a specific word of `a`, which may be several words wide
The width of the `word` is defined by the user of the function
-}
setWord ::
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
  -- | The value we want to set it too
  BitVector word ->
  -- | The output, our input with the word swapped
  input
setWord a i w = unpack . resize . pack $ replace i w inputWords
 where
  inputWords :: Vec n (BitVector word)
  inputWords = unpack . resize $ pack a

{- | Retrieves a specific word from the ILA buffer. It will only listen to addresses within the
range of 0x3000_0000 and 0x3fff_ffff. Each address refers to a specific word (=32 bits) in the
buffer.

Samples may not precisely equal 32 bits. They may be smaller or bigger than 32 bits. However, in
this function each sample is assigned one or more addresses for each word it's width takes up.
So a 10 bit sample will only take up one address, and a 50 bit one would take up 2 addresses.
Different samples never share the same memory space.

The memory remains consecutive
-}
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

-- | Defines how the ILA should treat the trigger select registers
data IlaPredicateOperation = PredicateAND | PredicateOR
  deriving (Generic, NFDataX, Show, Enum, BitPack)

-- | The actual operation associated with this predicate operation
predicateOp :: (Foldable t) => IlaPredicateOperation -> (t Bool -> Bool)
predicateOp PredicateAND = and
predicateOp PredicateOR = or

{- | A record containing the values stored in the ILA register map. Not all values are read/writeable
from the outside.

A lot of the register map is also exposed as a memory map, with the following layout:
|   Address   | Bus Select |      Description      |  Operation  |
|-------------|------------|-----------------------|-------------|
| 0x0000_0000 | 0b0001     | Capture               | ReadWrite   |
| 0x0000_0000 | 0b0010     | Trigger reset         | ReadWrite'1 |
| 0x0000_0001 | 0b1111     | Trigger point         | ReadWrite   |
| 0x0000_0002 | 0b1111     | ILA hash              | Read        |
| 0x0000_0003 | 0b0001     | ILA trigger operation | ReadWrite'2 |
| 0x0000_0004 | 0b1111     | ILA trigger select 0  | ReadWrite'3 |
| 0x0000_0005 | 0b1111     | ILA trigger select 1  | ReadWrite'3 |
| 0x1000_0000 | 0b1111     | ILA trigger mask      | ReadWrite   |
| 0x1100_0000 | 0b1111     | ILA trigger compare   | ReadWrite   |
| 0x3000_0000 | 0b1111     | Buffer samples        | Read        |

'1: Reading from this address will return wether or not the ILA has been triggered or not
'2: Trigger operation is either AND if thre first bit is set, otherwise OR is assumed
'3: Each bit is for one trigger
-}
data IlaRM bitSizeA depth n = IlaRM
  { capture :: Bool
  -- ^ If the ILA should be capturing signals
  , triggered :: Bool
  , -- depending on the `triggerPoint` it may still be sampling new values.
    triggerPoint :: Index depth
  -- ^ The amount of signals to capture *after* triggering
  , shouldSample :: Bool
  -- ^ Indicates if the ILA should continue sampling data. This gets automatically set after the
  -- ILA has been triggered and enough samples have been collected. It will only get out of this
  -- state by resetting the ILA.
  , sampledAfterTrigger :: Index depth
  -- ^ The amount of samples the ILA captured after triggering
  , triggerSelect :: BitVector 32
  -- ^ Which trigger predicate to use
  , triggerOperation :: IlaPredicateOperation
  -- ^ How the ILA treats the trigger predicates in combination with `triggerSelect` bits
  , triggerMask :: BitVector bitSizeA
  -- ^ The mask given to the trigger predicate
  , triggerCompare :: BitVector bitSizeA
  -- ^ The compare value given to the trigger predicate
  }
  deriving (Generic, NFDataX, Show)

deriveSignalHasFields ''IlaRM

{- | Read from memory mapped registers in the register map using the addresses selected from a
wishbone packet
-}
readIlaMM ::
  forall a depth n.
  ( KnownNat depth
  , KnownNat a
  , KnownNat n
  , 1 <= n
  , 1 <= depth
  , 1 <= a
  ) =>
  -- | The wishbone address
  BitVector 32 ->
  -- | The wishbone bus select
  BitVector 4 ->
  -- | The ILA MM
  IlaRM a depth n ->
  -- | Associated value with the address and bus select, `Nothing` if there is none
  Maybe (BitVector 32)
readIlaMM 0x0000_0000 0b0001 rm = Just . extend $ pack rm.capture
readIlaMM 0x0000_0000 0b0010 rm = Just . extend $ pack rm.triggered
readIlaMM 0x0000_0001 0b1111 rm = Just . resize $ pack rm.triggerPoint
readIlaMM 0x0000_0003 0b0001 rm = Just . resize $ pack rm.triggerOperation
readIlaMM 0x0000_0004 0b1111 rm = Just $ resize (rm.triggerSelect .>>. 32)
readIlaMM 0x0000_0005 0b1111 rm = Just $ resize (rm.triggerSelect .&. 0xffff_ffff)
readIlaMM _ _ _ = Nothing

-- | Tests if multiple bits (via a mask) match
testBits ::
  (Bits a) =>
  -- | Value to test
  a ->
  -- | The compare value
  a ->
  -- | The bit mask
  a ->
  -- | True is the bits match
  Bool
testBits value ref msk = value .&. msk == ref

{- | Certain registers trigger 'actions' rather than save a value, this enum allows the ILA to
properly respond to those requests
-}
data IlaAction = None | ResetTrigger
  deriving (Generic, Eq, NFDataX, Show, Enum)

-- | Update the memory mapped registers from the register map from the wishbone request
writeIlaMM ::
  forall a depth n.
  ( KnownNat depth
  , KnownNat a
  , KnownNat n
  , 1 <= depth
  , 1 <= n
  , 1 <= a `DivRU` 32
  , 1 <= a
  ) =>
  -- | The wishbone address
  BitVector 32 ->
  -- | The wishbone bus select
  BitVector 4 ->
  -- | The value to write
  BitVector 32 ->
  -- | The Ila register map
  IlaRM a depth n ->
  -- | Updated Ila register map, invalid addresses will not modify the register map
  (IlaRM a depth n, IlaAction)
writeIlaMM 0x0000_0000 0b0001 write rm = (rm{capture = unpack $ truncateB write}, None)
writeIlaMM 0x0000_0000 0b0010 _write rm = (rm{triggered = False}, ResetTrigger)
writeIlaMM 0x0000_0001 0b1111 write rm = (rm{triggerPoint = unpack $ resize write}, None)
writeIlaMM 0x0000_0003 0b0001 write rm = (rm{triggerOperation = unpack $ resize write}, None)
writeIlaMM 0x0000_0004 0b1111 write rm = (rm{triggerSelect = write}, None)
writeIlaMM address 0b1111 write rm
  | testBits address 0x1000_0000 0xff00_0000 =
      (rm{triggerMask = setWord rm.triggerMask index write}, None)
  | testBits address 0x1100_0000 0xff00_0000 =
      (rm{triggerCompare = setWord rm.triggerCompare index write}, None)
 where
  index = unpack . resize $ address .&. 0x00ff_ffff
writeIlaMM _ _ _ rm = (rm, None)

{- | ILA Wishbone interface

This circuit can be used to instantiate and configure an ILA using a wishbone interface. Changing
ILA behaviour is done by writing to specific addresses and selecting the right bytes using busSelect.
-}
ilaWb ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | Initial ILA configuration
  IlaConfig dom ->
  -- | The ILA wishbone interface
  Circuit
    (Wishbone dom Standard 32 (BitVector 32))
    ()
ilaWb (IlaConfig @_ @a @depth @m depth initTriggerPoint ilaHash tracing triggers captureSignal) = Circuit exposeIn
 where
  exposeIn (fwdM2S, _) = out
   where
    -- \| The initial contents of the memory map
    initRM :: IlaRM (BitSize a) depth m
    initRM =
      IlaRM
        { capture = False
        , triggered = False
        , triggerPoint = initTriggerPoint
        , shouldSample = True
        , sampledAfterTrigger = 0
        , triggerOperation = PredicateOR
        , triggerSelect = minBound
        , triggerMask = maxBound
        , triggerCompare = 0 -- TODO: make this selectable by user
        }

    -- \| Update parts of the RM which aren't depending on input from WB
    updateRM ::
      -- \| If the predicate got triggered
      Bool ->
      -- \| If the capture is set
      Bool ->
      -- \| The current register map
      IlaRM (BitSize a) depth m ->
      -- \| The updated register map
      IlaRM (BitSize a) depth m
    updateRM triggered capture rm =
      rm
        { triggered = rm.triggered || triggered
        , capture = capture
        , sampledAfterTrigger = if rm.triggered then satAdd SatBound 1 rm.sampledAfterTrigger else 0
        , shouldSample = not rm.triggered || rm.sampledAfterTrigger < rm.triggerPoint
        }

    -- \| Selects the right predicate and applies it on incoming sample
    doesTrigger :: IlaRM (BitSize a) depth m -> a -> Bool
    -- doesTrigger rm currentSample = ilaPredicateEq currentSample rm.triggerCompare rm.triggerMask
    doesTrigger rm currentSample =
      predicateOp rm.triggerOperation $
        imap
          ( \index trigger ->
              testBit rm.triggerSelect (fromIntegral index)
                && trigger currentSample rm.triggerCompare rm.triggerMask
          )
          triggers

    -- \| Handle WB writes and update the memory map accordingly,
    (ilaRM, ilaAction) =
      unbundle $
        moore
          ( \(oldMM, _) (wb, currentSample, capture) ->
              if inWbCycle' wb && wb.writeEnable
                then
                  writeIlaMM wb.addr wb.busSelect wb.writeData $
                    updateRM (doesTrigger oldMM currentSample) capture oldMM
                else (updateRM (doesTrigger oldMM currentSample) capture oldMM, None)
          )
          id
          (initRM, None)
          (bundle (fwdM2S, tracing, captureSignal))

    -- \| The output from the ILA's internal buffer
    bufferOutput =
      ilaBufferManager
        ( ilaCore
            depth
            ilaRM.capture
            tracing
            (not <$> ilaRM.shouldSample)
            ((== ResetTrigger) <$> ilaAction)
        )
        fwdM2S

    -- \| Distribute the read request to different parts of the ILA depending on the address & bus select
    -- It will first attempt to read from the register map (& constant values), if there's no valid
    -- address there. It will return the buffer content. If an invalid address gets read there,
    -- it will return zero.
    readManager' ::
      -- \| The current WB packet
      WishboneM2S 32 4 (BitVector 32) ->
      -- \| The ila register map
      IlaRM (BitSize a) depth m ->
      -- \| The value the buffer is currently pointing at
      BitVector 32 ->
      -- \| The value the component should respond with
      BitVector 32
    readManager' wb rm bufValue
      | wb.addr == 0x0000_0002 && wb.busSelect == 0b1111 = ilaHash
      | otherwise =
          case readIlaMM wb.addr wb.busSelect rm of
            Just v -> v
            Nothing
              | rm.shouldSample -> 0
              | otherwise -> bufValue
    readManager = liftA3 readManager' fwdM2S ilaRM bufferOutput

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
    out =
      ( register emptyWishboneS2M $ liftA2 reply delayedAck readManager
      , ()
      )

{- | The ILA component itself

Given any set of signals and a trigger condition, it will be capable of sampling the signals up until
it is triggered. At which point the connected host device can send commands to retrieve / re-arm the
trigger. Data communication is being done through `Etherbone` packets which get sent from the host
device. If run configuration of the ILA on the FPGA is desired, please look at `ilaWb` to instantiate
an ILA with an Wishbone interface instead.
-}
ila ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | The initial configuration of the ILA
  IlaConfig dom ->
  -- | The ILA circuit, the incoming packet stream should contain valid `Etherbone` packets. The
  -- outgoing stream are etherbone response packets.
  Circuit
    (PacketStream dom 4 ())
    (PacketStream dom 4 ())
ila config = circuit $ \incoming -> do
  (outgoing, wbMaster) <- etherboneC 0 (pure Nil) -< incoming

  ilaWb config -< wbMaster

  idC -< outgoing

{- | UART wrapper around the ILA. A simple drop-in replacement for `ila`. Useful if the only
connection to the host PC is an UART connection.
-}
ilaUart ::
  forall dom baud.
  ( HiddenClockResetEnable dom
  , ValidBaud dom baud
  ) =>
  SNat baud ->
  -- | The initial configuration of the ILA
  IlaConfig dom ->
  -- | The ILA circuit but with signals of bits as input and output. These should be directly wired
  -- to the toplevel UART RX and TX pins.
  Circuit
    (CSignal dom Bit)
    (CSignal dom Bit)
ilaUart baud config = circuit $ \rxBit -> do
  (rxByte, txBit) <- uartDf baud -< (txByte, rxBit)

  rxPs <- etherboneDfPacketizer <| holdUntilAck -< rxByte
  txByte <- ps2df -< txPs

  txPs <- ila config -< rxPs

  idC -< txBit
