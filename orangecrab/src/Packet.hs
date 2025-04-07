{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Packet where

import Clash.Prelude
import Data.Maybe qualified as DM
import Data.Proxy
import Protocols
import Protocols.PacketStream

class IlaPacketType a where
  ilaPacketType :: a -> BitVector 16

{- | Common ILA packet structure

Shared in every packet;
Size (in bytes): | 4 | 2 |
Type:            | P | T |
Description:
  P: Preamble, used to find new packets if an error has accured, should always be `0xea88eacd`
  T: Packet type
-}
data IlaFinalHeader = IlaFinalHeader
  { preamble :: BitVector 32
  , kind :: BitVector 16
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

{- | ILA Display Packet
Version 1

An empty packet, merely indicating to the system it should display whatever signals the computer
has received

Size (in bytes): none
Type:            none
Packet type: 0x0002
Description:
   none
-}
data IlaDisplayPacket = IlaDisplayPacket
  {}
  deriving (Generic, NFDataX, BitPack, Eq, Show)

instance IlaPacketType IlaDisplayPacket where
  ilaPacketType _ = 2

-- | A circuit for creating an `IlaDisplayPacket`
ilaDisplayPacket ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | The input is a trigger which generates an IlaDisplayPacket on the output until it receives
  -- backpressure. The trigger only needs to be set for one clock cycle.
  Circuit
    (CSignal dom Bool)
    (PacketStream dom 0 IlaDisplayPacket)
ilaDisplayPacket = Circuit exposeIn
 where
  exposeIn (trigger, backpressure) = out
   where
    packet True =
      Just $
        PacketStreamM2S
          { _meta = IlaDisplayPacket
          , _last = Just 0
          , _abort = False
          , _data = Nil
          }
    packet False = Nothing

    oldTriggered = register False triggered
    triggered = (trigger .||. oldTriggered) .&&. (not . _ready <$> backpressure)

    out = (pure (), packet <$> triggered)

{- | ILA Data Packet
Version 1

The heart of the ILA data communications, this packet contains the raw data captured by the ILA
The captured data will be chopped into bytes and sent over any underlaying network layer

Size (in bytes): | 2 | 2 | 2 | 4 | ... |
Type:            | V | I | W | L |  D  |
Packet type: 0x0001
Description:
  V: Version number, for this version it should be `0x0001`
  I: Data ID, an identifier for the Clash `signal` being sampled
  W: Data width, specifies the width of the data in bits, note that data MUST be byte aligned
  L: Length of the data stream in bytes
  D: The data from the ILA, length specified by L in chunks of bytes, each bit is a logical level of a pin
NOTES:
 Data ID only gives enough information to differentiate between different Clash `Signal`s. To know
 from which ILA the data id comes from. Another type of packet will determine which IDs belong to
 which ILAs.
-}
data IlaDataHeader = IlaDataHeader
  { version :: BitVector 16
  , id :: BitVector 16
  , width :: Unsigned 16
  , length :: Unsigned 32
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

instance IlaPacketType IlaDataHeader where
  ilaPacketType _ = 1

-- | Construct a data packet from a stream of raw data
dataPacket ::
  forall dom dataWidth size t.
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat size
  , BitPack t
  , 1 <= dataWidth
  , 1 <= size
  ) =>
  Proxy t ->
  -- | Circuit which takes in a datastream with the length as metadata and outputs packaged data
  Circuit
    (PacketStream dom dataWidth (BitVector 16, Index size))
    (PacketStream dom dataWidth IlaDataHeader)
dataPacket _ = packetizerC headerTransfer headerTransfer
 where
  headerTransfer oldMeta =
    IlaDataHeader
      { version = 0x0001
      , id = fst oldMeta
      , width = natToNum @(BitSize t)
      , length = (natToNum @(BitSize t `DivRU` 8)) * (unpack . resize . pack $ snd oldMeta)
      }

{- | Finalize a ILA packet

Prepends a preamble and the packet type to any form of ILA packet and erases the specific packet
type from the metadata. This packet can now be sent over any transport medium to the host
computer and be understood by the ILA software.
-}
finalizePacket ::
  forall dom dataWidth packet.
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , IlaPacketType packet
  , 1 <= dataWidth
  ) =>
  Circuit
    (PacketStream dom dataWidth packet)
    (PacketStream dom dataWidth IlaFinalHeader)
finalizePacket = packetizerC headerTransfer headerTransfer
 where
  headerTransfer packet =
    IlaFinalHeader
      { preamble = 0xea88eacd
      , kind = ilaPacketType packet
      }
