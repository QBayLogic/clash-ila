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
-}
data IlaFinalHeader = IlaFinalHeader
  { preamble :: BitVector 32
  -- ^ P: Preamble, used to find new packets if an error has accured, should always be `0xea88eacd`
  , kind :: BitVector 16
  -- ^ T: Packet type
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

{- | ILA Data Packet
Version 1

The heart of the ILA data communications, this packet contains the raw data captured by the ILA
The captured data will be chopped into bytes and sent over any underlaying network layer

Size (in bytes): | 2 | 2 | 2 | 4 | ... |
Type:            | V | I | W | L |  D  |

NOTE: Data ID only gives enough information to differentiate between different Clash `Signal`s. To know
from which ILA the data id comes from. Another type of packet will determine which IDs belong to
which ILAs.
-}
data IlaDataHeader = IlaDataHeader
  { version :: BitVector 16
  -- ^ V: Version number, for this version it should be `0x0001`
  , id :: BitVector 16
  -- ^ I: Data ID, an identifier for the Clash `signal` being sampled
  , width :: Unsigned 16
  -- ^ W: Data width, specifies the width of the data in bits, note that data MUST be byte aligned
  , length :: Unsigned 32
  -- ^ L: Length of the data stream in bytes
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
      , hash = fst oldMeta
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
