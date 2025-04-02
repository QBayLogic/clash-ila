{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Packet where

import Clash.Prelude
import ConfigGen
import Data.Maybe qualified as DM
import Data.Proxy
import Protocols
import Protocols.PacketStream

data IlaIncomingPacket = IlaResetTrigger | IlaChangeTriggerPoint (BitVector 32)
  deriving (Generic, NFDataX, BitPack, Eq, Show)

class IlaPacketType a where
  ilaPacketType :: a -> BitVector 16

{- | Common ILA packet structure

Shared in every packet;
Size (in bytes): | 4 | 1 |
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

data DepacketizeState
  = DSIdle
  | DSPreamble (Index 3)
  | DSType
  | DSData (IlaIncomingPacket, Index 64)
  deriving (Generic, NFDataX)

{- | Deserialize a raw byte stream into a packet, if possible
Currently only supports the IlaResetTrigger, but will support more in the future
-}
deserializeToPacket ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Circuit
    (CSignal dom (Maybe (BitVector 8)))
    (CSignal dom (Maybe IlaIncomingPacket))
deserializeToPacket = Circuit exposeIn
 where
  exposeIn (byte, _) = out
   where
    transfer ::
      DepacketizeState -> Maybe (BitVector 8) -> (DepacketizeState, Maybe IlaIncomingPacket)
    transfer state Nothing = (state, Nothing)
    transfer DSIdle (Just 0xea) = (DSPreamble 0, Nothing)
    transfer DSIdle (Just _) = (DSIdle, Nothing)
    transfer (DSPreamble 0) (Just 0x88) = (DSPreamble 1, Nothing)
    transfer (DSPreamble 1) (Just 0xea) = (DSPreamble 2, Nothing)
    transfer (DSPreamble 2) (Just 0xcd) = (DSType, Nothing)
    transfer (DSPreamble _) (Just _) = (DSIdle, Nothing)
    -- Parse IlaResetTrigger
    transfer DSType (Just 0x02) = (DSIdle, Just IlaResetTrigger)
    -- Parse IlaChangeTriggerPoint
    transfer DSType (Just 0x03) = (DSData (IlaChangeTriggerPoint 0, 3), Nothing)
    transfer (DSData ((IlaChangeTriggerPoint trigPoint), 0)) (Just by) =
      (DSIdle, Just $ IlaChangeTriggerPoint (shiftL trigPoint 8 .|. resize by))
    transfer (DSData ((IlaChangeTriggerPoint trigPoint), i)) (Just by) =
      (DSData (IlaChangeTriggerPoint (shiftL trigPoint 8 .|. resize by), i - 1), Nothing)
    -- Catch-all cases
    transfer (DSData _) (Just _) = (DSIdle, Nothing)
    transfer DSType (Just _) = (DSIdle, Nothing)

    packet :: Signal dom (Maybe IlaIncomingPacket)
    packet = mealy transfer DSIdle byte

    out = (pure (), packet)

