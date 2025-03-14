
module Packet where

import Clash.Prelude


import Protocols
import Protocols.Wishbone
import Protocols.PacketStream
import qualified Protocols.Df as Df

import qualified Data.Maybe as DM
import qualified Data.List as DL

import Data.Proxy

-- ILA packet structures
--
-- # ILA Data Packet
-- Version 1
--
-- Size (in bytes): | 2 | 2 | 2 | 2 | 4 | ... |
-- type:            | T | V | I | W | L |  D  |
-- Description:
--   T: Packet type, for this type it should be `0x0000`
--   V: Version number, for this version it should be `0x00000001`
--   I: Data ID, an identifier for the Clash `signal` being sampled
--   W: Data width, specifies the width of the data in bits, note that data MUST be byte aligned
--   L: Length of the data stream in bytes
--   D: The data from the ILA, length specified by L in chunks of bytes, each bit is a logical level of a pin
-- NOTES:
--  Data ID only gives enough information to differentiate between different Clash `Signal`s. To know
--  from which ILA the data id comes from. Another type of packet will determine which IDs belong to
--  which ILAs.

data IlaDataPacket = IlaDataPacket {
  _type :: BitVector 16,
  _version :: BitVector 16,
  _id :: BitVector 16,
  _width :: BitVector 16,
  _length :: BitVector 32
} deriving(Generic, NFDataX, BitPack)

-- | Construct a data packet from a stream of raw data
dataPacket ::
  forall dom dataWidth size t .
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
    (PacketStream dom dataWidth (Index size))
    (PacketStream dom dataWidth ())
dataPacket _ = packetizerC metaTransfer headerTransfer
 where
  metaTransfer _ = ()
  headerTransfer oldMeta = IlaDataPacket {
      _type = 0x0000,
      _version = 0x0001,
      _id = 0x0000,
      _width = natToNum @(BitSize t),
      _length = resize $ pack oldMeta
    }

