
module Packet where

import Clash.Prelude

import Protocols
import Protocols.Wishbone
import Protocols.PacketStream
import qualified Protocols.Df as Df

import qualified Data.Maybe as DM
import qualified Data.List as DL

-- ILA packet structures
--
-- # ILA Data Packet
-- Version 1
--
-- Size (in bytes): | 4 | 2 | 2 | 2 | 2 | 4 | ... |
-- type:            | V | T | I | Y | O | L |  D  |
-- Description:
--   V: Version number, for this version it should be `0x00000001`
--   T: Packet type, for this type it should be `0x0000`
--   I: ILA ID, this ensures the computer side doesn't apply the packet to the wrong ILA
--   Y: Data ID, data with the same ID belong together in the same word
--   O: Data ordering, specifies the bit position in a word
--   L: Length of the data stream in BITS!
--   D: The data from the ILA, length specified by L in chunks of bytes, each bit is a logical level of a pin

data IlaDataPacket = IlaDataPacket {
  _version :: BitVector 32,
  _length :: BitVector 32
} deriving(Generic, NFDataX, BitPack)

-- | Construct a data packet from a stream of raw data
-- NOTE: this is NOT compliant to the specifications, yet
dataPacket ::
  forall dom dataWidth size .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat size
  , 1 <= dataWidth
  , 1 <= size
  ) =>
  -- | Circuit which takes in a datastream with the length as metadata and outputs packaged data
  Circuit
    (PacketStream dom dataWidth (Index size))
    (PacketStream dom dataWidth ())
dataPacket = packetizerC metaTransfer headerTransfer
 where
  metaTransfer _ = ()
  headerTransfer oldMeta = IlaDataPacket {
      _version = 0x00000001,
      _length = resize $ pack oldMeta
    }

