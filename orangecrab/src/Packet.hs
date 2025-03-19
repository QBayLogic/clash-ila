module Packet where

import Clash.Prelude
import Data.Maybe qualified as DM
import Data.Proxy
import Protocols
import Protocols.Df qualified as Df
import Protocols.PacketStream

-- ILA packet structures
--
-- Shared in every packet;
-- Size (in bytes): | 4 | 2 |
-- Type:            | P | T |
-- Description:
--   P: Preamble, used to find new packets if an error has accured, should always be `0xea88eacd`
--   T: Packet type
--
--
-- # ILA Data Packet
-- Version 1
--
-- Size (in bytes): | 2 | 2 | 2 | 4 | ... |
-- Type:            | V | I | W | L |  D  |
-- Packet type: 0x0001
-- Description:
--   V: Version number, for this version it should be `0x0001`
--   I: Data ID, an identifier for the Clash `signal` being sampled
--   W: Data width, specifies the width of the data in bits, note that data MUST be byte aligned
--   L: Length of the data stream in bytes
--   D: The data from the ILA, length specified by L in chunks of bytes, each bit is a logical level of a pin
-- NOTES:
--  Data ID only gives enough information to differentiate between different Clash `Signal`s. To know
--  from which ILA the data id comes from. Another type of packet will determine which IDs belong to
--  which ILAs.

data IlaDataPacket = IlaDataPacket
  { _preamble :: BitVector 32,
    _type :: BitVector 16,
    _version :: BitVector 16,
    _id :: BitVector 16,
    _width :: BitVector 16,
    _length :: BitVector 32
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

-- | Construct a data packet from a stream of raw data
dataPacket ::
  forall dom dataWidth size t.
  ( HiddenClockResetEnable dom,
    KnownNat dataWidth,
    KnownNat size,
    BitPack t,
    1 <= dataWidth,
    1 <= size
  ) =>
  Proxy t ->
  -- | Circuit which takes in a datastream with the length as metadata and outputs packaged data
  Circuit
    (PacketStream dom dataWidth (Index size))
    (PacketStream dom dataWidth ())
dataPacket _ = packetizerC metaTransfer headerTransfer
  where
    metaTransfer _ = ()
    headerTransfer oldMeta =
      IlaDataPacket
        { _preamble = 0xea88eacd,
          _type = 0x0000,
          _version = 0x0001,
          _id = 0x0000,
          _width = natToNum @(BitSize t),
          _length = (natToNum @(BitSize t `DivRU` 8)) * (resize $ pack oldMeta)
        }

-- | Converts `PacketStream` into a `Df` of bytes. For packet stream data larger than one byte,
-- it will send out bytes in most-significant-byte order.
--
-- NOTE: Please do properly follow the PacketStream standard and keep sending the same data until
-- `_ready` is raised. Not doing so will cause issues.
ps2df ::
  forall dom dataWidth a.
  ( HiddenClockResetEnable dom,
    KnownNat dataWidth,
    1 <= dataWidth
  ) =>
  -- | The input `PacketStream` and the output `Df`, will only set `_ready` to true once every
  -- byte in PacketStream has been sent over in Df
  Circuit (PacketStream dom dataWidth a) (Df dom (BitVector 8))
ps2df = Circuit exposeIn
  where
    exposeIn (incoming, backpressure) = out
      where
        oldIndex :: Signal dom (Index dataWidth)
        oldIndex = register 0 index
        index :: Signal dom (Index dataWidth)
        index =
          mux
            (DM.isNothing <$> incoming)
            (pure 0)
            $ mux
              (pure (Ack False) .==. backpressure)
              oldIndex
              (satAdd SatWrap 1 <$> oldIndex)

        convert _ Nothing = Df.NoData
        convert i (Just m2s) = Df.Data $ _data m2s !! i

        out =
          ( PacketStreamS2M <$> (liftA2 (\i b -> i == maxBound && b == Ack True) oldIndex backpressure),
            liftA2 convert index incoming
          )
