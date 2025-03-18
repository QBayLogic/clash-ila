
module Tests.Packet where

import Clash.Prelude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Data.Data as DD
import qualified Data.List.Split as DLS

import Clash.Sized.Vector (unsafeFromList)
import Clash.Hedgehog.Sized.Unsigned (genUnsigned)

import Protocols
import Protocols.PacketStream

import Packet

splitIntoBytes :: (BitPack a) => a -> Vec (BitSize a `DivRU` 8) (BitVector 8)
splitIntoBytes bv = unpack . resize $ pack bv

dataPackedModel ::
  [[Unsigned 32]] ->
  [Maybe (PacketStreamM2S 4 ())]
dataPackedModel i = out
 where
  header :: BitVector 32 -> [BitVector 8]
  header len =
    -- Preamble
    [ 0xea, 0x88, 0xea, 0xcd ] DL.++
    -- Type
    [ 0x00, 0x00 ] DL.++
    -- Version
    [ 0x00, 0x01 ] DL.++
    -- Id
    [ 0x00, 0x00 ] DL.++
    -- Width
    [ 0x00, 0x20 ] DL.++
    -- Length
    (toList $ splitIntoBytes len)

  byteSequence w =
    header (resize . pack $ (DL.length w) * 4) DL.++
    (DL.concat $ toList . splitIntoBytes <$> w)

  padBytes (isLast, bytes)
    | DL.length bytes < 4 = PacketStreamM2S {
        _data = unsafeFromList $ bytes DL.++ (DL.replicate (4 - DL.length bytes) undefined),
        _last = Just $ unpack . resize . pack $ DL.length bytes,
        _meta = (),
        _abort = False
      }
    | otherwise = PacketStreamM2S {
        _data = unsafeFromList bytes,
        _last = if isLast then Just 4 else Nothing,
        _meta = (),
        _abort = False
      }

  packetize :: [Unsigned 32] -> [PacketStreamM2S 4 ()]
  packetize w = go
   where
    chopped = (DLS.chunksOf 4 $ byteSequence w)
    isLast = (== DL.length chopped - 1) <$> [0..DL.length chopped - 1]

    go = padBytes <$> DL.zip isLast chopped

  packet [] = [Nothing]
  packet w = Nothing : (Just <$> packetize w) DL.++ [Nothing]

  out = [Nothing] DL.++ DL.concatMap packet i

-- | Will generate valid list of PacketStreams given a list of values to package in the IlaDataPacket format
-- NOTE: Make sure the list does NOT exceed the maximum!
testbenchDataPacket ::
  [[Unsigned 32]] ->
  [Maybe (PacketStreamM2S 4 (Index 25))]
testbenchDataPacket i = go
 where
  toPacket :: Index 25 -> (Index 25, Unsigned 32) -> PacketStreamM2S 4 (Index 25)
  toPacket len (idx, num) = PacketStreamM2S {
      _abort = False,
      _meta = len,
      _last = if idx == len - 1 then Just 4 else Nothing,
      _data = splitIntoBytes num
    }

  convList :: [Unsigned 32] -> [PacketStreamM2S 4 (Index 25)]
  convList l = toPacket (unpack . resize . pack $ DL.length l) <$> DL.zip [0..] l

  package :: [Unsigned 32] -> [Maybe (PacketStreamM2S 4 (Index 25))]
  package l = (Just <$> convList l) DL.++ [Nothing]

  go :: [Maybe (PacketStreamM2S 4 (Index 25))]
  go = DL.concatMap package i

structureProperty :: Property
structureProperty = property $ do
  input <- forAll $ Gen.list (Range.linear 0 100) $
    Gen.list (Range.linear 0 24) (genUnsigned @32 $ Range.linear 0 4_000_000_000)

  let toSimulate = withClockResetEnable systemClockGen systemResetGen enableGen
  let simOptions = def { resetCycles = 1, ignoreReset = False }
  let expected = dataPackedModel input
  let simulated = DL.take (DL.length expected) $ simulateC (toSimulate dataPacket (DD.Proxy :: DD.Proxy (Unsigned 32))) simOptions (testbenchDataPacket input)

  simulated === expected

