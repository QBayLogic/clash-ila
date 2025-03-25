{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tests.Packet where

import Clash.Hedgehog.Sized.Unsigned (genUnsigned)
import Clash.Prelude
import Clash.Sized.Vector (unsafeFromList)
import Data.Data qualified as DD
import Data.List qualified as DL
import Data.List.Split qualified as DLS
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Packet
import Protocols
import Protocols.PacketStream

splitIntoBytes :: (BitPack a) => a -> Vec (BitSize a `DivRU` 8) (BitVector 8)
splitIntoBytes bv = unpack . resize $ pack bv

splitIntoBv16 :: (BitPack a) => a -> Vec 2 (BitVector 8)
splitIntoBv16 bv = unpack . resize $ pack bv

splitIntoBv32 :: (BitPack a) => a -> Vec 4 (BitVector 8)
splitIntoBv32 bv = unpack . resize $ pack bv

-- | Ideal scenario for PacketStreams
dataPackedModel ::
  forall a.
  (BitPack a) =>
  [[a]] ->
  [Maybe (PacketStreamM2S (BitSize a `DivRU` 8) IlaDataPacket)]
dataPackedModel i = out
 where
  widthInBytes :: Int
  widthInBytes = natToNum @(BitSize a `DivRU` 8)
  width :: Int
  width = natToNum @(BitSize a)

  header :: BitVector (BitSize a) -> [BitVector 8]
  header len =
    -- Version
    [0x00, 0x01]
      DL.++
      -- Id
      [0x00, 0x00]
      DL.++
      -- Width
      (toList $ splitIntoBv16 width)
      DL.++
      -- Length
      (toList $ splitIntoBv32 len)

  byteSequence w =
    header (resize . pack $ (DL.length w) * widthInBytes)
      DL.++ (DL.concat $ toList . splitIntoBytes <$> w)

  padBytes ::
    IlaDataPacket ->
    (Bool, [BitVector 8]) ->
    PacketStreamM2S (BitSize a `DivRU` 8) IlaDataPacket
  padBytes meta (isLast, bytes)
    | DL.length bytes < widthInBytes =
        PacketStreamM2S
          { _data =
              unsafeFromList $ bytes DL.++ (DL.replicate (widthInBytes - DL.length bytes) undefined)
          , _last = Just $ unpack . resize . pack $ DL.length bytes
          , _meta = meta
          , _abort = False
          }
    | otherwise =
        PacketStreamM2S
          { _data = unsafeFromList bytes
          , _last = if isLast then Just (unpack . resize . pack $ widthInBytes) else Nothing
          , _meta = meta
          , _abort = False
          }

  packetize :: [a] -> [PacketStreamM2S (BitSize a `DivRU` 8) IlaDataPacket]
  packetize w = go
   where
    chopped = (DLS.chunksOf widthInBytes $ byteSequence w)
    isLast = (== DL.length chopped - 1) <$> [0 .. DL.length chopped - 1]

    go =
      padBytes
        IlaDataPacket
          { length = resize . pack $ DL.length w * widthInBytes
          , width = resize . pack $ width
          , id = 0x0000
          , version = 0x0001
          }
        <$> DL.zip isLast chopped

  packet [] = []
  packet w = Nothing : (Just <$> packetize w)

  out = [Nothing] DL.++ DL.concatMap packet i

{- | Will generate valid list of PacketStreams given a list of values to package in the IlaDataPacket format
NOTE: Make sure the list does NOT exceed the maximum! It may be any other length but it should
not go above the maximum
-}
testbenchDataPacket ::
  forall maxLength a.
  ( BitPack a
  , KnownNat maxLength
  , 1 <= BitSize a
  , 1 <= maxLength
  ) =>
  [[a]] ->
  [Maybe (PacketStreamM2S (BitSize a `DivRU` 8) (BitVector 16, Index maxLength))]
testbenchDataPacket i = go
 where
  widthInBytes :: Int
  widthInBytes = natToNum @(BitSize a `DivRU` 8)

  toPacket ::
    Index maxLength ->
    (Index maxLength, a) ->
    PacketStreamM2S (BitSize a `DivRU` 8) (BitVector 16, Index maxLength)
  toPacket len (idx, num) =
    PacketStreamM2S
      { _abort = False
      , _meta = (0, len)
      , _last = if idx == len - 1 then Just (unpack . resize . pack $ widthInBytes) else Nothing
      , _data = splitIntoBytes num
      }

  convList :: [a] -> [PacketStreamM2S (BitSize a `DivRU` 8) (BitVector 16, Index maxLength)]
  convList l = toPacket (unpack . resize . pack $ DL.length l) <$> DL.zip [0 ..] l

  package :: [a] -> [Maybe (PacketStreamM2S (BitSize a `DivRU` 8) (BitVector 16, Index maxLength))]
  package l = (Just <$> convList l)

  go :: [Maybe (PacketStreamM2S (BitSize a `DivRU` 8) (BitVector 16, Index maxLength))]
  go = DL.concatMap package i

structureProperty :: Property
structureProperty = property $ do
  input <-
    forAll $
      Gen.list (Range.linear 0 100) $
        Gen.list (Range.linear 0 24) (genUnsigned @9 $ Range.linear 0 500)

  let toSimulate = withClockResetEnable systemClockGen systemResetGen enableGen
  let simOptions = def{resetCycles = 1, ignoreReset = False}
  let expected = dataPackedModel input
  let simulated =
        DL.take (DL.length expected) $
          simulateC
            (toSimulate dataPacket (DD.Proxy :: DD.Proxy (Unsigned 9)))
            simOptions
            (testbenchDataPacket @25 input)

  simulated === expected
