{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- | Takes in a bitpackable 'thing' and convert it to its byte representation
convertBytes :: (BitPack a) => a -> Vec (BitSize a `DivRU` 8) (BitVector 8)
convertBytes bv = unpack . resize $ pack bv

-- | Takes in a bitpackable 'thing' and convert it to its word representation
-- Word in this case refers to two bytes (blame x64 for this)
convertWord :: (BitPack a) => a -> Vec 2 (BitVector 8)
convertWord bv = unpack . resize $ pack bv

-- | Takes in a bitpackable 'thing' and convert it to its double representation
-- Double in this case refers to four bytes (blame x64 for this)
convertDouble :: (BitPack a) => a -> Vec 4 (BitVector 8)
convertDouble bv = unpack . resize $ pack bv

-- | Ideal scenario for PacketStreams
dataPackedModel ::
  forall a.
  (BitPack a) =>
  [[a]] ->
  [Maybe (PacketStreamM2S (BitSize a `DivRU` 8) IlaDataHeader)]
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
      [0x00, 0x00, 0x00, 0x00]
      DL.++
      -- Width
      (toList $ convertWord width)
      DL.++
      -- Length
      (toList $ convertDouble len)

  byteSequence w =
    header (resize . pack $ (DL.length w) * widthInBytes)
      DL.++ (DL.concat $ toList . convertBytes <$> w)

  padBytes ::
    IlaDataHeader ->
    (Bool, [BitVector 8]) ->
    PacketStreamM2S (BitSize a `DivRU` 8) IlaDataHeader
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

  packetize :: [a] -> [PacketStreamM2S (BitSize a `DivRU` 8) IlaDataHeader]
  packetize w = go
   where
    chopped = (DLS.chunksOf widthInBytes $ byteSequence w)
    isLast = (== DL.length chopped - 1) <$> [0 .. DL.length chopped - 1]

    go =
      padBytes
        IlaDataHeader
          { length = unpack . resize . pack $ DL.length w * widthInBytes
          , width = unpack . resize . pack $ width
          , hash = 0x00000000
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
  [Maybe (PacketStreamM2S (BitSize a `DivRU` 8) (BitVector 32, Index maxLength))]
testbenchDataPacket i = go
 where
  widthInBytes :: Int
  widthInBytes = natToNum @(BitSize a `DivRU` 8)

  toPacket ::
    Index maxLength ->
    (Index maxLength, a) ->
    PacketStreamM2S (BitSize a `DivRU` 8) (BitVector 32, Index maxLength)
  toPacket len (idx, num) =
    PacketStreamM2S
      { _abort = False
      , _meta = (0, len)
      , _last = if idx == len - 1 then Just (unpack . resize . pack $ widthInBytes) else Nothing
      , _data = convertBytes num
      }

  convList :: [a] -> [PacketStreamM2S (BitSize a `DivRU` 8) (BitVector 32, Index maxLength)]
  convList l = toPacket (unpack . resize . pack $ DL.length l) <$> DL.zip [0 ..] l

  package :: [a] -> [Maybe (PacketStreamM2S (BitSize a `DivRU` 8) (BitVector 32, Index maxLength))]
  package l = (Just <$> convList l)

  go :: [Maybe (PacketStreamM2S (BitSize a `DivRU` 8) (BitVector 32, Index maxLength))]
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

packetTestGroup :: Group
packetTestGroup = Group "Packets"
  [ ("DataPacketStructure", structureProperty)
  ]

