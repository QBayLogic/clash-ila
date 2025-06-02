{-# LANGUAGE OverloadedStrings #-}

module Tests.Communication where

import Clash.Hedgehog.Sized.Unsigned (genUnsigned)
import Clash.Prelude as CP
import Clash.Sized.Vector (fromList, unsafeFromList)

import Protocols
import Protocols.Df qualified as Df
import Protocols.PacketStream

import Data.Data qualified as DD
import Data.List qualified as DL
import Data.List.Split qualified as DLS
import Data.Maybe qualified as DM
import Prelude as P

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Communication

depacketizeProperty :: Property
depacketizeProperty = property $ do
  let
    wcre = withClockResetEnable systemClockGen systemResetGen enableGen
    simOptions = def{resetCycles = 0, ignoreReset = False}

    -- Etherbone settings & flags
    ebMagicMsb = 0x4e
    ebMagicLsb = 0x6f
    ebVersion = 0x10
    ebPortAddrSize = 0x44
    ebFlags = 0x00
    ebByteSelect = 0x0f

    bytesPerWord = 4

    commonBytes =
      [ ebMagicMsb
      , ebMagicLsb
      , ebVersion
      , ebPortAddrSize
      , ebFlags
      , ebByteSelect
      ] ::
        [BitVector 8]

  readsInt <- forAll $ Gen.list (Range.linear 0 255) $ Gen.int (Range.constant 0 65536)
  writesInt <- forAll $ Gen.list (Range.linear 0 255) $ Gen.int (Range.constant 0 65536)

  let
    intoBytes' :: BitVector 32 -> Vec 4 (BitVector 8)
    intoBytes' = unpack
    intoBytes = toList . intoBytes'

    reads = P.concatMap intoBytes (fromIntegral <$> readsInt :: [BitVector 32]) :: [BitVector 8]
    writes = P.concatMap intoBytes (fromIntegral <$> writesInt :: [BitVector 32]) :: [BitVector 8]

    -- \| Prefixes the address to the to reads/writes, if there are any
    prefixAddr input
      | P.length input == 0 = input
      | otherwise = [0xff, 0xff, 0xff, 0xff] P.++ input

    readWriteLength :: [BitVector 8]
    readWriteLength =
      [ fromIntegral $ P.length reads `div` bytesPerWord
      , fromIntegral $ P.length writes `div` bytesPerWord
      ]

    byteStream = commonBytes P.++ readWriteLength P.++ prefixAddr reads P.++ prefixAddr writes

    toPS' bytes isLast =
      PacketStreamM2S
        { _abort = False
        , _last = if isLast then Just . fromIntegral $ P.length bytes else Nothing
        , _meta = ()
        , _data = unsafeFromList @4 (bytes P.++ P.replicate (bytesPerWord - P.length bytes) 0x00)
        }

    toPS [] = []
    toPS [remaining] = [toPS' remaining True]
    toPS (x : xs) = toPS' x False : toPS xs

    expected = toPS $ DLS.chunksOf bytesPerWord byteStream
    simulated =
      P.take (P.length expected) $
        DM.catMaybes $
          simulateC (wcre etherboneDfPacketizer) simOptions (Df.Data <$> byteStream)

  simulated === expected

communicationTestGroup :: Group
communicationTestGroup =
  Group
    "Communication"
    [ ("Depacketization", depacketizeProperty)
    ]
