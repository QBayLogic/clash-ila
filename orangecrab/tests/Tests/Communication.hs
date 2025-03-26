module Tests.Communication where

-- NOTE: THESE TESTS ARE STILL WORK IN PROGRESS
-- They do __NOT__ work yet!
-- This file is very incomplete

import Clash.Hedgehog.Sized.Unsigned (genUnsigned)
import Clash.Prelude
import Clash.Sized.Vector (unsafeFromList)

import Protocols
import Protocols.Df qualified as Df
import Protocols.PacketStream

import Data.Data qualified as DD
import Data.List qualified as DL
import Data.List.Split qualified as DLS

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Communication

ps2dfModel ::
  forall byteSize.
  (KnownNat byteSize) =>
  [Maybe (PacketStreamM2S byteSize ())] ->
  [Df.Data (BitVector 8)]
ps2dfModel i = go
 where
  intoBytes :: Maybe (PacketStreamM2S byteSize a) -> [Df.Data (BitVector 8)]
  intoBytes (Just packet) = Df.Data <$> (DL.take (validByteCount $ _last packet) $ toList (_data packet))
  intoBytes Nothing = [Df.NoData]

  validByteCount :: Maybe (Index (byteSize + 1)) -> Int
  validByteCount (Just c) = fromEnum c
  validByteCount (Nothing) = fromEnum (maxBound :: Index (byteSize + 1))

  go = DL.concat (intoBytes <$> i)

createPacket ::
  forall a buffWidth aWidth.
  ( BitPack a
  , aWidth ~ BitSize a `DivRU` 8
  , aWidth * 8 ~ BitSize a
  , KnownNat buffWidth
  , KnownNat aWidth
  , 1 <= buffWidth
  , 1 <= aWidth
  ) =>
  [a] ->
  [Maybe (PacketStreamM2S buffWidth ())]
createPacket input = go
 where
  intoBytes :: a -> Vec aWidth (BitVector 8)
  intoBytes i = bitCoerce i

  -- Just $ PacketStreamM2S {
  -- _abort=False, _meta=i, _data=resize (bitCoerce i), _last=__last}

  go = error "X"

convertionProperty :: Property
convertionProperty = property $ do
  let toSimulate = withClockResetEnable systemClockGen systemResetGen enableGen
  let simOptions = def{resetCycles = 1, ignoreReset = False}

  True === True
