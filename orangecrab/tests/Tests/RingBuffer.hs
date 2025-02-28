{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.RingBuffer where

import Clash.Prelude
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.List as DL
import qualified Data.Maybe as DM

import RingBuffer

data SimpleState a = SSIgnore | SSValue a | SSEnd deriving(Generic, NFDataX)
intoMaybe :: SimpleState a -> Maybe a
intoMaybe SSIgnore = Nothing
intoMaybe SSEnd = Nothing
intoMaybe (SSValue a) = Just a

intoSS :: Maybe a -> SimpleState a
intoSS Nothing = SSIgnore
intoSS (Just a) = SSValue a

isEndSS :: SimpleState a -> Bool
isEndSS SSEnd = True
isEndSS _ = False

simulateDump ::
  forall dom a size . (KnownDomain dom, NFDataX a, KnownNat size, 1 <= size) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Buffer size
  SNat size ->
  -- | Init value for the buffer
  a ->
  -- | Input data
  [Maybe a] ->
  -- | Buffer content
  [a]
simulateDump clk rst en size ini rawInput = go
  where
    -- Pad our input data so we can capture every output signal and ignore the reset clock
    inputList = DL.concat [
        [SSIgnore], -- Reset clock
        intoSS <$> rawInput, -- Our input
        [SSEnd], -- Trigger the read
        DL.replicate (fromInteger $ snatToInteger size) SSIgnore -- Pad the signal so we can read it all
      ]
    input = fromList inputList
    ring = (withClockResetEnable clk rst en ringBuffer) size ini $ (intoMaybe <$> input)
    ringOutput = (withClockResetEnable clk rst en dumpRingBuffer) (isEndSS <$> input) ring
    go = DM.catMaybes $ sampleN (DL.length inputList + 1) ringOutput

expectedDump ::
  -- | Buffer size
  SNat size ->
  -- | Init value for the buffer
  a ->
  -- | Input data
  [Maybe a] ->
  -- | Buffer content
  [a]
expectedDump size ini rawInput = go
  where
    input = DM.catMaybes rawInput
    inputLength = DL.length input
    bufferSize = fromInteger $ snatToInteger size

    case1 = (DL.take inputLength input) DL.++ (DL.replicate (bufferSize - inputLength) ini)
    case2 =
      (DL.take (inputLength - bufferSize) $ DL.drop bufferSize input) DL.++
      (DL.drop (inputLength - bufferSize) $ DL.take bufferSize input)

    go
      | inputLength <= bufferSize = case1
      | otherwise = case2

writeProperty :: Property
writeProperty = property $ do
  initValue <- forAll $ Gen.int (Range.constant 1000 2000)
  randomInput <- forAll $ Gen.list (Range.linear 0 300) $ Gen.maybe $ Gen.int (Range.constant 0 100)

  simulateDump systemClockGen systemResetGen enableGen d50 initValue randomInput === expectedDump d50 initValue randomInput

