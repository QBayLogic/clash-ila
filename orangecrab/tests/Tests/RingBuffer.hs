module Tests.RingBuffer where

import Clash.Prelude
import Data.List qualified as DL
import Data.Maybe qualified as DM
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import RingBuffer

-- | Define the start/middle/end of a sequence, making sequences testing easier
data SequenceState a = SSNothing | SSValue a | SSInputEnd deriving (Generic, NFDataX)

intoMaybe :: SequenceState a -> Maybe a
intoMaybe SSNothing = Nothing
intoMaybe SSInputEnd = Nothing
intoMaybe (SSValue a) = Just a

intoSS :: Maybe a -> SequenceState a
intoSS Nothing = SSNothing
intoSS (Just a) = SSValue a

isEndSS :: SequenceState a -> Bool
isEndSS SSInputEnd = True
isEndSS _ = False

-- | Simulate feeding input into the ring buffer
simulateDump ::
  forall dom a size.
  (HiddenClockResetEnable dom, NFDataX a, KnownNat size, 1 <= size) =>
  -- | Buffer size
  SNat size ->
  -- | Init value for the buffer
  a ->
  -- | Input data
  [Maybe a] ->
  -- | Buffer content
  [a]
simulateDump size ini rawInput = go
 where
  -- Pad our input data so we can capture every output signal and ignore the reset clock
  inputList =
    DL.concat
      [ [SSNothing] -- Reset clock
      , intoSS <$> rawInput -- Our input
      , [SSInputEnd] -- Trigger the read
      , DL.replicate (fromInteger $ snatToInteger size) SSInputEnd -- Pad out the signal so we can read the input back
      ]
  input = fromList inputList
  ring = ringBuffer size ini (pure False) (intoMaybe <$> input)
  ringOutput = testbenchRingBuffer (isEndSS <$> input) ring
  go = DM.catMaybes $ sampleN (DL.length inputList + 1) ringOutput

{- | Expected output of the ring buffer
There are two cases to consider;
 1. Our input is smaller than or equal to the buffer
 2. Our input is bigger than the buffer
In case 1, the expected output is the same as the input
In case 2, the expected output are the last inserted values
-}
expectedDump ::
  -- | Buffer size
  SNat size ->
  -- | Input data
  [Maybe a] ->
  -- | Buffer content
  [a]
expectedDump size rawInput = go
 where
  input = DM.catMaybes rawInput
  inputLength = DL.length input
  bufferSize = fromInteger $ snatToInteger size

  case1 = (DL.take inputLength input)
  case2 = DL.drop (inputLength - bufferSize) input

  go
    | inputLength <= bufferSize = case1
    | otherwise = case2

{- | Tests for writing to the buffer
As a consequence, it also tests reading of the buffer
-}
writeProperty :: Property
writeProperty = property $ do
  initValue <- forAll $ Gen.int (Range.constant 1000 2000)
  randomInput <-
    forAll $ Gen.list (Range.linear 0 300) $ Gen.maybe $ Gen.int (Range.constant 0 100)

  let actual =
        withClockResetEnable @System
          clockGen
          resetGen
          enableGen
          simulateDump
          d50
          initValue
          randomInput
      expected = expectedDump d50 randomInput

  actual === expected
