
module RingBuffer where

import Clash.Prelude
import Data.Maybe (isJust)

-- | A non-conventional ring buffer, capable of writing data to the tail of the buffer, overwriting old data
-- Can read from any point within the buffer specified by the index
ringBuffer ::
  forall dom a size . (HiddenClockResetEnable dom, NFDataX a, KnownNat size, 1 <= size) =>
  -- | The size of the circle buffer
  SNat size ->
  -- | Initial value to use with 
  a ->
  -- | Clear the buffer
  -- NOTE: any writes during a clear cycle will be discarded
  -- NOTE: a read prior to the clear cycle will fail!
  Signal dom Bool ->
  -- | Value to push onto the buffer
  Signal dom (Maybe a) ->
  -- | Index in the circle buffer to read from
  Signal dom (Index size) ->
  -- | Value stored at the index specified
  -- NOTE: Reads are delayed by 1 cycle
  (Signal dom a, Signal dom (Index (size + 1)))
ringBuffer size ini clear writeData readIndex =
  (blockRam1 NoClearOnReset size ini readAddr ramWrite, getLength)
  where
    getHeadTail :: Signal dom (Index size, Index size)
    getHeadTail = register (0, 0) $
      mux clear
        (pure (0, 0)) $
        liftA3 newHeadTail getHeadTail writeData getLength

    getLength :: Signal dom (Index (size + 1))
    getLength = register 0 $
      mux clear
        (pure 0) $
        newLength <$> bundle (getLength, writeData)

    newLength (old, Nothing) = old
    newLength (old, Just _) = satAdd SatBound old 1

    newHeadTail old Nothing _ = old
    newHeadTail (_, oldTail) (Just _) len = (newHead, newTail)
      where
        newTail = satAdd SatWrap oldTail 1

        newHead
          -- If we're overflowing, move the head to match the tail
          -- Otherwise, be zero
          | len == maxBound = newTail
          | otherwise = 0

    readAddr :: Signal dom (Index size)
    readAddr = liftA2 (satAdd SatWrap) (fst $ unbundle getHeadTail) readIndex

    ramWrite :: Signal dom (Maybe (Index size, a))
    ramWrite = liftA2 (\addr write -> (fmap (addr,) write)) (snd $ unbundle getHeadTail) writeData

-- | A testbench for the ring buffer, capable of reading out the entire content of the ringBuffer
testbenchRingBuffer :: 
  forall dom a size . (HiddenClockResetEnable dom, NFDataX a, KnownNat size, 1 <= size) =>
  -- | Every clock this is active, it will output one value from the buffer
  -- If all values are read, the output will be `Nothing`
  Signal dom Bool ->
  -- | The ring buffer component
  (Signal dom (Index size) -> (Signal dom a, Signal dom (Index (size + 1)))) ->
  -- | Data from the buffer, takes `size + 1` amount of cycles to read out the entire buffer
  -- The first set of data will arive 1 cycles after being triggered
  Signal dom (Maybe a)
testbenchRingBuffer active buffer = readValue
  where
    (bufferValue, bufferLength) = buffer index

    -- | Sync the activity with the delay from the RAM read
    delayedActive = register False active
    -- | The output value is always invalid (thus should be Nothing) if we already wrote all bytes
    -- or the buffer has no content
    valueValid = delayedActive .&&. (bufferLength ./=. pure 0) .&&. writeCount ./=. bufferLength

    -- | Keep track of how many items we read out
    writeCount :: Signal dom (Index (size + 1))
    writeCount = register 0 $ liftA2 newWriteCount valueValid writeCount

    newWriteCount :: Bool -> Index (size + 1) -> Index (size + 1)
    newWriteCount True count = count + 1
    newWriteCount False count = count

    index = register 0 $ liftA3 newIndex active index bufferLength

    newIndex True i len
      | i == (resize $ satSub SatZero len 1) = i
      | otherwise = i + 1
    newIndex False i _ = i

    readValue = mux valueValid
      (Just <$> bufferValue)
      (pure Nothing)

