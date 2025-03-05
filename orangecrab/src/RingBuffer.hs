{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

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

data ReadingState i = RSIdle | RSReading i | RSDead deriving(Generic, NFDataX)

-- | Read the entire contents of a ring buffer
dumpRingBuffer :: 
  forall dom a size . (HiddenClockResetEnable dom, NFDataX a, KnownNat size, 1 <= size) =>
  -- | Trigger, when set it will reset the readIndex count
  Signal dom Bool ->
  -- | The ring buffer component
  (Signal dom (Index size) -> (Signal dom a, Signal dom (Index (size + 1)))) ->
  -- | Data from the buffer, takes `size + 1` amount of cycles to read out the entire buffer
  -- The first set of data will arive 1 cycles after being triggered
  -- Signal dom (Maybe a)
  Signal dom (Maybe a)
dumpRingBuffer trigger buffer = readValue
  where
    (bufferValue, bufferLength) = buffer $ zeroDefault <$> index

    -- By default, try to read out the first value, doing so saves us a clock cycle
    zeroDefault :: Maybe (Index size) -> Index size
    zeroDefault Nothing = 0
    zeroDefault (Just i) = i

    updateState :: ReadingState (Index size) -> (Bool, Index (size + 1)) -> ReadingState (Index size)
    updateState _ (_, 0) = RSIdle
    updateState RSIdle (True, _) = RSReading 1 -- We already read out the value of index 0
    updateState RSIdle (False, _) = RSIdle
    updateState (RSReading i) (_, len)
      -- If we're at our last value, keep it active for one more cycle but without 'fetching' anything
      | i == (resize $ satSub SatZero len 1) = RSDead
      -- | Special case for when the buffer is of size 1, since we already pre-fetched the value
      -- the prior case will not trigger and will loop infinitely.
      -- We also skip the RSDead state, as this cycle the value is already sent
      | (0 :: Index size) == (resize $ satSub SatZero len 1) = RSIdle
      | otherwise = RSReading $ i + 1
    updateState RSDead _ = RSIdle

    updateOutput :: ReadingState (Index size) -> Maybe (Index size)
    updateOutput RSIdle = Nothing
    updateOutput (RSReading i) = Just i
    updateOutput RSDead = Just 0

    index :: Signal dom (Maybe (Index size))
    index = moore updateState updateOutput RSIdle $ bundle (trigger, bufferLength)

    readValue = mux (isJust <$> index)
      (Just <$> bufferValue)
      (pure Nothing)

