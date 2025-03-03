{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module RingBuffer where

import Clash.Prelude

-- | A non-conventional ring buffer, capable of writing data to the tail of the buffer, overwriting old data
-- Can read from any point within the buffer specified by the index
ringBuffer ::
  forall dom a size . (HiddenClockResetEnable dom, NFDataX a, KnownNat size, 1 <= size) =>
  -- | The size of the circle buffer
  SNat size ->
  -- | Initial value to use with 
  a ->
  -- | Clear the buffer
  Signal dom Bool ->
  -- | Value to push onto the buffer
  Signal dom (Maybe a) ->
  -- | Index in the circle buffer to read from
  Signal dom (Index size) ->
  -- | Value stored at the index specified
  -- NOTE: Reads are delayed by 1 cycle
  (Signal dom a, Signal dom (Index size))
ringBuffer size ini clear writeData readIndex =
  (blockRam1 NoClearOnReset size ini readAddr ramWrite, getLength)
  where
    getHeadTail :: Signal dom (Index size, Index size)
    getHeadTail = register (0, 0) $ newHeadTail <$> bundle (getHeadTail, writeData, getLength)

    getLength :: Signal dom (Index size)
    getLength = register 0 $ mux clear (pure 0) $ newLength <$> bundle (getLength, writeData)

    newLength (old, Nothing) = old
    newLength (old, Just _) = satAdd SatBound old 1

    newHeadTail (old, Nothing, _) = old
    newHeadTail ((_, oldTail), Just _, len) = (newHead, newTail)
      where
        newTail = satAdd SatWrap oldTail 1

        newHead
          -- If we're overflowing, move the head to match the tail
          -- Otherwise, be zero
          | len == maxBound = newTail
          | otherwise = 0

    readAddr :: Signal dom (Index size)
    readAddr = (uncurry $ satAdd SatWrap) <$> bundle (fst $ unbundle getHeadTail, readIndex)

    ramWrite :: Signal dom (Maybe (Index size, a))
    ramWrite = (\(addr, write) ->
        case write of
          Nothing -> Nothing
          Just value -> Just (addr, value)
      ) <$> bundle (snd $ unbundle getHeadTail, writeData)

-- | Read the entire contents of a ring buffer
dumpRingBuffer :: 
  forall dom a size . (HiddenClockResetEnable dom, NFDataX a, KnownNat size, 1 <= size) =>
  -- | Trigger, when set it will reset the readIndex count
  Signal dom Bool ->
  -- | The ring buffer component
  (Signal dom (Index size) -> (Signal dom a, Signal dom (Index size))) ->
  -- | Data from the buffer, takes `size + 2` amount of cycles to read out the entire buffer
  -- The first set of data will arive 2 cycles after being triggered
  Signal dom (Maybe a)
dumpRingBuffer trigger buffer = readValue
  where
    readIndex :: Signal dom (Maybe (Index size))
    readIndex = register Nothing $
      mux
        trigger
        (pure $ Just 0) $
        mux
          (readIndex .==. pure (Just maxBound))
          (pure Nothing) $
          (fmap (+1)) <$> readIndex

    -- Compensate for the 1 clock cycle delay from reading
    -- The last clock cycle needs to be repeated for this reason
    -- And the first needs to be ignored
    indexDelay Nothing = 0
    indexDelay (Just v)
      | v == maxBound = v
      | otherwise = v + 1

    readValue :: Signal dom (Maybe a)
    readValue = mux
      (readIndex .==. pure Nothing)
      (pure Nothing)
      (Just <$> (fst $ buffer $ indexDelay <$> readIndex))

