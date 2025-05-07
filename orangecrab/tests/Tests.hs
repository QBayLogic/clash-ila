{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clash.Prelude
import Hedgehog
import Tests.RingBuffer qualified as RB
import Tests.Communication qualified as COM

main :: IO Bool
main = checkParallel COM.communicationTestGroup .&&. checkParallel RB.ringBufferTestGroup
