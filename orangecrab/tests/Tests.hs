{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clash.Prelude
import Hedgehog
import Tests.RingBuffer qualified as RB

main :: IO Bool
main = checkParallel RB.ringBufferTestGroup
