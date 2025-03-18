{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clash.Prelude
import Hedgehog

import qualified Tests.RingBuffer as RB
import qualified Tests.Packet as P

main :: IO Bool
main = (
  checkParallel $ Group "RingBuffer" [
      ("ReadWrite", RB.writeProperty)
    ]
  ) .&&. (
    checkParallel $ Group "Packets" [
        ("DataPacketStructure", P.structureProperty)
      ]
  )

