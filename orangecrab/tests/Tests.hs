{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clash.Prelude
import Hedgehog
import Tests.Packet qualified as P
import Tests.RingBuffer qualified as RB

main :: IO Bool
main =
  ( checkParallel
      $ Group
        "RingBuffer"
        [ ("ReadWrite", RB.writeProperty)
        ]
  )
    .&&. ( checkParallel
            $ Group
              "Packets"
              [ ("DataPacketStructure", P.structureProperty)
              ]
         )
