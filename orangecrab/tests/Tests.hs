{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clash.Prelude
import Hedgehog

import Tests.RingBuffer

main :: IO Bool
main = checkParallel $ Group "Ram" [
    ("write", writeProperty)
  ]

