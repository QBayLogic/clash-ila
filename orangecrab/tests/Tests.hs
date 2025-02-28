{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests where

import Clash.Prelude
import Hedgehog

import Tests.RingBuffer

tests :: IO Bool
tests = checkParallel $ Group "Ram" [
    ("write", writeProperty)
  ]

