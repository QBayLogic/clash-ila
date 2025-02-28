{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test where

import Test.RingBuffer (writeProperty)

tests :: IO Bool
tests = checkParallel $ Group "Ram" [
    ("write", writeProperty)
  ]

