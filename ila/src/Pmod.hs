{- |
Module      : Pmod
Copyright   : Copyright © 2024 QBayLogic B.V.
License     : MIT
Maintainer  : QBayLogic B.V.
Stability   : experimental
Portability : POSIX

Interface definitions for a selection of Digilent PMOD modules.
-}
module Pmod where

import Clash.Prelude

{- | Digilent Pmod LED
https://digilent.com/reference/pmod/pmodled/start
-}
data PmodLED = PmodLED
  { pmodLED3 :: "7" ::: Bool
  , pmodLED2 :: "6" ::: Bool
  , pmodLED1 :: "5" ::: Bool
  , pmodLED0 :: "4" ::: Bool
  }
  deriving (Generic, NFDataX, BitPack)

{- | Digilent Pmod 8LD
https://digilent.com/reference/pmod/pmod8ld/start
-}
data Pmod8LD = Pmod8LD
  { pmod8LD7 :: "7" ::: Bool
  , pmod8LD6 :: "6" ::: Bool
  , pmod8LD5 :: "5" ::: Bool
  , pmod8LD4 :: "4" ::: Bool
  , pmod8LD3 :: "3" ::: Bool
  , pmod8LD2 :: "2" ::: Bool
  , pmod8LD1 :: "1" ::: Bool
  , pmod8LD0 :: "0" ::: Bool
  }
  deriving (Generic, NFDataX, BitPack)

{- | Digilent Pmod BTN
https://digilent.com/reference/pmod/pmodbtn/start
-}
data PmodBTN = PmodBTN
  { pmodBTN3 :: "7" ::: Bool
  , pmodBTN2 :: "6" ::: Bool
  , pmodBTN1 :: "5" ::: Bool
  , pmodBTN0 :: "4" ::: Bool
  }
  deriving (Generic, NFDataX, BitPack)

{- | A class for mapping large pmod modules to smaller ones, whose
pins match in such a way that the larger module can be connected to
a smaller connector.
-}
class Reduce a b where
  reduce :: a -> b
  default reduce ::
    (BitPack a, BitPack b, 4 <= BitSize a, 4 ~ BitSize b) =>
    a ->
    b
  reduce = unpack . slice @_ @_ @(BitSize a - 4) d3 d0 . pack

instance Reduce Pmod8LD PmodLED where
  reduce = unpack . slice d7 d4 . pack
