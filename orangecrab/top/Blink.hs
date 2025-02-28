{-|
Module      : Blink
Copyright   : Copyright © 2024 QBayLogic B.V.
License     : MIT
Maintainer  : QBayLogic B.V.
Stability   : experimental
Portability : POSIX

Blinking RGB led.
-}
module Blink where

import Clash.Annotations.TH
import Clash.Prelude

import Domain
import RGB

topEntity ::
  "CLK" ::: Clock Dom48 ->
  "BTN" ::: Reset Dom48 ->
  "rgb_led0" ::: Signal Dom48 RGB
topEntity clk rst = withClockResetEnable clk rst enableGen
  $ driveRGB $ mealy (~~>) (0 :: Index 48_000_000, 0 :: Index 9) $ pure ()
 where
  (c, s) ~~> () =
    ( ( satSucc SatWrap c
      , if c == 0 then satSucc SatWrap s else s
      )
    , (!! s)
    $ black  :> red    :> green :> blue   :> white :>
      yellow :> orange :> cyan  :> violet :> Nil
    )

makeTopEntity 'topEntity
