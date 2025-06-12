{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Blink where

import Clash.Annotations.TH
import Clash.Cores.UART (ValidBaud)
import Clash.Prelude

import ConfigGen
import Domain
import Ila
import Protocols



-- | Simple UART ILA demonstration
topLogicUart ::
  forall dom baud.
  (HiddenClockResetEnable dom, ValidBaud dom baud) =>
  -- | Baud rate of UART
  SNat baud ->
  -- | RX
  Signal dom Bit ->
  -- | Leds
  Signal dom Bit
topLogicUart baud rx = go
 where
  -- Simple demo signal to 'debug'
  counter0 :: (HiddenClockResetEnable dom) => Signal dom (Unsigned 36)
  counter0 = register 0 $ satAdd SatWrap 1 <$> counter0
  counter1 :: (HiddenClockResetEnable dom) => Signal dom (Signed 12)
  counter1 = register 20 $ satAdd SatWrap 4 <$> counter1
  counter2 :: (HiddenClockResetEnable dom) => Signal dom (Signed 50)
  counter2 = register 40 $ satAdd SatWrap 3 <$> counter2

  Circuit demoIla = ilaUart
    baud
    $ ilaConfig
      (counter0, "+1")
      (counter1, "+4")
      (counter2, "+3")
      WithIlaConfig
        { bufferDepth = d100
        , name = "Simple_Demo"
        , triggerPoint = 0
        , predicates = ilaDefaultPredicates
        }
  txBit = snd $ demoIla (rx, (pure ()))

  go = txBit



-- | The top entity
topEntity ::
  "CLK" ::: Clock Dom48 ->
  "BTN" ::: Reset Dom48 ->
  "PMOD1_6" ::: Signal Dom48 Bit ->
  "PMOD1_5" ::: Signal Dom48 Bit
topEntity clk rst = withClockResetEnable clk rst enableGen (topLogicUart (SNat @9600))

makeTopEntity 'topEntity
