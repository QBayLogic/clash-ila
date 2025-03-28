{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Blink where

import Clash.Annotations.TH
import Clash.Cores.UART (ValidBaud)
import Clash.Prelude

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Maybe qualified as DM

import Communication
import ConfigGen
import Data.HList
import Domain
import Ila
import Pmod
import Protocols

-- | Resets the ILA trigger whenever we receive an incoming byte from UART
triggerResetUart ::
  (HiddenClockResetEnable dom) =>
  Circuit (CSignal dom (Maybe (BitVector 8))) (CSignal dom Bool)
triggerResetUart = Circuit exposeIn
 where
  exposeIn (incoming, _) = out
   where
    out = (pure (), DM.isJust <$> incoming)

-- | Set the ILA trigger reset state based on the button 0 (TRUE) and button 1 (FALSE)
triggerResetButtons ::
  (HiddenClockResetEnable dom) =>
  Circuit (CSignal dom (BitVector 4)) (CSignal dom Bool)
triggerResetButtons = Circuit exposeIn
 where
  exposeIn (incoming, _) = out
   where
    reg = register False $ liftA2 change incoming reg

    change 1 _ = True
    change 2 _ = False
    change _ r = r

    out = (pure (), reg)

topLogicUart ::
  forall dom baud.
  (HiddenClockResetEnable dom, ValidBaud dom baud) =>
  -- | Baud rate of UART
  SNat baud ->
  -- | Buttons
  Signal dom (BitVector 4) ->
  -- | RX
  Signal dom Bit ->
  -- | TX
  Signal dom Bit
topLogicUart baud btns rx = go
 where
  -- Simple demo signal to 'debug'
  counter0 :: (HiddenClockResetEnable dom) => Signal dom (BitVector 9)
  counter0 = register 0 $ satAdd SatWrap 1 <$> counter0
  counter1 :: (HiddenClockResetEnable dom) => Signal dom (BitVector 9)
  counter1 = register 20 $ satAdd SatWrap 1 <$> counter1
  counter2 :: (HiddenClockResetEnable dom) => Signal dom (BitVector 9)
  counter2 = register 40 $ satAdd SatWrap 1 <$> counter2

  Circuit main = circuit $ \(btns, rxBit) -> do
    (_rxByte, txBit) <- uartDf baud -< (txByte, rxBit)
    triggerReset <- triggerResetButtons -< btns
    packet <-
      ila
        ( ilaConfig
            d100
            "name"
            ((counter0, "Base") .*. (counter1, "+20") .*. (counter2, "+40") .*. HNil)
            (bundle (counter0, counter1, counter2))
        )
        (\(a, _, _) -> a == 300)
        -< triggerReset
    txByte <- ps2df -< packet
    idC -< txBit

  go = snd $ main ((btns, rx), pure ())

topEntity ::
  "CLK" ::: Clock Dom48 ->
  "BTN" ::: Reset Dom48 ->
  "PMOD3" ::: Signal Dom48 PmodBTN ->
  "PMOD1_6" ::: Signal Dom48 Bit ->
  "PMOD1_5" ::: Signal Dom48 Bit
topEntity clk rst btn = withClockResetEnable clk rst enableGen (topLogicUart (SNat @9600) (pack <$> btn))

makeTopEntity 'topEntity
