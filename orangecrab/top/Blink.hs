{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Blink where

import Clash.Annotations.TH
import Clash.Cores.UART (ValidBaud)
import Clash.Prelude

import Data.Maybe qualified as DM

import Communication
import Domain
import Pmod
import Probes
import Protocols

triggerReader ::
  (HiddenClockResetEnable dom) =>
  Circuit (CSignal dom (Maybe (BitVector 8))) (CSignal dom Bool)
triggerReader = Circuit exposeIn
 where
  exposeIn (incoming, _) = out
   where
    out = (pure (), DM.isJust <$> incoming)

triggerReaderBtn ::
  (HiddenClockResetEnable dom) =>
  Circuit (CSignal dom (BitVector 4)) (CSignal dom Bool)
triggerReaderBtn = Circuit exposeIn
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
  counter :: (HiddenClockResetEnable dom) => Signal dom (BitVector 9)
  counter = register 0 $ satAdd SatWrap 1 <$> counter

  Circuit main = circuit $ \(rxBit) -> do
    (activation, txBit) <- uartDf baud -< (txByte, rxBit)
    triggerReset <- triggerReader -< activation
    packet <- ilaProbe (SNat @1500) (==300) counter -< triggerReset
    txByte <- ps2df -< packet
    idC -< txBit

  go = snd $ main (rx, pure ())

topEntity ::
  "CLK" ::: Clock Dom48 ->
  "BTN" ::: Reset Dom48 ->
  "PMOD3" ::: Signal Dom48 PmodBTN ->
  "PMOD1_6" ::: Signal Dom48 Bit ->
  "PMOD1_5" ::: Signal Dom48 Bit
topEntity clk rst btn = withClockResetEnable clk rst enableGen (topLogicUart (SNat @9600) (pack <$> btn))

makeTopEntity 'topEntity
