{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Blink where

import Clash.Annotations.TH
import Clash.Cores.UART (ValidBaud)
import Clash.Prelude
import Communication
import Data.Proxy
import Domain
import Packet
import Pmod
import Probes
import Protocols
import RingBuffer

triggerReader ::
  (HiddenClockResetEnable dom) =>
  Circuit (CSignal dom (BitVector 4)) (CSignal dom Bool)
triggerReader = Circuit exposeIn
 where
  exposeIn (incoming, _) = out
   where
    value = register False $ liftA2 newValue value incoming

    newValue _ 1 = True
    newValue _ 2 = False
    newValue old _ = old

    out = (pure (), value)

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

  ila = ilaCore d6 (pure True) (== 20) (pure False) counter
  reader = ringBufferReaderPS ila

  Circuit main = circuit $ \(btns, rxBit) -> do
    (_activation, txBit) <- uartDf baud -< (txByte, rxBit)
    activeSignal <- triggerReader -< btns
    bufferData <- reader -< activeSignal
    packet <- dataPacket (Proxy :: Proxy (BitVector 9)) -< bufferData
    finalPacket <- finalizePacket -< packet
    txByte <- ps2df -< finalPacket
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
