{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Blink where

import Clash.Annotations.TH
import Clash.Prelude

import Clash.Cores.UART (ValidBaud)

import Protocols
import Protocols.Wishbone
import Protocols.PacketStream
import qualified Protocols.Df as Df

import qualified Data.List as DL
import qualified Data.Maybe as DM

import Data.Proxy

import Domain
import Pmod
import Communication
import Probes
import Packet

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

ps2df ::
  (HiddenClockResetEnable dom) =>
  Circuit (PacketStream dom 1 a) (Df dom (BitVector 8))
ps2df = Circuit exposeIn
 where
  exposeIn (incoming, backpressure) = out
   where
    ack2bool :: Ack -> Bool
    ack2bool (Ack b) = b

    conv Nothing = Df.NoData
    conv (Just m2s) = Df.Data $ head $ _data m2s

    out = (
        PacketStreamS2M . ack2bool <$> backpressure,
        conv <$> incoming
      )

topLogicUart ::
  forall dom baud .
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
  bufferBlank = ringBuffer d4 (69 :: Unsigned 8) (pure False)

  inserted :: Signal dom (Index 6)
  inserted = register 0 $ flip (satAdd SatBound) 1 <$> inserted

  buffer = bufferBlank $ (\i -> case i of
    1 -> Just 65
    2 -> Just 66
    3 -> Just 67
    4 -> Just 68
    _ -> Nothing) <$> inserted
  reader = ringBufferReaderPS buffer

  Circuit main = circuit $ \(btns, rxBit) -> do
    (_activation, txBit) <- uartDf baud -< (txByte, rxBit)
    activeSignal <- triggerReader -< btns
    bufferData <- reader -< activeSignal
    packet <- dataPacket (Proxy :: Proxy (BitVector 8)) -< bufferData
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
