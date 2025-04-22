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

import Data.Maybe qualified as DM

import Communication
import ConfigGen
import Domain
import Ila
import Packet
import Pmod
import Protocols

import Clash.Cores.Etherbone
import Protocols.Df qualified as Df
import Protocols.PacketStream
import Protocols.Wishbone
import Prelude qualified as P
import Debug.Trace
import Data.String.Interpolate (__i)

demoWishbone ::
  forall dom addrW dat datBytes.
  ( HiddenClockResetEnable dom
  , addrW ~ 32
  , dat ~ BitVector 32
  , datBytes ~ BitSize dat `DivRU` 8
  ) =>
  Circuit
    (Wishbone dom Standard addrW dat)
    (CSignal dom Bool)
demoWishbone = Circuit exposeIn
 where
  exposeIn (fwd, _) = out
   where
    new0 :: Signal dom (Maybe dat)
    new0 = writeReq 1 <$> fwd
    new1 :: Signal dom (Maybe dat)
    new1 = writeReq 2 <$> fwd

    addr0 :: Signal dom dat
    addr0 = register 0 $ liftA2 DM.fromMaybe addr0 new0
    addr1 :: Signal dom dat
    addr1 = register 0 $ liftA2 DM.fromMaybe addr1 new1

    writeReq :: BitVector addrW -> WishboneM2S addrW datBytes dat -> Maybe dat
    writeReq addr packet
      | packet.strobe && packet.writeEnable && packet.addr == addr = Just packet.writeData
      | otherwise = Nothing

    readData :: WishboneM2S addrW datBytes dat -> dat -> dat -> dat
    readData packet a0 a1
      | packet.addr == 1 = a0
      | packet.addr == 2 = a1
      | otherwise = minBound

    process :: WishboneM2S addrW datBytes dat -> dat -> WishboneS2M dat
    process m2s rd =
      WishboneS2M
        { retry = False
        , stall = False
        , acknowledge = m2s.strobe
        , readData = rd
        , err = False
        }

    led = register False $ led .||. strobe <$> fwd

    out = (liftA2 process fwd $ liftA3 readData fwd addr0 addr1, led)

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

-- | UART ILA demonstration toplevel
topLogicUart ::
  forall dom baud.
  (HiddenClockResetEnable dom, ValidBaud dom baud) =>
  -- | Baud rate of UART
  SNat baud ->
  -- | Buttons
  Signal dom (BitVector 4) ->
  -- | RX
  Signal dom Bit ->
  -- | Leds
  ( Signal dom Pmod8LD
  , -- \| TX
    Signal dom Bit
  )
topLogicUart baud btns rx = go
 where
  -- Simple demo signal to 'debug'
  counter0 :: (HiddenClockResetEnable dom) => Signal dom (BitVector 9)
  counter0 = register 0 $ satAdd SatWrap 1 <$> counter0
  counter1 :: (HiddenClockResetEnable dom) => Signal dom (BitVector 8)
  counter1 = register 20 $ satAdd SatWrap 1 <$> counter1
  counter2 :: (HiddenClockResetEnable dom) => Signal dom (Signed 10)
  counter2 = register 40 $ satAdd SatWrap 1 <$> counter2

  Circuit demoWish = circuit $ \rxBit -> do
    (rxByte, txBit) <- uartDf baud -< (txByte, rxBit)
    rxPs <- etherboneDfPacketizer <| holdUntilAck -< rxByte
    txByte <- ps2df -< txPs

    (txPs, wbMaster) <- etherboneC 0 (pure Nil) -< rxPs
    led <- demoWishbone -< wbMaster

    idC -< (txBit, led)

  -- Circuit main = circuit $ \(rxBit) -> do
  --   (rxByte, txBit) <- uartDf baud -< (txByte, rxBit)
  --   packet <-
  --     ila
  --       ( ilaConfig
  --           d100
  --           20
  --           "name"
  --           ( ilaProbe
  --               (counter0, "c0")
  --               (counter1, "c1")
  --               (counter2, "c2")
  --               ()
  --           )
  --       )
  --       ((== 300) <$> counter0)
  --       (pure True)
  --       -< rxByte
  --   txByte <- ps2df <| dropMeta -< packet
  --   idC -< txBit

  -- go = snd $ main (rx, pure ())
  (txBit, isOn) = snd $ demoWish (rx, (pure (), pure ()))

  leds :: Signal dom Pmod8LD
  leds = (\v -> unpack . pack $ replicate d8 v) <$> isOn

  go = (leds, txBit)

-- | The top entity
topEntity ::
  "CLK" ::: Clock Dom48 ->
  "BTN" ::: Reset Dom48 ->
  "PMOD3" ::: Signal Dom48 PmodBTN ->
  "PMOD1_6" ::: Signal Dom48 Bit ->
  ( "PMOD2" ::: Signal Dom48 Pmod8LD
  , "PMOD1_5" ::: Signal Dom48 Bit
  )
topEntity clk rst btn = withClockResetEnable clk rst enableGen (topLogicUart (SNat @9600) (pack <$> btn))

makeTopEntity 'topEntity
