{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Blink where

import Clash.Annotations.TH
import Clash.Prelude

import Protocols
import Protocols.Wishbone

import Domain
import Pmod

import RingBuffer

ledController ::
  (HiddenClockResetEnable dom) =>
  Circuit
    (CSignal dom (Index 4))
    (Wishbone dom Standard (CLog 2 4) (Unsigned 8), CSignal dom (BitVector 8))
ledController = Circuit exposeIn
  where
    exposeIn (fwdIn, (bwdIn, _)) = out
      where
        ledBuffer = register (0 :: Unsigned 8) $ mux acceptNew readFromBuffer ledBuffer

        acceptNew = acknowledge <$> bwdIn
        readFromBuffer = readData <$> bwdIn

        transferBusy = register False $ invokeNew .||. (transferBusy .&&. not <$> acceptNew)
        invokeNew = flip testBit 0 <$> fwdIn

        makePacket = liftA2 (\addr sendReq -> WishboneM2S {
            burstTypeExtension = LinearBurst,
            cycleTypeIdentifier = Classic,
            writeEnable = False,
            strobe = sendReq,
            busCycle = sendReq,
            lock = False,
            busSelect = 0,
            writeData = 0,
            addr = (flip clearBit) 0 $ bitCoerce addr
          }) fwdIn transferBusy

        out = (pure (), (makePacket, pack <$> ledBuffer))

btnToIndex :: BitVector 4 -> Index 4
btnToIndex 1 = 0
btnToIndex 2 = 1
btnToIndex 4 = 2
btnToIndex 8 = 3
btnToIndex _ = 0

topLogic ::
  forall dom .
  (HiddenClockResetEnable dom) =>
  Signal dom PmodBTN -> Signal dom Pmod8LD
topLogic btn = go
  where
    bufferBlank = ringBuffer d4 (69 :: Unsigned 8) (pure False)

    inserted :: Signal dom (Index 6)
    inserted = register 0 $ flip (satAdd SatBound) 1 <$> inserted

    buffer = bufferBlank $ (\i -> case i of
      1 -> Just 3
      2 -> Just 7
      3 -> Just 15
      4 -> Just 31
      _ -> Nothing) <$> inserted
    reader = ringBufferReader buffer

    testCir ::
      (HiddenClockResetEnable dom) =>
      (Fwd (CSignal dom (Index 4)), Bwd (CSignal dom (BitVector 8))) ->
      (Bwd (CSignal dom (Index 4)), Fwd (CSignal dom (BitVector 8)))
    Circuit testCir = circuit $ \input -> do
      (wishM, sig) <- ledController -< input
      reader -< wishM
      idC -< sig

    go = unpack <$> (snd $ testCir (btnToIndex . pack <$> btn, pure ()))

topEntity ::
  "CLK" ::: Clock Dom48 ->
  "BTN" ::: Reset Dom48 ->
  "PMOD3" ::: Signal Dom48 PmodBTN ->
  "PMOD2" ::: Signal Dom48 Pmod8LD
topEntity clk rst = withClockResetEnable clk rst enableGen topLogic

makeTopEntity 'topEntity
