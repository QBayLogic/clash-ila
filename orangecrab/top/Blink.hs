{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Blink where

import Clash.Annotations.TH
import Clash.Prelude

import Clash.Cores.UART (ValidBaud)

import Protocols
import Protocols.Wishbone
import qualified Protocols.Df as Df

import qualified Data.List as DL
import qualified Data.Maybe as DM

import Domain
import Pmod
import Communication
import Probes

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
    reader = ringBufferReaderWb buffer

    testCir ::
      (HiddenClockResetEnable dom) =>
      (Fwd (CSignal dom (Index 4)), Bwd (CSignal dom (BitVector 8))) ->
      (Bwd (CSignal dom (Index 4)), Fwd (CSignal dom (BitVector 8)))
    Circuit testCir = circuit $ \input -> do
      (wishM, sig) <- ledController -< input
      reader -< wishM
      idC -< sig

    go = unpack <$> (snd $ testCir (btnToIndex . pack <$> btn, pure ()))

-- | How is this NOT a thing yet??
ackToBool :: Ack -> Bool
ackToBool (Ack v) = v

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
topLogicUart baud buttons rx = go
 where
  testSignal = register (0 :: BitVector 8) $ satAdd SatWrap 1 <$> testSignal

  ila = ilaCore (SNat @10) (pure True) (\s -> s == 58) (pure False) testSignal

  bufferCircuit ::
    Circuit
      (CSignal dom (Maybe (BitVector 8)))
      (CSignal dom (Maybe (BitVector 8)))
  bufferCircuit = Circuit exposeIn
   where
    exposeIn (idx, _) = out
     where
      valueValid = register False $ DM.isJust <$> idx
      value = valueToChr <$> (fst . ila $ chrToIndex <$> idx)

      valueToChr :: Maybe (BitVector 8) -> Maybe (BitVector 8)
      valueToChr (Just v) = Just $ v
      valueToChr Nothing = Nothing

      chrToIndex :: Maybe (BitVector 8) -> Index 10
      chrToIndex (Just v) = unpack . pack . resize $ satSub SatZero v 48
      chrToIndex _ = 0

      out = (
          pure (),
          mux valueValid
            (value)
            (pure Nothing)
        )

  Circuit main = circuit $ \rxBit -> do
    (newIndex, txBit) <- uartDf baud -< (txBuffer, rxBit)
    bufferValue <- bufferCircuit -< newIndex
    txBuffer <- holdUntilAck -< bufferValue
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
