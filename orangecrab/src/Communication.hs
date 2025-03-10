
module Communication where

import Clash.Cores.UART (ValidBaud, uart)
import Clash.Prelude

import Protocols
import qualified Protocols.Df as Df

uartDf ::
  forall dom baud .
  (HiddenClockResetEnable dom, ValidBaud dom baud) =>
  -- | Uart BAUD rate
  SNat baud ->
  -- | The UART circuit, a small wrapper around default uart to make it use Df instead of raw signals
  -- The inputs are: transmission byte, rx bit
  -- The outputs are: recieved byte, tx bit
  Circuit
    (Df dom (BitVector 8), CSignal dom Bit)
    (CSignal dom (Maybe (BitVector 8)), CSignal dom Bit)
uartDf baud = Circuit exposeIn
  where
    exposeIn ((transmit, rxBit), _) = out
      where
        (recieved, txBit, acked) = uart baud rxBit (Df.dataToMaybe <$> transmit)

        out = (
            (Ack <$> acked, pure ()),
            (recieved, txBit)
          )

