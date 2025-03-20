module Communication where

import Clash.Cores.UART (ValidBaud, uart)
import Clash.Prelude
import Protocols
import Protocols.Df qualified as Df

uartDf ::
  forall dom baud.
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

    out =
      ( (Ack <$> acked, pure ())
      , (recieved, txBit)
      )

-- | Holds a certain value until we get an `Ack`, only then will it accept new values
holdUntilAck ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | The data you want to hold
  -- It only accepts new data if it is currently not storing any, OR we got a positive acknowledgement
  -- The output will be the data currently held, if any
  Circuit
    (CSignal dom (Maybe (BitVector 8)))
    (Df dom (BitVector 8))
holdUntilAck = Circuit exposeIn
 where
  exposeIn (incoming, ack) = out
   where
    hold :: (Signal dom (Df.Data (BitVector 8)))
    hold =
      register Df.NoData
        $ mux
          (ackToBool <$> ack .||. Df.noData <$> hold)
          (Df.maybeToData <$> incoming)
          hold

    -- I'm honestly surprised that this isn't a thing already
    -- There are so many nice Df utility functions, yet this one misses
    -- Oh well
    ackToBool :: Ack -> Bool
    ackToBool (Ack b) = b

    out = (pure (), hold)
