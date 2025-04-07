module Communication where

import Clash.Cores.UART (ValidBaud, uart)
import Clash.Prelude
import Data.Maybe qualified as DM
import Protocols
import Protocols.Df qualified as Df
import Protocols.PacketStream

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
      register Df.NoData $
        mux
          (ackToBool <$> ack .||. Df.noData <$> hold)
          (Df.maybeToData <$> incoming)
          hold

    -- I'm honestly surprised that this isn't a thing already
    -- There are so many nice Df utility functions, yet this one misses
    -- Oh well
    ackToBool :: Ack -> Bool
    ackToBool (Ack b) = b

    out = (pure (), hold)

-- | Silently drops the meta of a `PacketStream`
-- Makes no other modifications to the `PacketStream` itself
dropMeta ::
  forall dom a meta.
  Circuit
    (PacketStream dom a meta)
    (PacketStream dom a ())
dropMeta = mapMeta (\_ -> ())

{- | Converts `PacketStream` into a `Df` of bytes. For packet stream data larger than one byte,
it will send out bytes in most-significant-byte order.

NOTE: Please do properly follow the PacketStream standard and keep sending the same data until
`_ready` is raised. Not doing so will cause issues.
-}
ps2df ::
  forall dom dataWidth.
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  ) =>
  -- | The input `PacketStream` and the output `Df`, will only set `_ready` to true once every
  -- byte in PacketStream has been sent over in Df
  Circuit
    (PacketStream dom dataWidth ())
    (Df dom (BitVector 8))
ps2df = Circuit exposeIn <| downConverterC @1
 where
  exposeIn (incoming, backpressure) = out
   where
    toDf :: Maybe (PacketStreamM2S 1 ()) -> Df.Data (BitVector 8)
    toDf (Just packet) = Df.Data $ at d0 (_data packet)
    toDf Nothing = Df.NoData

    ack2ps :: Ack -> PacketStreamS2M
    ack2ps (Ack b) = PacketStreamS2M b

    out =
      ( ack2ps <$> backpressure
      , toDf <$> incoming
      )
