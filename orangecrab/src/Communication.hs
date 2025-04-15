module Communication where

import Clash.Cores.UART (ValidBaud, uart)
import Clash.Prelude
import Data.Maybe qualified as DM
import Data.Type.Equality ((:~:) (Refl))
import Protocols
import Protocols.Df qualified as Df
import Protocols.PacketStream

{- | Send out `Df` data through UART

In essence just a tiny wrapper around `uart` to provide backpressure using `Circuit`s rather than
manual signals
-}
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

{- | Silently drops the meta of a `PacketStream`
Makes no other modifications to the `PacketStream` itself
-}
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

{- | Converts a `Df` into a `PacketStream`. The packet stream will be transactions of individual
bytes, if you want it to bundle multiple bytes together, use `upConverterC`
-}
df2ps ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Circuit
    (Df dom (BitVector 8))
    (PacketStream dom 1 ())
df2ps = Circuit exposeIn
 where
  exposeIn (incoming, backpressure) = out
   where
    ps2ack :: PacketStreamS2M -> Ack
    ps2ack = Ack . _ready

    toPs :: Df.Data (BitVector 8) -> Maybe (PacketStreamM2S 1 ())
    toPs Df.NoData = Nothing
    toPs (Df.Data byte) =
      Just $
        PacketStreamM2S
          { _meta = ()
          , _last = Just 1
          , _abort = False
          , _data = byte :> Nil
          }

    out =
      ( ps2ack <$> backpressure
      , toPs <$> incoming
      )

{- | Given a stream of bytes, replace the `_last` with `Nothing`, every `n`th packet will have
`Just 1` set. This is useful in combination with `upConverterC`
-}
mergeBytePackets ::
  forall dom n a.
  ( HiddenClockResetEnable dom
  , KnownNat n
  , 1 <= n
  ) =>
  -- | How many packets need to be bundled together
  SNat n ->
  Circuit
    (PacketStream dom 1 a)
    (PacketStream dom 1 a)
mergeBytePackets _ = Circuit exposeIn
 where
  exposeIn (fwdIn, bwdIn) = out
   where
    counter :: Signal dom (Index n)
    counter = register 0 $ mux (DM.isJust <$> fwdIn) (satAdd SatWrap 1 <$> counter) counter

    replaceLast (Just packet) count
      | count == natToNum @(n - 1) = Just packet{_last = Just 1}
      | otherwise = Just packet{_last = Nothing}
    replaceLast Nothing _ = Nothing

    out = (bwdIn, liftA2 replaceLast fwdIn counter)

-- | Converts a `Signal dom (Maybe a)` into a `Df dom a`. Since CSignals do not accept backpressure
-- all backpressure will be ignored and is hence, unsafe.
unsafeCSignalToDf ::
  forall dom a.
  (HiddenClockResetEnable dom) =>
  Circuit
    (CSignal dom (Maybe a))
    (Df dom a)
unsafeCSignalToDf = Circuit exposeIn
 where
  exposeIn (incoming, _) = out
   where
    toData Nothing = Df.NoData
    toData (Just a) = Df.Data a

    out =
      ( pure ()
      , toData <$> incoming
      )
