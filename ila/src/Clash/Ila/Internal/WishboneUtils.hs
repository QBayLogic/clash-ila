{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Clash.Ila.Internal.WishboneUtils where

import Clash.Prelude

import Protocols.Wishbone

{- | Check if the current wishbone cycle has a specific address and byte select

NOTE: only returns true in a valid WB cycle!
-}
compareAddr ::
  forall dom addrW selW dat.
  ( KnownNat addrW
  , KnownNat selW
  ) =>
  -- | Address to compare against
  BitVector addrW ->
  -- | Byte select to compare against
  BitVector selW ->
  -- | The M2S signal
  Signal dom (WishboneM2S addrW selW dat) ->
  -- | Output indicating if the current cycle matches the signal
  Signal dom Bool
compareAddr addr byteSel m2s =
  (\fwd -> fwd.busCycle && fwd.strobe && fwd.addr == addr && fwd.busSelect == byteSel)
    <$> m2s

{- | Check if the current wishbone cycle falls within a specific address range. Ignoring byte select

NOTE: only returns true in a valid WB cycle!
-}
compareAddrRange ::
  forall dom addrW selW dat.
  ( KnownNat addrW
  , KnownNat selW
  ) =>
  -- | The minimum valid address (inclusive)
  BitVector addrW ->
  -- | The maximum valid address (inclusive)
  BitVector addrW ->
  -- | The M2S signal
  Signal dom (WishboneM2S addrW selW dat) ->
  -- | Output indicating if the current cycle falls within the address range
  Signal dom Bool
compareAddrRange minAddr maxAddr m2s =
  (\fwd -> fwd.busCycle && fwd.strobe && fwd.addr >= minAddr && fwd.addr <= maxAddr)
    <$> m2s

{- | A mux but switching based on address

NOTE: only returns true in a WB write cycle!
-}
addrMux ::
  forall dom addrW selW dat item.
  ( KnownNat addrW
  , KnownNat selW
  ) =>
  -- | Address to compare against
  BitVector addrW ->
  -- | Byte select to compare against
  BitVector selW ->
  -- | The output if the current cycle matches the address
  Signal dom item ->
  -- | The output if the current cycle has a different address from the provided one
  Signal dom item ->
  -- | The M2S signal
  Signal dom (WishboneM2S addrW selW dat) ->
  -- | The actual output
  Signal dom item
addrMux addr byteSel true false m2s = mux (compareAddr addr byteSel m2s) true false
