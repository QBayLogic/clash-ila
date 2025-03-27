{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module ConfigGen where

import Clash.Prelude

import Data.Aeson

import GHC.Records (getField)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Data (Proxy)
import Data.Hashable
import Data.List
import Language.Haskell.TH.Datatype
import Control.Monad

sig1 :: Signal dom (BitVector 12)
sig1 = pure 8

sig2 :: (KnownNat n) => Signal dom (Index n)
sig2 = pure 2

sig3 :: Signal System (Index 420)
sig3 = pure 69

sigs ::
  (KnownNat n) => SNat n -> Signal dom (BitVector 12, Index n)
sigs _ = bundle (sig1, sig2)

sigk :: Signal dom (BitVector 12, Index 5)
sigk = bundle (sig1, sig2)

getBitSize :: Name -> Q Language.Haskell.TH.Type
getBitSize name = do
  ForallT _ _ (AppT _ typeInfo) <- reifyType name

  bitsized <- [t| BitSize $(return typeInfo) |]

  expandSyns bitsized

woo :: Name -> Q Language.Haskell.TH.Exp
woo name = do
  [| sizeFromSignal $(varE name) |]

signalNameSize :: [Name] -> Q Language.Haskell.TH.Exp
signalNameSize names = [| $(listE $ Data.List.map (\name -> [| (name, woo $(varE name)) |] ) names) |]


-- fileContent :: (HiddenClockResetEnable dom, Lift a) => Signal dom a
-- fileContent sig = [| $(Clash.Prelude.lift sig) |]

-- getBitSize :: Name -> Q Language.Haskell.TH.Exp

printBitSize :: forall n. (KnownNat n) => Proxy n -> String
printBitSize i = show $ natVal @n i

sizeFromSignal ::
  forall dom a n.
  (BitPack a, n ~ BitSize a) =>
  Signal dom a -> Int
sizeFromSignal _ = natToNum @n

class GenConfig dom a b t where
  genConfig :: (BitPack a, Lift a, BitPack b, Lift b) => [GenSignal] -> (Signal dom a) -> (Signal dom b, String) -> Q t

-- last input
instance GenConfig dom a b (SNat n -> String -> Q (Signal dom (b,a))) where
  genConfig acc sacc (x, name_x) n topLeveLName = do
        let genSignals = nacc
        let nonHashed = GenIla topLeveLName (snatToNum n) 0 genSignals
        let hashedIla = GenIla topLeveLName (snatToNum n) (hash nonHashed) genSignals
        let genIlas = GenIlas [hashedIla]
        runIO $ encodeFile "ilaconf.json" genIlas
        return $ bundle (x, sacc)
    where
      nacc = acc Data.List.++ [(GenSignal name_x (sizeFromSignal x))] 

instance forall dom t a b. (GenConfig t dom a b, BitPack a, Lift a) => GenConfig ((Signal dom a, String) -> (Signal dom (a,t))) dom a (Signal dom b) where
  genConfig acc sacc (x, name_x) =
    genConfig (acc Data.List.++ [GenSignal name_x (sizeFromSignal @dom x)]) (bundle (x, sacc))

writeConfig ::
  forall dom n a .
  (KnownNat n, BitPack a) => SNat n -> String -> Unbundled dom a -> [String] -> Q Language.Haskell.TH.Syntax.Exp
writeConfig _ toplevelName signals names = do
  -- let sized = sizeFromSignal <$> signals
  -- let sized = signalListSizes signals
  -- let names = signalListNames signals

  -- let genSignals = (uncurry GenSignal) <$> (Data.List.zip names sized)
  let genSignals = (\n -> GenSignal n 1) <$> names

  let nonHashed = GenIla toplevelName (natToNum @n) 0 genSignals
  let hashedIla = GenIla toplevelName (natToNum @n) (hash nonHashed) genSignals

  let genIlas = GenIlas [hashedIla]

  runIO (encodeFile "ilaconf.json" genIlas)

  [|str|]

data IlaConfig a = IlaConfig
  { hash :: BitVector 32
  , tracing :: a
  }

data GenSignal = GenSignal
  { name :: String
  , width :: Int
  }
  deriving (Generic, Show, ToJSON, Eq, Hashable)

data GenIla = GenIla
  { toplevel :: String
  , bufferSize :: Int
  , hash :: Int
  , signals :: [GenSignal]
  }
  deriving (Generic, Show, ToJSON, Eq, Hashable)

data GenIlas = GenIlas
  { ilas :: [GenIla]
  }
  deriving (Generic, Show, ToJSON, Eq, Hashable)
