{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module ConfigGen where

import Clash.Prelude hiding (Exp, Type)

import Data.Aeson

import Data.Either
import Data.HList
import Data.Hashable

import Text.Show.Pretty

import Clash.Annotations.Primitive
import Clash.Backend
import Clash.Core.Term
import Clash.Core.TermLiteral
import Clash.Core.Type
import Clash.Core.Var
import Clash.Netlist.BlackBox
import Clash.Netlist.BlackBox.Types
import Clash.Netlist.Types
import Clash.Primitives.DSL
import Control.Lens qualified
import Control.Monad.State
import Data.String.Interpolate (__i)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Prettyprinter
import Prelude qualified as P

import Data.List

class LabelledSignals t n dom a where
  ilaProbe' :: (Vec n GenSignal, Signal dom a) -> t

instance (KnownNat n, 1 <= n, m ~ n) => LabelledSignals (Vec m GenSignal, Signal dom a) n dom a where
  ilaProbe' :: (Vec n GenSignal, Signal dom a) -> (Vec n GenSignal, Signal dom a)
  ilaProbe' acc = acc

instance
  ( LabelledSignals cont (n + 1) dom nextS
  , BitPack a
  , nextS ~ (s, a)
  ) =>
  LabelledSignals ((Signal dom a, String) -> cont) n dom s
  where
  ilaProbe' :: (Vec n GenSignal, Signal dom s) -> (Signal dom a, String) -> cont
  ilaProbe' (acc, sigAcc) (sig, label) =
    let
      newAcc :: Vec (n + 1) GenSignal
      newAcc =
        GenSignal
          { name = label
          , width = natToNum @(BitSize a)
          }
          :> acc

      newSig :: Signal dom nextS
      newSig = bundle (sigAcc, sig)
     in
      ilaProbe' (newAcc, newSig)

ilaProbe :: forall dom t. (HiddenClockResetEnable dom, LabelledSignals t 0 dom ()) => t
ilaProbe = ilaProbe' (Nil, pure () :: Signal dom ())

writeSignalInfo ::
  forall dom n.
  (HiddenClockResetEnable dom) =>
  Vec n (Int, String) ->
  BitVector 32
writeSignalInfo !_sig = 0 :: BitVector 32
{-# OPAQUE writeSignalInfo #-}
{-# ANN writeSignalInfo hasBlackBox #-}
{-# ANN
  writeSignalInfo
  ( let
      primitive = 'writeSignalInfo
      template = 'signalInfoBBF
     in
      InlineYamlPrimitive
        [minBound ..]
        [__i|
      BlackBoxHaskell:
        name: #{primitive}
        templateFunction: #{template}
        workInfo: Always
    |]
  )
  #-}

signalInfoBBF :: (HasCallStack) => BlackBoxFunction
signalInfoBBF _ _primitive args _ = Control.Lens.view tcCache >>= go
 where
  go tcm
    | [_, sizes] <- lefts args
    , [_, (coreView tcm -> LitTy (NumTy n))] <- rights args
    , Just (SomeNat (Proxy :: Proxy n)) <- someNatVal n =
        mkBlackBox $ getSizes @n sizes
    | otherwise = errorX "Improper data given, expected Vec n (Int, String)"

  mkBlackBox sizes = pure $ Right (blackBoxMeta sizes, blackBox)

  getSizes :: forall n. (KnownNat n) => Term -> Vec n (Int, String)
  getSizes term = case termToData @(Vec n (Int, String)) term of
    Left _ -> errorX "Failed to coerce term into Vec n (Int, String), cannot write ILA config to file."
    Right s -> s

  blackBoxMeta :: Vec n (Int, String) -> BlackBoxMeta
  blackBoxMeta sizes =
    emptyBlackBoxMeta
      { bbKind = TDecl
      , bbRenderVoid = RenderVoid
      , bbIncludes =
          [
            ( ("ilaconf", "json")
            , BBFunction (show 'renderJSONTF) 0 (renderJSONTF sizes)
            )
          ]
      }

  -- We actually don't want to generate special HDL, so this will be black
  blackBox :: BlackBox
  blackBox = BBFunction (show 'renderHDLTF) 0 renderHDLTF
{-# NOINLINE signalInfoBBF #-}

renderHDLTF :: (HasCallStack) => TemplateFunction
renderHDLTF = TemplateFunction [] (const True) (\_ -> pure "")
{-# NOINLINE renderHDLTF #-}

renderJSONTF :: (HasCallStack) => Vec n (Int, String) -> TemplateFunction
renderJSONTF args = TemplateFunction [] (const True) (renderJSON args)
{-# NOINLINE renderJSONTF #-}

renderJSON ::
  forall s n.
  ( HasCallStack
  , Backend s
  ) =>
  Vec n (Int, String) ->
  BlackBoxContext ->
  State s (Doc ())
renderJSON args _context = pure $ [__i|Sizes are: #{args}|]
{-# NOINLINE renderJSON #-}

ilaConfig ::
  forall dom a n s.
  ( HiddenClockResetEnable dom
  , -- , NamedSignal ts
    KnownNat n
  , Lift a
  ) =>
  -- | How many samples should it capture of each signal?
  SNat n ->
  -- | How many samples after triggering should it still capture?
  Index n ->
  -- | Merely an identifier, recommended to be the name of the toplevel design, but it can be any name you fancy
  String ->
  -- | A list
  (Vec s GenSignal, Signal dom a) ->
  -- | The ILA configuration itself
  IlaConfig n (Signal dom a)
ilaConfig size triggerPoint toplevelName (genSignal, bundled) =
  IlaConfig
    { hash = writeSignalInfo ((\s -> (s.width, s.name)) <$> genSignal)
    , size = size
    , triggerPoint = triggerPoint
    , tracing = bundled
    }

-- A record containing the actual configuration of the ILA
data IlaConfig n a = IlaConfig
  { hash :: BitVector 32
  -- ^ A hash to verify which ILA the system is communicating with
  , size :: SNat n
  -- ^ Size of the buffers, aka; how many samples should it capture
  , triggerPoint :: Index n
  -- ^ How many samples *after* triggering it should sample
  , tracing :: a
  -- ^ The signal to trace
  }
  deriving (Lift)

-- All types primarily just used to properly format a JSON document

-- | Individual signal JSON representation
data GenSignal = GenSignal
  { name :: String
  , width :: Int
  }
  deriving (Generic, Show, ToJSON, Eq, Hashable)

-- | Individual ILA JSON representation
data GenIla = GenIla
  { toplevel :: String
  , bufferSize :: Int
  , hash :: Int
  , signals :: [GenSignal]
  }
  deriving (Generic, Show, ToJSON, Eq, Hashable)

-- | Toplevel of the JSON file
data GenIlas = GenIlas
  { ilas :: [GenIla]
  }
  deriving (Generic, Show, ToJSON, Eq, Hashable)
