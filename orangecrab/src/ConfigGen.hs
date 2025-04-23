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

import Data.Data
import Data.List

{- | From a tuple consisting of a signal and a string, grab the bit width of the signal and put it
in a vector. At the same time, bundle every signal together.

This function is intended to be used in tangent with `ilaConfig`
-}
class LabelledSignals t n dom a where
  ilaProbe' :: (Vec n GenSignal, Signal dom a) -> t

-- | Base case
instance (KnownNat n, 1 <= n, m ~ n) => LabelledSignals (Vec m GenSignal, Signal dom a) n dom a where
  ilaProbe' :: (Vec n GenSignal, Signal dom a) -> (Vec n GenSignal, Signal dom a)
  ilaProbe' acc = acc

-- | Induction case
instance {-# OVERLAPPABLE #-}
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

instance
  ( LabelledSignals cont 1 dom a
  , BitPack a
  , nextS ~ a
  ) =>
  LabelledSignals ((Signal dom a, String) -> cont) 0 dom s
  where
  ilaProbe' :: (Vec n GenSignal, Signal dom s) -> (Signal dom a, String) -> cont
  ilaProbe' (Nil, _) (sig, label) = ilaProbe' (newAcc, newSig)
    where
      newAcc :: Vec 1 GenSignal
      newAcc =
        GenSignal
          { name = label
          , width = natToNum @(BitSize a)
          }
          :> Nil

      newSig :: Signal dom nextS
      newSig = sig

{- | A polyvariadic function containing 'labelled signals', aka, a list of tuples where the left
side is an arbitary signal, and the right a string. The final entry should always be an empty
tuple `()`. This is so GHC can properly infer the type. Omitting the empty tuple will require
you to explicitly mark out the result type.

The result of the function is intended to be forwarded to `IlaConfig`

The result type is `(Vec n (Int, String), Signal dom a)`

Example:

>>> counter = register 0 $ counter + 1 :: Signal dom (Unsigned 8)
>>> active = pure True :: Signal dom Bool
>>> ilaProbe (counter, "8 bit value") (active "system active") ()
-}
-- ilaProbe :: forall dom t a. (HiddenClockResetEnable dom, LabelledSignals t 0 dom a) => t
ilaProbe first@(f, _) = ilaProbe' (Nil, f) first

-- | Write signal information to a file, using blackboxes
writeSignalInfo ::
  forall dom n s.
  (HiddenClockResetEnable dom) =>
  -- | Toplevel name
  String ->
  -- | Buffer size
  SNat s ->
  -- | A `Vec` of signal widths and their label
  Vec n (Int, String) ->
  -- | The hash of the JSON
  BitVector 32
writeSignalInfo !_toplevel !_bufSize !_sigInfo = 0 :: BitVector 32
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

-- | The write signal blackbox function, grabs the AST from the context it gets invoked in
signalInfoBBF :: (HasCallStack) => BlackBoxFunction
signalInfoBBF _ _ args _ = Control.Lens.view tcCache >>= go
 where
  go tcm
    | [_, toplevel, _, sigInfo] <- lefts args
    , [_, (coreView tcm -> LitTy (NumTy n)), (coreView tcm -> LitTy (NumTy s))] <- rights args
    , Just (SomeNat (Proxy :: Proxy n)) <- someNatVal n
    , Just (SomeNat (Proxy :: Proxy s)) <- someNatVal s =
        mkBlackBox $ GenIlas [getGenIla @n toplevel (SNat @s) (getSigInfo sigInfo)]
    | otherwise = errorX "Improper data given, expected Vec n (Int, String)"

  -- \| Make the actual blackbox
  mkBlackBox input = pure $ Right (blackBoxMeta input, blackBox)

  -- \| Coerce a `Term` back into a type
  -- Panics on failure
  coerceToType ::
    (TermLiteral a) =>
    -- \| The AST of the type
    Term ->
    -- \| Error label, to be displayed if coercion fails
    String ->
    -- \| The result type
    a
  coerceToType term err = case termToData term of
    Left _ -> errorX [__i|Cannot coerce term into an #{err}, cannot write ILA config|]
    Right v -> v

  -- \| Get the signal information from it's AST form
  getSigInfo :: forall n. (KnownNat n) => Term -> Vec n (Int, String)
  getSigInfo term = coerceToType term "signal info"

  -- \| Generate the ILA from the AST
  getGenIla ::
    forall n s. (KnownNat n, KnownNat s) => Term -> SNat s -> Vec n (Int, String) -> GenIla
  getGenIla toplevel bufSize sigInfo =
    GenIla
      { toplevel = coerceToType toplevel "toplevel name"
      , bufferSize = snatToNum bufSize
      , hash = 0
      -- The reverse is needed as the polyvariadic function builds up the vector in reverse order
      , signals = P.reverse $ toList $ toGenSignal <$> sigInfo
      }

  -- \| Meta information about the blackbox
  -- We abuse the `bbIncludes` feature to write our ILA configuration to a JSON file
  blackBoxMeta :: GenIlas -> BlackBoxMeta
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

  -- \| The blackbox itself, which we don't use as we only want to write to meta files
  blackBox :: BlackBox
  blackBox = BBFunction (show 'renderHDLTF) 0 renderHDLTF
{-# NOINLINE signalInfoBBF #-}

{- | Template function to generate HDL
As we only want to write JSON, this is simply an empty string
-}
renderHDLTF :: (HasCallStack) => TemplateFunction
renderHDLTF = TemplateFunction [] (const True) (\_ -> pure "")
{-# NOINLINE renderHDLTF #-}

-- | Template function to generate JSON
renderJSONTF :: (HasCallStack) => GenIlas -> TemplateFunction
renderJSONTF args = TemplateFunction [] (const True) (renderJSON args)
{-# NOINLINE renderJSONTF #-}

-- | Actually render JSON
renderJSON ::
  forall s.
  ( HasCallStack
  , Backend s
  ) =>
  -- | The Ilas to encode to JSON
  GenIlas ->
  -- | Unused
  BlackBoxContext ->
  -- | The output JSON content
  State s (Doc ())
renderJSON args _ = pure [__i|#{encoded}|]
 where
  encoded = encode args
{-# NOINLINE renderJSON #-}

ilaConfig ::
  forall dom a n s.
  ( HiddenClockResetEnable dom
  , KnownNat n
  , Lift a
  ) =>
  -- | How many samples should it capture of each signal?
  SNat n ->
  -- | How many samples after triggering should it still capture?
  Index n ->
  -- | Merely an identifier, recommended to be the name of the toplevel design, but it can be any name you fancy
  String ->
  -- | A tuple containing a `Vec` of `GenSignals` and a bundled signal consisting out of every signal you want to monitor
  -- You can generate this by using `ilaProbe`
  (Vec s GenSignal, Signal dom a) ->
  -- | The ILA configuration itself
  IlaConfig n (Signal dom a)
ilaConfig size triggerPoint toplevel (genSignal, bundled) =
  IlaConfig
    { hash = writeSignalInfo toplevel size (fromGenSignal <$> genSignal)
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

toGenSignal :: (Int, String) -> GenSignal
toGenSignal (w, n) = GenSignal n w

fromGenSignal :: GenSignal -> (Int, String)
fromGenSignal s = (s.width, s.name)

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
