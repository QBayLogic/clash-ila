{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module ConfigGen where

import Clash.Prelude hiding (Exp, Type)
import Prelude qualified as P

import Clash.Annotations.Primitive
import Clash.Backend
import Clash.Core.Term
import Clash.Core.TermLiteral
import Clash.Core.Type
import Clash.Netlist.BlackBox.Types
import Clash.Netlist.Types
import Clash.Primitives.DSL qualified as DSL

import Control.Lens (view)
import Control.Monad.State (State)
import Data.String.Interpolate (__i)
import GHC.Stack (HasCallStack)
import Prettyprinter (Doc)

import Data.Aeson (ToJSON, encode)
import Data.Data (Proxy (Proxy))
import Data.Either
import Data.Hashable (Hashable, hash)
import Data.Monoid (Ap (getAp))
import Data.Word (Word32)

{- | From a tuple consisting of a signal and a string, grab the bit width of the signal and put it
in a vector. At the same time, bundle every signal together.

This function is intended to be used in tangent with `ilaConfig`
-}
class LabelledSignals t n dom a where
  ilaProbe' :: (Vec n GenSignal, Signal dom a) -> t

-- | Base case
instance (KnownNat n, m ~ n, a ~ s) => LabelledSignals (Vec m GenSignal, Signal dom s) n dom a where
  ilaProbe' :: (Vec n GenSignal, Signal dom a) -> (Vec n GenSignal, Signal dom a)
  ilaProbe' acc = acc

-- | Induction case
instance
  {-# OVERLAPPABLE #-}
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
  ilaProbe' (Nil, _) (sig, label) = ilaProbe' (newAcc, sig)
   where
    newAcc :: Vec 1 GenSignal
    newAcc =
      GenSignal
        { name = label
        , width = natToNum @(BitSize a)
        }
        :> Nil

{- | A polyvariadic function containing 'labelled signals', aka, a list of tuples where the left
side is an arbitary signal, and the right a string. 

# Example:

>>> counter = register 0 $ counter + 1 :: Signal dom (Unsigned 8)
>>> active = pure True :: Signal dom Bool
>>> ilaProbe (counter, "8 bit value") (active "system active")
-}
ilaProbe :: (LabelledSignals ((Signal dom a, b) -> t) 0 dom a) => (Signal dom a, b) -> t
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
writeSignalInfo !_toplevel !_bufSize !_sigInfo = 0
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
signalInfoBBF _ _ args _ = view tcCache >>= go
 where
  go tcm
    | [_, toplevel, _, sigInfo] <- lefts args
    , [_, (coreView tcm -> LitTy (NumTy n)), (coreView tcm -> LitTy (NumTy s))] <- rights args
    , Just (SomeNat (Proxy :: Proxy n)) <- someNatVal n
    , Just (SomeNat (Proxy :: Proxy s)) <- someNatVal s =
        mkBlackBox $ getGenIla @n toplevel (SNat @s) (getSigInfo sigInfo)
    | otherwise = errorX "Improper data given, expected Vec n (Int, String)"

  -- \| Make the actual blackbox
  mkBlackBox input = pure $ Right (blackBoxMeta input, blackBox input)

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
      , hash =
          fromIntegral
            $ hash
              ( coerceToType toplevel "toplevel name" :: String
              , snatToInteger bufSize
              , toList sigInfo
              )
      , -- The reverse is needed as the polyvariadic function builds up the vector in reverse order
        signals = P.reverse $ toList $ toGenSignal <$> sigInfo
      }

  -- \| Meta information about the blackbox
  -- We abuse the `bbIncludes` feature to write our ILA configuration to a JSON file
  blackBoxMeta :: GenIla -> BlackBoxMeta
  blackBoxMeta sizes =
    emptyBlackBoxMeta
      { bbKind = Clash.Netlist.BlackBox.Types.TExpr
      , bbIncludes =
          [
            ( ("ilaconf", "json")
            , BBFunction (show 'renderJSONTF) 0 (renderJSONTF sizes)
            )
          ]
      }

  -- \| The blackbox itself, which we don't use as we only want to write to meta files
  blackBox :: GenIla -> BlackBox
  blackBox sizes = BBFunction (show 'renderHDLTF) 0 (renderHDLTF sizes)
{-# NOINLINE signalInfoBBF #-}

{- | Template function to generate HDL
As we only want to write JSON, this is simply an empty string
-}
renderHDLTF :: (HasCallStack) => GenIla -> TemplateFunction
renderHDLTF args = TemplateFunction [] (const True) (renderHDL args)
{-# NOINLINE renderHDLTF #-}

-- | Template function to generate JSON
renderJSONTF :: (HasCallStack) => GenIla -> TemplateFunction
renderJSONTF args = TemplateFunction [] (const True) (renderJSON args)
{-# NOINLINE renderJSONTF #-}

-- | Calculate the hash over the file contents and return it as a number
renderHDL ::
  forall s.
  ( HasCallStack
  , Backend s
  ) =>
  -- | The Ilas to encode to get the hash from
  GenIla ->
  -- | Unused
  BlackBoxContext ->
  -- | The output JSON content
  State s (Doc ())
renderHDL ila _ = getAp $ expr True (DSL.bvLit 32 $ toInteger ila.hash).eex

-- | Actually render JSON
renderJSON ::
  forall s.
  ( HasCallStack
  , Backend s
  ) =>
  -- | The Ilas to encode to JSON
  GenIla ->
  -- | Unused
  BlackBoxContext ->
  -- | The output JSON content
  State s (Doc ())
renderJSON ila _ = pure [__i|#{encode ila}|]
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
  -- ^ The amount of samples it stores *after* triggering
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
  , bufferSize :: Word32
  , hash :: Word32
  , signals :: [GenSignal]
  }
  deriving (Generic, Show, ToJSON, Eq, Hashable)
