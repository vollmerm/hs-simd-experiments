{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.SIMD where
import Data.SIMD.DoubleX2
import Data.SIMD.FloatX4
import Data.Primitive.Types
import GHC.Exts

-- We should be able to use the Data.Primitive.ByteArray interface
-- with the Prim monad instead of manually passing around State# tokens.

instance Prim DoubleX2 where
  sizeOf# _ = 16#
  alignment# _ = 8#
  readByteArray# arr i s = case readDoubleArrayAsDoubleX2# arr i s of
    (# s, d #) -> (# s, DX2# d #)
  writeByteArray# arr i (DX2# d) = writeDoubleArrayAsDoubleX2# arr i d

instance Prim FloatX4 where
  sizeOf# _ = 16#
  alignment# _ = 4#
  readByteArray# arr i s = case readFloatArrayAsFloatX4# arr i s of
    (# s, d #) -> (# s, FX4# d #)
  writeByteArray# arr i (FX4# d) = writeFloatArrayAsFloatX4# arr i d


-- based on abhiroop/lift-vector
class (Num v, Real (Elem v)) => SIMD v where
  type Elem v
  type ElemTuple v
  zero     :: v
  vecSize  :: v -> Int
  elemSize :: v -> Int
  unpack   :: v -> ElemTuple v
  pack     :: ElemTuple v -> v

instance SIMD FloatX4 where
  type Elem      FloatX4 = Float
  type ElemTuple FloatX4 = (Float, Float, Float, Float)
  zero       = broadcastFloatX4 0
  vecSize  _ = 4
  elemSize _ = 4
  unpack     = unpackFloatX4
  pack       = packFloatX4

instance SIMD DoubleX2 where
  type Elem      DoubleX2 = Double
  type ElemTuple DoubleX2 = (Double, Double)
  zero       = broadcastDoubleX2 0
  vecSize  _ = 2
  elemSize _ = 8
  unpack     = unpackDoubleX2
  pack       = packDoubleX2

simdLength :: (SIMD v) => v -> Int
simdLength v = elemSize v * vecSize v
