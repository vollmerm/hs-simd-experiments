module Data.SIMD.FloatX4 where
import GHC.Exts

data FloatX4  = FX4# FloatX4#

plusFloatX4 :: FloatX4 -> FloatX4 -> FloatX4
plusFloatX4 (FX4# f1) (FX4# f2) = FX4# (plusFloatX4# f1 f2)

minusFloatX4 :: FloatX4 -> FloatX4 -> FloatX4
minusFloatX4 (FX4# f1) (FX4# f2) = FX4# (minusFloatX4# f1 f2)

timesFloatX4 :: FloatX4 -> FloatX4 -> FloatX4
timesFloatX4 (FX4# f1) (FX4# f2) = FX4# (timesFloatX4# f1 f2)

negateFloatX4 :: FloatX4 -> FloatX4
negateFloatX4 (FX4# f) = FX4# (negateFloatX4# f)

broadcastFloatX4 :: Float -> FloatX4
broadcastFloatX4 (F# x) = FX4# (broadcastFloatX4# x)

packFloatX4 :: (Float, Float, Float, Float) -> FloatX4
packFloatX4 (F# x1, F# x2, F# x3, F# x4) = FX4# (packFloatX4# (# x1, x2, x3, x4 #))

unpackFloatX4 :: FloatX4 -> (Float, Float, Float, Float)
unpackFloatX4 (FX4# f) = case unpackFloatX4# f of
    (# x1, x2, x3, x4 #) -> (F# x1, F# x2, F# x3, F# x4)

mapFloatX4 :: (Float -> Float) -> FloatX4 -> FloatX4
mapFloatX4 func f = case unpackFloatX4 f of
  (f1, f2, f3, f4) -> packFloatX4 (func f1, func f2, func f3, func f4)

instance Num FloatX4 where
  (+) = plusFloatX4
  (-) = minusFloatX4
  (*) = timesFloatX4
  negate = negateFloatX4
  abs    = mapFloatX4 abs
  signum = mapFloatX4 signum
  fromInteger = broadcastFloatX4 . fromInteger
