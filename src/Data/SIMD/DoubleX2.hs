module Data.SIMD.DoubleX2 where
import GHC.Exts

data DoubleX2 = DX2# DoubleX2#

plusDoubleX2 :: DoubleX2 -> DoubleX2 -> DoubleX2
plusDoubleX2 (DX2# d1) (DX2# d2) = DX2# (plusDoubleX2# d1 d2)

minusDoubleX2 :: DoubleX2 -> DoubleX2 -> DoubleX2
minusDoubleX2 (DX2# d1) (DX2# d2) = DX2# (minusDoubleX2# d1 d2)

timesDoubleX2 :: DoubleX2 -> DoubleX2 -> DoubleX2
timesDoubleX2 (DX2# d1) (DX2# d2) = DX2# (timesDoubleX2# d1 d2)

negateDoubleX2 :: DoubleX2 -> DoubleX2
negateDoubleX2 (DX2# d) = DX2# (negateDoubleX2# d)

broadcastDoubleX2 :: Double -> DoubleX2
broadcastDoubleX2 (D# x) = DX2# (broadcastDoubleX2# x)

packDoubleX2 :: (Double, Double) -> DoubleX2
packDoubleX2 (D# x1, D# x2) = DX2# (packDoubleX2# (# x1, x2 #))

unpackDoubleX2 :: DoubleX2 -> (Double, Double)
unpackDoubleX2 (DX2# d) = case unpackDoubleX2# d of
    (# x1, x2 #) -> (D# x1, D# x2)

mapDoubleX2 :: (Double -> Double) -> DoubleX2 -> DoubleX2
mapDoubleX2 func d = case unpackDoubleX2 d of
  (d1, d2) -> packDoubleX2 (func d1, func d2)

instance Num DoubleX2 where
  (+) = plusDoubleX2
  (-) = minusDoubleX2
  (*) = timesDoubleX2
  negate = negateDoubleX2
  abs    = mapDoubleX2 abs
  signum = mapDoubleX2 signum
  fromInteger = broadcastDoubleX2 . fromInteger
