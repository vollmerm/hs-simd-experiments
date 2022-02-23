{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

module Main where
import GHC.Exts
import Control.Monad.Primitive
import Control.Monad.ST
-- import Data.SIMD.DoubleX2
-- import Data.SIMD.FloatX4
-- import Data.SIMD

main :: IO ()
main = do
  print $ runST $ primitive $ \s ->
    case createDoubleArray# s 10# of
      (# s', arr #) -> case fillDoubleArray# s' arr (broadcastDoubleX2# 5.0##) of
        (# s'', arr' #) -> case sumVector# s' arr' of
          (# s''', val #) -> (# s''', D# val #) -- 50.0



sumVector# :: State# s -> MutableByteArray# s -> (# State# s, Double# #)
sumVector# s arr = mapByVectorAtIndex# s arr sumByVectorAtIndex# (+##)

createDoubleArray# :: State# s -> Int# -> (# State# s, MutableByteArray# s #)
createDoubleArray# s size =
  case newByteArray# (size *# 8#) s of
    (# s', arr #) -> fillDoubleArray# s' arr (broadcastDoubleX2# 0.0##)

fillDoubleArray# :: State# s -> MutableByteArray# s -> DoubleX2# -> (# State# s, MutableByteArray# s #)
fillDoubleArray# s arr val =
  let

    arrSize :: Int#
    arrSize = (sizeofMutableByteArray# arr) `quotInt#` 8#

    inner# :: Int# -> (# State# s2, MutableByteArray# s2 #) -> (# State# s2, MutableByteArray# s2 #)
    inner# ind (# s, arr #) =
      if isTrue# (ind <# arrSize)
      then let s' = writeDoubleArrayAsDoubleX2# arr ind val s
           in inner# (ind +# 2#) (# s', arr #)
      else (# s, arr #)

  in inner# 0# (# s, arr #)

{-# INLINE sumByVectorAtIndex# #-}
sumByVectorAtIndex# :: State# s -> MutableByteArray# s -> Int# -> DoubleX2# -> (# State# s, DoubleX2# #)
sumByVectorAtIndex# s arr ind acc  =
  case readDoubleArrayAsDoubleX2# arr ind s of
    (# s', vec #) -> (# s', plusDoubleX2# vec acc #)

{-# INLINE mapByVectorAtIndex# #-}
mapByVectorAtIndex# :: State# s
                    -> MutableByteArray# s
                    -> (forall s1. State# s1 -> MutableByteArray# s1 -> Int# -> DoubleX2# -> (# State# s1, DoubleX2# #))
                    -> (Double# -> Double# -> Double#)
                    -> (# State# s, Double# #)
mapByVectorAtIndex# s arr f1 f2 =
  let

    arrSize :: Int#
    arrSize = (sizeofMutableByteArray# arr) `quotInt#` 8#

    inner# :: MutableByteArray# s2 -> Int# -> (# State# s2, DoubleX2# #) -> (# State# s2, DoubleX2# #)
    inner# arr ind (# sa,acc #) =
      if isTrue# (ind <# arrSize)
      then inner# arr (ind +# 2#) (f1 sa arr ind acc)
      else (# sa, acc #)

  in case inner# arr 0# (# s, broadcastDoubleX2# 0.0## #) of
    (# s', vec #) -> case unpackDoubleX2# vec of
      (# d1, d2 #) -> (# s', d1 `f2` d2 #)

-- sumTwo :: State# s -> MutableByteArray# s -> Int# -> (# State# s, Double# #)
-- sumTwo s arr ind =
--   case readDoubleArrayAsDoubleX2# arr ind s of
--     (# s', vec #) -> case unpackDoubleX2# vec of
--       (# d1, d2 #) -> (# s', d1 +## d2 #)
