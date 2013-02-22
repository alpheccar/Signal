{-# LANGUAGE TypeOperators,TypeFamilies,MultiParamTypeClasses,FlexibleContexts, BangPatterns,PolyKinds, DataKinds, GADTs, GeneralizedNewtypeDeriving,DeriveDataTypeable,FlexibleInstances,ScopedTypeVariables #-}
{- | Fixed point arithmetic 

Fixed point arithmetic with signed, unsigned and saturation

-}
module Fixed(
      nbFractionalBits
    , HasDoubleRepresentation(..)
    , FixedPoint(..)
    , withSaturation 
    , withoutSaturation
    , Fixed
    , Saturation(..)
    , Int16
    , Int32 
    , Int40
    , Rounding(..)
    , AccurateMul(..)
    , Conversion(..)
    ) where 

import Data.Typeable
import GHC.TypeLits
import Data.Word
import Data.Int
import Data.Bits
import Text.Printf
import Data.Ratio
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Control.Monad(liftM)

--import Debug.Trace

--debug a = trace (show a) a 

newtype Int40 = Int40 Int64 deriving(Eq,Ord,Integral,Real)

instance Enum Int40 where 
    succ (Int40 a) | a == maxBound = error "Can't increase the max bound"
                   | otherwise = Int40 (succ a) 
    pred (Int40 a) | a == minBound = error "Can't decrease the min bound"
                   | otherwise = Int40 (pred a) 
    toEnum a = Int40 $ (toEnum a) .&. 0x0FFFFFFFFFF 
    fromEnum (Int40 a) = fromEnum a 
    enumFromTo (Int40 a) (Int40 b) = map Int40 (enumFromTo a b) 
    enumFromThenTo (Int40 a) (Int40 b) (Int40 c) = map Int40 (enumFromThenTo a b c)

instance Num Int40 where 
    (Int40 a) + (Int40 b) = Int40 $ (a+b) .&. 0x0FFFFFFFFFF
    (Int40 a) - (Int40 b) = Int40 $ (a-b) .&. 0x0FFFFFFFFFF 
    (Int40 a) * (Int40 b) = Int40 $ (a*b) .&. 0x0FFFFFFFFFF
    abs (Int40 a) = Int40 (abs a)
    signum (Int40 a) = Int40 (signum a) 
    fromInteger a = Int40 $ (fromInteger a) .&. 0x0FFFFFFFFFF


instance Bounded Int40 where 
    minBound = Int40 0xffffffff8000000000
    maxBound = Int40 0x000000007fffffffff

-- | Signals can use values which are not directly doubles (like voltage etc ...)
-- Those values must be convertible into Double for display on a graphic
class HasDoubleRepresentation a where
    toDouble :: a -> Double 
    fromDouble :: Double -> a

instance HasDoubleRepresentation Double where 
    toDouble = id 
    fromDouble = id

instance HasDoubleRepresentation Float where 
    toDouble = realToFrac 
    fromDouble = realToFrac

instance HasDoubleRepresentation Int where 
    toDouble = fromIntegral
    fromDouble = floor

class Rounding a s where
     roundf :: (SingI n,Num (Fixed a n s)) => Fixed a n s -> Fixed Int16 0 s 

instance Rounding Int32 s where
  roundf f = 
    let Fixed r = f + fromRawValue (1 `shiftL` ((nbFractionalBits f) - 1))
    in
    Fixed (fromIntegral $ r `shiftR` (nbFractionalBits f))

instance Rounding Int40 Unsaturated where
  roundf f = 
    let Fixed (Int40 r) = f + fromRawValue (Int40 (1 `shiftL` ((nbFractionalBits f) - 1)))
    in
    Fixed (fromIntegral $ r `shiftR` (nbFractionalBits f))

instance Rounding Int40 Saturated where
  roundf f = 
    let Fixed (Int40 r) = f + fromRawValue (Int40 (1 `shiftL` ((nbFractionalBits f) - 1)))
    in
    Fixed (fromIntegral $ (saturate32 r) `shiftR` (nbFractionalBits f))

newtype Fixed :: * -> Nat -> Saturation -> * where Fixed :: a -> Fixed a n sa

data Saturation = Saturated | Unsaturated 

newtype instance Sing (n :: Saturation) = Sat Saturation

instance SingI Saturated where 
  sing = Sat Saturated

instance SingI Unsaturated where 
  sing = Sat Unsaturated

instance SingE (Kind :: Saturation) Saturation where
  fromSing (Sat n) = n

newtype instance U.MVector s (Fixed a n sat)  = MVFixed (U.MVector s a)
newtype instance U.Vector    (Fixed a n sat) = VFixed (U.Vector a)

instance SingI n => Real (Fixed Int16 n Saturated) where 
  toRational = toRational . toDouble 

instance SingI n => Real (Fixed Int16 n Unsaturated) where 
  toRational = toRational . toDouble 

instance SingI n => Real (Fixed Int32 n Saturated) where 
  toRational = toRational . toDouble 

instance SingI n => Real (Fixed Int32 n Unsaturated) where 
  toRational = toRational . toDouble 

instance SingI n => Real (Fixed Int40 n Saturated) where 
  toRational = toRational . toDouble 

instance SingI n => Real (Fixed Int40 n Unsaturated) where 
  toRational = toRational . toDouble 

instance SingI n => RealFrac (Fixed Int16 n Saturated) where 
  properFraction a = let (b,na) = properFraction (toDouble a)
                     in
                     (b, fromDouble na)

instance SingI n => RealFrac (Fixed Int16 n Unsaturated) where 
  properFraction a = let (b,na) = properFraction (toDouble a)
                     in
                     (b, fromDouble na)

instance SingI n => RealFrac (Fixed Int32 n Saturated) where 
  properFraction a = let (b,na) = properFraction (toDouble a)
                     in
                     (b, fromDouble na)

instance SingI n => RealFrac (Fixed Int32 n Unsaturated) where 
  properFraction a = let (b,na) = properFraction (toDouble a)
                     in
                     (b, fromDouble na)

instance SingI n => Floating (Fixed Int16 n Unsaturated) where 
   pi = fromDouble pi
   exp = fromDouble . exp . toDouble
   log = fromDouble . log . toDouble
   sin = fromDouble . sin . toDouble
   cos = fromDouble . cos . toDouble
   sinh = fromDouble . sinh . toDouble
   cosh = fromDouble . cosh . toDouble
   asin = fromDouble . asin . toDouble
   acos = fromDouble . acos . toDouble
   atan = fromDouble . atan . toDouble
   asinh = fromDouble . asinh . toDouble
   acosh = fromDouble . acosh . toDouble
   atanh = fromDouble . atanh . toDouble 

instance SingI n => Floating (Fixed Int16 n Saturated) where 
   pi = fromDouble pi
   exp = fromDouble . exp . toDouble
   log = fromDouble . log . toDouble
   sin = fromDouble . sin . toDouble
   cos = fromDouble . cos . toDouble
   sinh = fromDouble . sinh . toDouble
   cosh = fromDouble . cosh . toDouble
   asin = fromDouble . asin . toDouble
   acos = fromDouble . acos . toDouble
   atan = fromDouble . atan . toDouble
   asinh = fromDouble . asinh . toDouble
   acosh = fromDouble . acosh . toDouble
   atanh = fromDouble . atanh . toDouble  

instance SingI n => Floating (Fixed Int32 n Unsaturated) where 
   pi = fromDouble pi
   exp = fromDouble . exp . toDouble
   log = fromDouble . log . toDouble
   sin = fromDouble . sin . toDouble
   cos = fromDouble . cos . toDouble
   sinh = fromDouble . sinh . toDouble
   cosh = fromDouble . cosh . toDouble
   asin = fromDouble . asin . toDouble
   acos = fromDouble . acos . toDouble
   atan = fromDouble . atan . toDouble
   asinh = fromDouble . asinh . toDouble
   acosh = fromDouble . acosh . toDouble
   atanh = fromDouble . atanh . toDouble 

instance SingI n => Floating (Fixed Int32 n Saturated) where 
   pi = fromDouble pi
   exp = fromDouble . exp . toDouble
   log = fromDouble . log . toDouble
   sin = fromDouble . sin . toDouble
   cos = fromDouble . cos . toDouble
   sinh = fromDouble . sinh . toDouble
   cosh = fromDouble . cosh . toDouble
   asin = fromDouble . asin . toDouble
   acos = fromDouble . acos . toDouble
   atan = fromDouble . atan . toDouble
   asinh = fromDouble . asinh . toDouble
   acosh = fromDouble . acosh . toDouble
   atanh = fromDouble . atanh . toDouble  

instance SingI n => RealFloat (Fixed Int16 n Saturated) where
  isInfinite = isInfinite . toDouble
  isDenormalized = isDenormalized . toDouble
  isNegativeZero = isNegativeZero . toDouble
  isIEEE = isIEEE . toDouble
  isNaN = isNaN . toDouble
  encodeFloat a b = fromDouble (encodeFloat a b)
  decodeFloat = decodeFloat . toDouble
  floatRange = floatRange . toDouble
  floatRadix = floatRadix . toDouble
  floatDigits = floatDigits . toDouble
  exponent a = exponent (toDouble a)
  significand = fromDouble . significand . toDouble 
  scaleFloat i = fromDouble . scaleFloat i . toDouble 
  atan2 a b = fromDouble (atan2 (toDouble a) (toDouble b))

instance SingI n => RealFloat (Fixed Int16 n Unsaturated) where
  isInfinite = isInfinite . toDouble
  isDenormalized = isDenormalized . toDouble
  isNegativeZero = isNegativeZero . toDouble
  isIEEE = isIEEE . toDouble
  isNaN = isNaN . toDouble
  encodeFloat a b = fromDouble (encodeFloat a b)
  decodeFloat = decodeFloat . toDouble 
  floatRange = floatRange . toDouble
  floatRadix = floatRadix . toDouble
  floatDigits = floatDigits . toDouble
  exponent a = exponent (toDouble a)
  significand = fromDouble . significand . toDouble 
  scaleFloat i = fromDouble . scaleFloat i . toDouble 
  atan2 a b = fromDouble (atan2 (toDouble a) (toDouble b))

instance SingI n => RealFloat (Fixed Int32 n Saturated) where
  isInfinite = isInfinite . toDouble
  isDenormalized = isDenormalized . toDouble
  isNegativeZero = isNegativeZero . toDouble
  isIEEE = isIEEE . toDouble
  isNaN = isNaN . toDouble 
  encodeFloat a b = fromDouble (encodeFloat a b)
  decodeFloat = decodeFloat . toDouble
  floatRange = floatRange . toDouble
  floatRadix = floatRadix . toDouble
  floatDigits = floatDigits . toDouble
  exponent a = exponent (toDouble a)
  significand = fromDouble . significand . toDouble 
  scaleFloat i = fromDouble . scaleFloat i . toDouble 
  atan2 a b = fromDouble (atan2 (toDouble a) (toDouble b))

instance SingI n => RealFloat (Fixed Int32 n Unsaturated) where
  isInfinite = isInfinite . toDouble
  isDenormalized = isDenormalized . toDouble
  isNegativeZero = isNegativeZero . toDouble
  isIEEE = isIEEE . toDouble
  isNaN = isNaN . toDouble
  encodeFloat a b = fromDouble (encodeFloat a b)
  decodeFloat = decodeFloat . toDouble 
  floatRange = floatRange . toDouble
  floatRadix = floatRadix . toDouble
  floatDigits = floatDigits . toDouble
  exponent a = exponent (toDouble a)
  significand = fromDouble . significand . toDouble 
  scaleFloat i = fromDouble . scaleFloat i . toDouble 
  atan2 a b = fromDouble (atan2 (toDouble a) (toDouble b))

instance (U.Unbox a) => M.MVector U.MVector (Fixed a n s) where
   {-# INLINE basicLength #-}
   {-# INLINE basicUnsafeSlice #-}
   {-# INLINE basicOverlaps #-}
   {-# INLINE basicUnsafeNew #-}
   {-# INLINE basicUnsafeRead #-}
   {-# INLINE basicUnsafeWrite #-}
   basicLength (MVFixed v) = M.basicLength v
   basicUnsafeSlice a b (MVFixed v) = MVFixed $ M.basicUnsafeSlice a b v
   basicOverlaps (MVFixed a) (MVFixed b) = M.basicOverlaps a b 
   basicUnsafeNew n = MVFixed `liftM` M.basicUnsafeNew n
   basicUnsafeRead (MVFixed v) i = do 
    r <- M.basicUnsafeRead v i
    return (Fixed r)
   basicUnsafeWrite (MVFixed v) i (Fixed r) = do 
    M.basicUnsafeWrite v i r 

instance (U.Unbox a) => G.Vector U.Vector (Fixed a n s) where
   {-# INLINE basicLength #-}
   {-# INLINE basicUnsafeFreeze #-}
   {-# INLINE basicUnsafeThaw #-}
   {-# INLINE basicUnsafeSlice #-}
   {-# INLINE basicUnsafeIndexM #-}
   basicLength (VFixed v) = G.basicLength v
   basicUnsafeFreeze (MVFixed v) = VFixed `liftM`G.basicUnsafeFreeze v
   basicUnsafeThaw (VFixed v) = MVFixed `liftM`G.basicUnsafeThaw v
   basicUnsafeSlice a b (VFixed v) = VFixed (G.basicUnsafeSlice a b v)
   basicUnsafeIndexM (VFixed v) i = Fixed `liftM` G.basicUnsafeIndexM v i 

instance (RealFloat a, U.Unbox a) => U.Unbox (Fixed a n sat)

getFract :: Fixed a (n :: Nat) sa -> Sing n -> Integer 
getFract _ s = fromSing s 

nbFractionalBits :: SingI n => Fixed a (n :: Nat) sa -> Int 
nbFractionalBits f = fromIntegral $ getFract f sing

saturate16 :: Int32 -> Int16 
saturate16 i | i > 0X00007fff = maxBound
             | i < 0xffff8000 = minBound 
             | otherwise = fromIntegral i

saturate16' :: Int64 -> Int16 
saturate16' i | i > 0X00007fff = maxBound
              | i < 0xffff8000 = minBound 
              | otherwise = fromIntegral i

saturate32 :: Int64 -> Int32 
saturate32 i | i > 0X000000007fffffff = maxBound
             | i < 0xffffffff80000000 = minBound 
             | otherwise = fromIntegral i

saturate40 :: Int64 -> Int40 
saturate40 i | i > 0x000000007fffffffff = maxBound
             | i < 0xffffffff8000000000 = minBound 
             | otherwise = fromIntegral i

instance Eq a => Eq (Fixed a n sa) where
    (Fixed a) == (Fixed b) = a == b

instance Ord a => Ord (Fixed a n sa) where 
    compare (Fixed a) (Fixed b) = compare a b

class AccurateMul a b where 
  amul :: SingI n => Fixed a n s -> Fixed a n s -> Fixed b (n + n) s


instance AccurateMul Int16 Int32 where 
  amul fa@(Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int64 
            lb = fromIntegral b :: Int64 
            r = (la * lb)
        in
        Fixed (fromIntegral r)

instance AccurateMul Int16 Int40 where 
  amul fa@(Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int64 
            lb = fromIntegral b :: Int64 
            r = (la * lb)
        in
        Fixed (fromIntegral r)

class Conversion a b where
  convert :: a -> b 

instance (SingI na, SingI nb,Integral a, Num b) => Conversion (Fixed a na s) (Fixed b nb Unsaturated) where 
  convert fa@(Fixed a) = 
    let la = fromIntegral a :: Int64 
        lb = la `shift` ((nbFractionalBits fb) - (nbFractionalBits fa)) 
        fb = Fixed (fromIntegral lb)
    in 
    fb

instance (SingI na, SingI nb,Integral a) => Conversion (Fixed a na s) (Fixed Int16 nb Saturated) where 
  convert fa@(Fixed a) =
    let la = fromIntegral a :: Int64 
        lb = la `shift` ((nbFractionalBits fb) - (nbFractionalBits fa))
        fb = Fixed (fromIntegral $ saturate16' lb) 
    in 
    fb

instance (SingI na, SingI nb,Integral a) => Conversion (Fixed a na s) (Fixed Int32 nb Saturated) where 
  convert fa@(Fixed a) =
    let la = fromIntegral a :: Int64 
        lb = la `shift` ((nbFractionalBits fb) - (nbFractionalBits fa)) 
        fb = Fixed (fromIntegral $ saturate32 lb)
    in 
    fb

instance (SingI na, SingI nb,Integral a) => Conversion (Fixed a na s) (Fixed Int40 nb Saturated) where 
  convert fa@(Fixed a) = 
    let la = fromIntegral a :: Int64 
        lb = la `shift` ((nbFractionalBits fb) - (nbFractionalBits fa)) 
        fb = Fixed (fromIntegral $ saturate40 lb)
    in 
    fb

instance SingI n => Num (Fixed Int16 n Unsaturated) where 
    (+) (Fixed a) (Fixed b) = Fixed (a + b)
    (-) (Fixed a) (Fixed b) = Fixed (a - b)
    abs (Fixed a) = Fixed (abs a)
    (*) fa@(Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int32 
            lb = fromIntegral b :: Int32 
            r = (la * lb)  `shiftR` (nbFractionalBits fa) 
        in
        Fixed (fromIntegral r)
    signum (Fixed a) = Fixed (signum a) 
    fromInteger a = ra
     where 
        b = a `shiftL` (nbFractionalBits ra) 
        ra | b > 0x7FFF = Fixed maxBound 
           | b < (fromIntegral (0xFFFF8000 :: Int32)) = Fixed minBound 
           | otherwise = Fixed (fromIntegral b)

instance SingI n => Num (Fixed Int32 n Unsaturated) where 
    (+) (Fixed a) (Fixed b) = Fixed  (a + b)
    (-) (Fixed a) (Fixed b) = Fixed (a - b)
    abs (Fixed a) | a == minBound = Fixed maxBound 
                  | otherwise = Fixed (abs a)
    (*) fa@(Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int64 
            lb = fromIntegral b :: Int64 
            r = (la * lb)  `shiftR` (nbFractionalBits fa) 
        in
        Fixed (fromIntegral r)
    signum (Fixed a) = Fixed (signum a) 
    fromInteger a = ra
     where 
        b = a `shiftL` (nbFractionalBits ra) 
        ra | b > 0x7FFFFFFF = Fixed maxBound 
           | b < (fromIntegral (0x80000000 :: Int32)) = Fixed minBound 
           | otherwise = Fixed (fromIntegral b)

instance SingI n => Num (Fixed Int40 n Unsaturated) where 
    (+) (Fixed a) (Fixed b) = Fixed  (a + b)
    (-) (Fixed a) (Fixed b) = Fixed (a - b)
    abs (Fixed a) | a == minBound = Fixed maxBound 
                  | otherwise = Fixed (abs a)
    (*) fa@(Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int64 
            lb = fromIntegral b :: Int64 
            r = (la * lb)  `shiftR` (nbFractionalBits fa) 
        in
        Fixed (fromIntegral r)
    signum (Fixed a) = Fixed (signum a) 
    fromInteger a = ra
     where 
        b = a `shiftL` (nbFractionalBits ra) 
        ra | b > 0x7FFFFFFFFF = Fixed maxBound 
           | b < (fromIntegral (0x8000000000 :: Int32)) = Fixed minBound 
           | otherwise = Fixed (fromIntegral b)

instance SingI n => Num (Fixed Int32 n Saturated) where 
    (+) (Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int64 
            lb = fromIntegral a :: Int64 
        in
        Fixed  (saturate32 $ la + lb)
    (-) (Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int64 
            lb = fromIntegral a :: Int64 
        in
        Fixed (saturate32 $ la - lb)
    abs (Fixed a) | a == minBound = Fixed maxBound 
                  | otherwise = Fixed (abs a)
    (*) fa@(Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int64 
            lb = fromIntegral b :: Int64 
            r = (la * lb)  `shiftR` (nbFractionalBits fa) 
        in
        Fixed (saturate32 r)
    signum (Fixed a) = Fixed (signum a) 
    fromInteger a = ra
     where 
        b = a `shiftL` (nbFractionalBits ra) 
        ra | b > 0x7FFFFFFF = Fixed maxBound 
           | b < (fromIntegral (0x80000000 :: Int32)) = Fixed minBound 
           | otherwise = Fixed (fromIntegral b)

instance SingI n => Num (Fixed Int40 n Saturated) where 
    (+) (Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int64 
            lb = fromIntegral a :: Int64 
        in
        Fixed  (saturate40 $ la + lb)
    (-) (Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int64 
            lb = fromIntegral a :: Int64 
        in
        Fixed (saturate40 $ la - lb)
    abs (Fixed a) | a == minBound = Fixed maxBound 
                  | otherwise = Fixed (abs a)
    (*) fa@(Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int64 
            lb = fromIntegral b :: Int64 
            r = (la * lb)  `shiftR` (nbFractionalBits fa) 
        in
        Fixed (saturate40 r)
    signum (Fixed a) = Fixed (signum a) 
    fromInteger a = ra
     where 
        b = a `shiftL` (nbFractionalBits ra) 
        ra | b > 0x7FFFFFFFFF = Fixed maxBound 
           | b < (fromIntegral (0x8000000000 :: Int64)) = Fixed minBound 
           | otherwise = Fixed (fromIntegral b)

instance SingI n => Num (Fixed Int16 n Saturated) where 
    (+) (Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int32 
            lb = fromIntegral b :: Int32 
        in 
        Fixed (saturate16 (la + lb))
    (-) (Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int32 
            lb = fromIntegral b :: Int32
        in 
        Fixed (saturate16 (la - lb))
    abs (Fixed a) | a == minBound = Fixed maxBound 
                  | otherwise = Fixed (abs a)
    (*) fa@(Fixed a) (Fixed b) = 
        let la = fromIntegral a :: Int32 
            lb = fromIntegral b :: Int32 
            r = (la * lb)  `shiftR` (nbFractionalBits fa) 
        in
        Fixed (saturate16 r)
    signum (Fixed a) = Fixed (signum a) 
    fromInteger a = ra
     where 
        b = a `shiftL` (nbFractionalBits ra) 
        ra | b > 0x7FFF = Fixed maxBound 
           | b < (fromIntegral (0xFFFF8000 :: Int32)) = Fixed minBound 
           | otherwise = Fixed (fromIntegral b)


instance (SingI n, Integral a) => Show (Fixed a n s) where 
    show f@(Fixed a) = 
        let fp = nbFractionalBits f 
            fr = 2**(- fromIntegral fp) :: Double
        in 
        printf ("%f") ((fromIntegral a)*fr)


withSaturation :: Fixed n s sa -> Fixed n s Saturated 
withSaturation (Fixed a) = Fixed a 

withoutSaturation :: Fixed n s sa -> Fixed n s Unsaturated 
withoutSaturation (Fixed a) = Fixed a 


standardInt16Div :: (SingI n,Num (Fixed Int16 n s)) => Fixed Int16 n s -> Fixed Int16 n s -> Fixed Int16 n s 
standardInt16Div fa@(Fixed a) (Fixed b) = 
        let s | a == 0 = 0
              | b == 0 = error "Can't divide by 0"
              | signum a * signum b < 0 = -1 
              | otherwise = 1 
            rescale f x y | x > y = rescale (f+1) x (y `shiftL` 1) 
                          | otherwise = (f,x,y) 
            (f,ra,rb) = rescale 0 (abs a) (abs b) 
            la = (fromIntegral ra :: Int32) `shiftL` 16 
            lb = fromIntegral rb :: Int32
            lr = la `quot` lb 
            r = s*(lr `shift` (f + nbFractionalBits fa - 16))
            nr | r >= 0x10000 = 0x7FFF 
               | otherwise = r
        in 
        fromRawValue $ (fromIntegral nr)

standardInt16FromRational :: (SingI n,HasDoubleRepresentation (Fixed Int16 n s),Num (Fixed Int16 n s)) => Rational -> Fixed Int16 n s
standardInt16FromRational r = 
        let n = numerator r 
            d = denominator r 
        in 
       fromDouble $ fromIntegral n / fromIntegral d

standardInt32Div :: (SingI n,Num (Fixed Int32 n s)) => Fixed Int32 n s -> Fixed Int32 n s -> Fixed Int32 n s 
standardInt32Div fa@(Fixed a) (Fixed b) = 
        let s | a == 0 = 0
              | b == 0 = error "Can't divide by 0"
              | signum a * signum b < 0 = -1 
              | otherwise = 1 
            rescale f x y | x > y = rescale (f+1) x (y `shiftL` 1) 
                          | otherwise = (f,x,y) 
            (f,ra,rb) = rescale 0 (abs a) (abs b) 
            la = (fromIntegral ra :: Int64) `shiftL` 32 
            lb = fromIntegral rb :: Int64
            lr = la `quot` lb 
            r = s*(lr `shift` (f + nbFractionalBits fa - 32))
            nr | r >= 0x100000000 = 0x7FFFFFFF 
               | otherwise = r
        in 
        fromRawValue $ (fromIntegral nr)

standardInt32FromRational :: (SingI n,HasDoubleRepresentation (Fixed Int32 n s),Num (Fixed Int32 n s)) => Rational -> Fixed Int32 n s
standardInt32FromRational r = 
        let n = numerator r 
            d = denominator r 
        in 
        fromDouble $ fromIntegral n / fromIntegral d

instance SingI n => Fractional (Fixed Int16 n Saturated) where 
    (/)  = standardInt16Div
    fromRational = standardInt16FromRational

instance SingI n => Fractional (Fixed Int16 n Unsaturated) where 
    (/)  = standardInt16Div
    fromRational = standardInt16FromRational

instance SingI n => Fractional (Fixed Int32 n Saturated) where 
    (/)  = standardInt32Div
    fromRational = standardInt32FromRational

instance SingI n => Fractional (Fixed Int32 n Unsaturated) where 
    (/)  = standardInt32Div
    fromRational = standardInt32FromRational

class FixedPoint a where
    fromRawValue :: a -> Fixed a n s 
    toRawValue :: Fixed a n s -> a

instance FixedPoint Int16 where 
    fromRawValue = Fixed
    toRawValue (Fixed a) = a

instance FixedPoint Int32 where 
    fromRawValue = Fixed
    toRawValue (Fixed a) = a

instance FixedPoint Word16 where 
    fromRawValue = Fixed
    toRawValue (Fixed a) = a

instance FixedPoint Word32 where 
    fromRawValue = Fixed
    toRawValue (Fixed a) = a

instance FixedPoint Int40 where 
    fromRawValue = Fixed
    toRawValue (Fixed a) = a

genericFromDouble16 :: (SingI n, Num (Fixed Int16 n sa)) => Double -> Fixed Int16 n sa 
genericFromDouble16 a = let la = saturate16 (floor (a * 2**(fromIntegral $ nbFractionalBits ra)) :: Int32)
                            ra = fromRawValue la
                        in 
                        ra

genericFromDouble32 :: (SingI n, Num (Fixed Int32 n sa)) => Double -> Fixed Int32 n sa 
genericFromDouble32 a = let la = saturate32 (floor (a * 2**(fromIntegral $ nbFractionalBits ra)) :: Int64)
                            ra = fromRawValue la
                        in 
                        ra

genericFromDouble40 :: (SingI n, Num (Fixed Int40 n sa)) => Double -> Fixed Int40 n sa 
genericFromDouble40 a = let la = saturate40 (floor (a * 2**(fromIntegral $ nbFractionalBits ra)) :: Int64)
                            ra = fromRawValue la
                        in 
                        ra

genericToDouble :: (SingI n, Integral a) => Fixed a n s -> Double 
genericToDouble f@(Fixed a) = fromIntegral a * 2**(- fromIntegral (nbFractionalBits f))

instance SingI n => HasDoubleRepresentation (Fixed Int16 n Unsaturated) where 
   toDouble = genericToDouble
   fromDouble = genericFromDouble16

instance SingI n => HasDoubleRepresentation (Fixed Int16 n Saturated) where 
   toDouble = genericToDouble
   fromDouble = genericFromDouble16


instance SingI n => HasDoubleRepresentation (Fixed Int32 n Unsaturated) where 
   toDouble = genericToDouble
   fromDouble = genericFromDouble32

instance SingI n => HasDoubleRepresentation (Fixed Int32 n Saturated) where 
   toDouble = genericToDouble
   fromDouble = genericFromDouble32

instance SingI n => HasDoubleRepresentation (Fixed Int40 n Unsaturated) where 
   toDouble = genericToDouble
   fromDouble = genericFromDouble40

instance SingI n => HasDoubleRepresentation (Fixed Int40 n Saturated) where 
   toDouble = genericToDouble
   fromDouble = genericFromDouble40