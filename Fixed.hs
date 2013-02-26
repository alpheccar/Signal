{-# LANGUAGE TypeOperators,TypeFamilies,MultiParamTypeClasses,FlexibleContexts, BangPatterns,PolyKinds, DataKinds, GADTs, GeneralizedNewtypeDeriving,DeriveDataTypeable,FlexibleInstances,ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
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
    , Word16 
    , Word32
    , Word40
    , Int128
    , AccurateMul(..)
    , Conversion(..)
    , amulc
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
import Common(HasDoubleRepresentation(..))
import Data.Complex
import SpecialInt 

import Debug.Trace

debug a = trace (show a) a 

instance HasDoubleRepresentation Double where 
    toDouble = id 
    fromDouble = id

instance HasDoubleRepresentation Float where 
    toDouble = realToFrac 
    fromDouble = realToFrac

instance HasDoubleRepresentation Int where 
    toDouble = fromIntegral
    fromDouble = floor

newtype Fixed :: * -> Nat -> Saturation -> * where Fixed :: a -> Fixed a n sa

data Saturation = Saturated | Unsaturated deriving(Eq)

newtype instance Sing (n :: Saturation) = Sat Saturation

type instance (15 :: Nat) + (1 :: Nat)  = 16
type instance (15 :: Nat) + (2 :: Nat)  = 17
type instance (15 :: Nat) + (3 :: Nat)  = 18
type instance (15 :: Nat) + (4 :: Nat)  = 19
type instance (15 :: Nat) + (5 :: Nat)  = 20
type instance (15 :: Nat) + (6 :: Nat)  = 21
type instance (15 :: Nat) + (7 :: Nat)  = 22
type instance (15 :: Nat) + (8 :: Nat)  = 23
type instance (15 :: Nat) + (9 :: Nat)  = 24
type instance (15 :: Nat) + (10 :: Nat)  = 25
type instance (15 :: Nat) + (11 :: Nat)  = 26
type instance (15 :: Nat) + (12 :: Nat)  = 27
type instance (15 :: Nat) + (13 :: Nat)  = 28
type instance (15 :: Nat) + (14 :: Nat)  = 29
type instance (15 :: Nat) + (15 :: Nat)  = 30
type instance (15 :: Nat) + (16 :: Nat)  = 31

instance SingI Saturated where 
  sing = Sat Saturated

instance SingI Unsaturated where 
  sing = Sat Unsaturated

instance SingE (Kind :: Saturation) Saturation where
  fromSing (Sat n) = n

newtype instance U.MVector s (Fixed a n sat)  = MVFixed (U.MVector s a)
newtype instance U.Vector    (Fixed a n sat) = VFixed (U.Vector a)

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

getSaturationMode :: Fixed a n sa -> Sing sa -> Saturation 
getSaturationMode _ s = fromSing s 

saturationMode :: SingI sa => Fixed a n sa -> Saturation 
saturationMode f = getSaturationMode f sing

saturateWithMode :: (SingI sa, SaturateConstraint a)
                 => Fixed a n sa 
                 -> SuperInt a 
                 -> a 
saturateWithMode f su | saturationMode f == Unsaturated = fromIntegral su 
                      | otherwise = saturate (witness f) su
  where 
    witness (Fixed a) = a


instance Eq a => Eq (Fixed a n sa) where
    (Fixed a) == (Fixed b) = a == b

instance Ord a => Ord (Fixed a n sa) where 
    compare (Fixed a) (Fixed b) = compare a b

class AccurateMul a b where 
  amul :: (SingI na, SingI nb) => Fixed a na s -> Fixed a nb s -> Fixed b (na + nb) s

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

amulc :: (SingI na,SingI nb, AccurateMul a b, Num (Fixed b (na+nb) s)) 
      => Complex (Fixed a na s) 
      -> Complex (Fixed a nb s) 
      -> Complex (Fixed b (na+nb) s)
amulc (xr :+ xi) (yr :+ yi) = (amul xr yr - amul xi yi) :+ (amul xr yi + amul xi yr)

class Conversion a b where 
  convert :: (SingI na, SingI nb, SingI sb, Integral a, Bits a, Bits (SuperInt b), NumberInfo a, NumberInfo b, SaturateConstraint b) 
          => Fixed a na sa 
          -> Fixed b nb sb 
  convert fa@(Fixed a) = fb
     where 
       fb = Fixed b
       b | la <= lb = saturateWithMode fb (fromIntegral a `shift` shiftValue)
         | otherwise = saturateWithMode fb (fromIntegral (a `shift` shiftValue))
       sa = nbFractionalBits fa
       sb = nbFractionalBits fb 
       shiftValue = sb - sa
       la = nbBits a 
       lb = nbBits b 

instance Conversion Int32 Int16 where
instance Conversion Int16 Int32 where
instance Conversion Int40 Int16 where
instance Conversion Int16 Int40 where

instance Conversion Word32 Word16 where 
instance Conversion Word16 Word32 where 
instance Conversion Word40 Word16 where
instance Conversion Word16 Word40 where

instance Conversion Int16 Int16 where
instance Conversion Int32 Int32 where
instance Conversion Int40 Int40 where

instance Conversion Word16 Word16 where
instance Conversion Word32 Word32 where
instance Conversion Word40 Word40 where

genericOperator :: (SingI s, Integral (SuperInt a), Num (BaseValue a), Num (SuperInt a), Integral a, NumberInfo (SuperInt a), NumberInfo a, RawValue a)
                => (SuperInt a -> SuperInt a -> SuperInt a) 
                -> Fixed a n s 
                -> Fixed a n s 
                -> Fixed a n s
genericOperator op f@(Fixed a) (Fixed b) =         
        let la = (fromIntegral a)
            lb = (fromIntegral b)
        in
        Fixed (saturateWithMode f $ op la lb)

genericMulOperator :: (SingI s, SingI n, Bits (SuperInt a), Integral (SuperInt a), Num (BaseValue a), Num (SuperInt a), Integral a, NumberInfo (SuperInt a), NumberInfo a, RawValue a)
                   => Fixed a n s 
                   -> Fixed a n s 
                   -> Fixed a n s
genericMulOperator f@(Fixed a) (Fixed b) =         
        let la = (fromIntegral a)
            lb = (fromIntegral b)
            r = (la * lb) `shiftR`(nbFractionalBits f)
        in
        Fixed (saturateWithMode f $ r)

genericAbs (Fixed a) | a == minBound = Fixed maxBound 
                     | otherwise = Fixed (abs a)

genericFromInteger a = r
     where 
        b = a `shiftL` (nbFractionalBits r) 
        theMax = maxBound 
        theMin = minBound
        r | b > fromIntegral theMax = Fixed theMax 
          | b < fromIntegral theMin = Fixed theMin 
          | otherwise = Fixed (fromIntegral b)

genericProperFraction f@(Fixed a) = 
  let l = nbFractionalBits f 
      b = a `shiftR` l 
      na = f - fromIntegral b
  in 
  (fromIntegral b,na)

#define FIXED_INSTANCES(INT) \
instance (SingI n, SingI s) => Num (Fixed INT n s) where {\
     (+) = genericOperator (+) \
;    (-) = genericOperator (-) \
;    abs = genericAbs \
;    (*) = genericMulOperator \
;    signum (Fixed a) = Fixed (signum a) \
;    fromInteger = genericFromInteger}; \
instance FixedPoint INT where {\
     fromRawValue = Fixed \
;    toRawValue (Fixed a) = a}; \
instance (SingI n, SingI s) => HasDoubleRepresentation (Fixed INT n s) where {\
   toDouble = genericToDouble \
;  fromDouble = genericFromDouble}; \
instance (SingI n, SingI s) => Fractional (Fixed INT n s) where {\
    (/)  = genericDiv \
;   fromRational = genericFromRational}; \
instance (SingI n,SingI s) => Real (Fixed INT n s) where {\
  toRational = toRational . toDouble }; \
instance (SingI n, SingI s) => RealFrac (Fixed INT n s) where {\
  properFraction = genericProperFraction }; \
instance (SingI n, SingI s) => Floating (Fixed INT n s) where {\
    pi = fromDouble pi \
;   exp = fromDouble . exp . toDouble \
;   log = fromDouble . log . toDouble \
;   sin = fromDouble . sin . toDouble \
;   cos = fromDouble . cos . toDouble \
;   sinh = fromDouble . sinh . toDouble \
;   cosh = fromDouble . cosh . toDouble \
;   asin = fromDouble . asin . toDouble \
;   acos = fromDouble . acos . toDouble \
;   atan = fromDouble . atan . toDouble \
;   asinh = fromDouble . asinh . toDouble \
;   acosh = fromDouble . acosh . toDouble \
;   atanh = fromDouble . atanh . toDouble}; \
instance (SingI n, SingI s) => RealFloat (Fixed INT n s) where {\
   isInfinite = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  isDenormalized = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  isNegativeZero = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  isIEEE = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  isNaN = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  encodeFloat = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  decodeFloat = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  floatRange = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  floatRadix = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  floatDigits = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  exponent = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  significand = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  scaleFloat = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") \
;  atan2 = error ("RealFloat has no meaning for a fixed point number and is needed only because of Complex") };


-- RealFloat is only required because Complex a is requiring it of a.
-- It has no meaning for a fixed point 
-- Which means that some function of Complex caannot be used (like magnitude)
FIXED_INSTANCES(Int16)
FIXED_INSTANCES(Int32)
FIXED_INSTANCES(Int40)
FIXED_INSTANCES(Int64)
FIXED_INSTANCES(Int128)

FIXED_INSTANCES(Word16)
FIXED_INSTANCES(Word32)
FIXED_INSTANCES(Word40)
FIXED_INSTANCES(Word64)

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

genericDiv :: (SingI n, SingI s, NumberInfo (SuperInt a), NumberInfo a, RawValue a, Num (BaseValue a), Integral a, Bits (SuperInt a),Integral (SuperInt a), Num (SuperInt a)) => Fixed a n s -> Fixed a n s -> Fixed a n s 
genericDiv f@(Fixed a) (Fixed b) =         
        let la = (fromIntegral a)
            lb = (fromIntegral b)
            (q,_) = (la `shiftL` (nbFractionalBits f)) `quotRem` lb
            result = q
        in
        Fixed (saturateWithMode f $ result)

genericFromRational r = 
        let n = numerator r 
            d = denominator r 
        in 
        fromIntegral n / fromIntegral d

class FixedPoint a where
    fromRawValue :: a -> Fixed a n s 
    toRawValue :: Fixed a n s -> a

genericFromDouble :: (SingI n, SingI sa, Integral (SuperInt a), FixedPoint a, Num a, Num (BaseValue a), Integral a, NumberInfo (SuperInt a), NumberInfo a, RawValue a) 
                  => Double 
                  -> Fixed a n sa 
genericFromDouble a = let la = saturateWithMode ra (floor (a * 2**(fromIntegral $ nbFractionalBits ra)))
                          ra = fromRawValue la
                      in 
                      ra

genericToDouble :: (SingI n, Integral a) => Fixed a n s -> Double 
genericToDouble f@(Fixed a) = fromIntegral a * 2**(- fromIntegral (nbFractionalBits f))


