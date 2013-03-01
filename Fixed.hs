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
    , Resolution(..)
    , withSaturation 
    , withoutSaturation
    , withRounding
    , withNoRounding
    , Fixed
    , Saturation(..)
    , Rounding(..)
    , Int16
    , Int32 
    , Int40
    , Word16 
    , Word32
    , Word40
    , Int128
    , Conversion(..)
    , amulc
    , roundf
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
import Control.DeepSeq
import System.Random

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

newtype Fixed :: * -> Nat -> Saturation -> Rounding -> * where Fixed :: a -> Fixed a n sa r

data Saturation = Sat | Unsat deriving(Eq)
data Rounding = RO | NR  deriving(Eq)

newtype instance Sing (n :: Saturation) = SatC Saturation
newtype instance Sing (n :: Rounding) = RoundC Rounding

instance SingI Sat where 
  sing = SatC Sat

instance SingI Unsat where 
  sing = SatC Unsat

instance SingE (Kind :: Saturation) Saturation where
  fromSing (SatC n) = n

instance SingI RO where 
  sing = RoundC RO

instance SingI NR where 
  sing = RoundC NR

instance SingE (Kind :: Rounding) Rounding where
  fromSing (RoundC n) = n

newtype instance U.MVector s (Fixed a n sat r)  = MVFixed (U.MVector s a)
newtype instance U.Vector    (Fixed a n sat r) = VFixed (U.Vector a)

instance (U.Unbox a) => M.MVector U.MVector (Fixed a n s r) where
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

instance (U.Unbox a) => G.Vector U.Vector (Fixed a n s r) where
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

instance (RealFloat a, U.Unbox a) => U.Unbox (Fixed a n sat r)

{-# INLINE getFract #-}
getFract :: Fixed a (n :: Nat) sa r -> Sing n -> Integer 
getFract _ s = fromSing s 

{-# INLINE nbFractionalBits #-}
nbFractionalBits :: SingI n => Fixed a (n :: Nat) sa r -> Int 
nbFractionalBits f = fromIntegral $ getFract f sing

{-# INLINE getSaturationMode #-}
getSaturationMode :: Fixed a n sa r -> Sing sa -> Saturation 
getSaturationMode _ s = fromSing s 

{-# INLINE saturationMode #-}
saturationMode :: SingI sa => Fixed a n sa r -> Saturation 
saturationMode f = getSaturationMode f sing

{-# INLINE getRoundingMode #-}
getRoundingMode :: Fixed a n s (r :: Rounding) -> Sing r -> Rounding 
getRoundingMode _ s = fromSing s 

{-# INLINE roundingMode #-}
roundingMode :: SingI r => Fixed a n sa r -> Rounding
roundingMode f = getRoundingMode f sing

{-# INLINE saturateWithMode #-}
saturateWithMode :: (SingI sa, SaturateConstraint a)
                 => Fixed a n sa r
                 -> SuperInt a 
                 -> a 
saturateWithMode f su | saturationMode f == Unsat = fromIntegral su 
                      | otherwise = saturate (witness f) su
  where 
    witness (Fixed a) = a

{-# INLINE roundWithMode #-}
roundWithMode :: (SingI r, Bits b, Ord b, Num b)
              => Fixed a n sa r 
              -> Int 
              -> Int
              -> b
              -> b 
roundWithMode f na nb su = rounding (roundingMode f) na nb su
  
{-# INLINE roundf #-}
roundf :: (SingI n, SingI r, SingI sa, SaturateConstraint a, Bits (SuperInt a)) 
       => Fixed a n sa r 
       -> Fixed a n sa r 
roundf f@(Fixed a) = 
  let na = nbFractionalBits f 
  in 
  Fixed $ saturateWithMode f (roundWithMode f na 0 (fromIntegral a))

{-# INLINE rounding #-}
rounding :: (Bits a, Num a, Ord a) 
         => Rounding 
         -> Int -- ^ Current fractional bits 
         -> Int -- ^ Future fractional bits 
         -> a 
         -> a 
rounding r cf ff v | cf <= ff = v
                   | otherwise = 
              case r of 
                RO -> nearR  
                NR -> v
  where 
    rpos = cf - ff
    roundingMask = complement ((1 `shiftL` rpos) - 1)
    nearR = (v + (1 `shiftL` (rpos - 1))) .&. roundingMask --  near
   

instance Eq a => Eq (Fixed a n sa r) where
    (Fixed a) == (Fixed b) = a == b

instance Ord a => Ord (Fixed a n sa r) where 
    compare (Fixed a) (Fixed b) = compare a b

type AMulConstraint a b = (Num (BaseValue b), Bits b, Integral b, Integral (SuperInt b), Num (SuperInt b), Integral a, NumberInfo (SuperInt b), NumberInfo a, NumberInfo b, RawValue b, Num b) 

{-# INLINE amul #-}
amul :: (SingI na, SingI nb, SingI s, AMulConstraint a b)
     => Fixed a na s r 
     -> Fixed a nb s r 
     -> Fixed b (na + nb) s r
amul fa@(Fixed a) (Fixed b) = 
        let la = fromIntegral a
            lb = fromIntegral b
            r = (la * lb)
            result = Fixed (saturateWithMode result r)
        in
        result

{-# INLINE amulc #-}
amulc :: (SingI na,SingI nb, SingI s, AMulConstraint a b,Num (Fixed b (na+nb) s r)) 
      => Complex (Fixed a na s r) 
      -> Complex (Fixed a nb s r) 
      -> Complex (Fixed b (na+nb) s r)
amulc (xr :+ xi) (yr :+ yi) = (amul xr yr - amul xi yi) :+ (amul xr yi + amul xi yr)

class Conversion a b where 
  {-# INLINE convert #-}
  convert :: (SingI na, SingI nb, SingI sb, SingI r, Integral a, Bits a, Bits (SuperInt b), NumberInfo a, NumberInfo b, SaturateConstraint b) 
          => Fixed a na sa r
          -> Fixed b nb sb r
  convert fa@(Fixed a) = fb
     where 
       fb = Fixed b
       b | la <= lb = saturateWithMode fb (fromIntegral a `shift` shiftValue)
         | otherwise = saturateWithMode fb (fromIntegral ((roundWithMode fa la lb a) `shift` shiftValue))
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

{-# INLINE genericOperator #-}
genericOperator :: (SingI s, Bits a, Integral (SuperInt a), Num (BaseValue a), Num (SuperInt a), Integral a, NumberInfo (SuperInt a), NumberInfo a, RawValue a)
                => (SuperInt a -> SuperInt a -> SuperInt a) 
                -> Fixed a n s r
                -> Fixed a n s r
                -> Fixed a n s r
genericOperator op f@(Fixed a) (Fixed b) =         
        let la = (fromIntegral a)
            lb = (fromIntegral b)
        in
        Fixed (saturateWithMode f $ op la lb)

{-# INLINE genericMulOperator #-}
genericMulOperator :: (SingI s, SingI n, SingI r, Bits a, Bits (SuperInt a), Integral (SuperInt a), Num (BaseValue a), Num (SuperInt a), Integral a, NumberInfo (SuperInt a), NumberInfo a, RawValue a)
                   => Fixed a n s r
                   -> Fixed a n s r
                   -> Fixed a n s r
genericMulOperator f@(Fixed a) (Fixed b) =         
        let la = (fromIntegral a)
            lb = (fromIntegral b)
            na = (nbFractionalBits f)
            r = (roundWithMode f (na + na) na (la * lb)) `shiftR` (nbFractionalBits f)
        in
        Fixed (saturateWithMode f $ r)

{-# INLINE genericAbs #-}
genericAbs (Fixed a) | a == minBound = Fixed maxBound 
                     | otherwise = Fixed (abs a)

{-# INLINE genericFromInteger #-}
genericFromInteger a = r
     where 
        b = a `shiftL` (nbFractionalBits r) 
        theMax = maxBound 
        theMin = minBound
        r | b > fromIntegral theMax = Fixed theMax 
          | b < fromIntegral theMin = Fixed theMin 
          | otherwise = Fixed (fromIntegral b)

{-# INLINE genericProperFraction #-}
genericProperFraction f@(Fixed a) = 
  let l = nbFractionalBits f 
      b = a `shiftR` l 
      na = f - fromIntegral b
  in 
  (fromIntegral b,na)

{-
 
For Random instance

-}       

{-# INLINE genericRandomR #-}
genericRandomR :: (RandomGen g, Random a, FixedPoint a)
               => (Fixed a n s r, Fixed a n s r) 
               -> g 
               -> (Fixed a n s r, g) 
genericRandomR (Fixed mi,Fixed ma) g = 
  let (na,ng) = randomR (mi,ma) g 
  in 
  (fromRawValue na,ng)

{-# INLINE genericRandom #-}
genericRandom :: (Random (Fixed a n s r),Bounded (Fixed a n s r), RandomGen g, Random a, FixedPoint a) 
              => g 
              -> (Fixed a n s r, g)
genericRandom g = randomR (minBound, maxBound) g

class Resolution a where 
  smallestValue :: a -> a
  maxValue :: a -> a 
  minValue :: a -> a
  signedFormat :: a -> Bool
  bitWidth :: a -> Int


#define FIXED_INSTANCES(INT) \
instance NFData (Fixed INT n s r) where {\
    rnf (Fixed a) = rnf a };\
instance (SingI n, SingI s, SingI r) => Random (Fixed INT n s r) where {\
    randomR = genericRandomR \
;   random = genericRandom }; \
instance (SingI n, SingI s, SingI r) => Num (Fixed INT n s r) where {\
     (+) = genericOperator (+) \
;    (-) = genericOperator (-) \
;    abs = genericAbs \
;    (*) = genericMulOperator \
;    signum (Fixed a) = Fixed (signum a) \
;    fromInteger = genericFromInteger}; \
instance (SingI n, SingI s, SingI r) => Bounded (Fixed INT n s r) where {\
     maxBound = fromRawValue maxBound \
;    minBound = fromRawValue minBound };\
instance (SingI n, SingI s, SingI r) => Resolution (Fixed INT n s r) where {\
     smallestValue _ = fromRawValue 1 \
;    maxValue _ = maxBound \
;    minValue _ = minBound \
;    signedFormat a = signed (toRawValue a)\
;    bitWidth (Fixed a) = nbBits a};\
instance FixedPoint INT where {\
     fromRawValue = Fixed \
;    toRawValue (Fixed a) = a}; \
instance (SingI n, SingI s) => HasDoubleRepresentation (Fixed INT n s r) where {\
   toDouble = genericToDouble \
;  fromDouble = genericFromDouble}; \
instance (SingI n, SingI s, SingI r) => Fractional (Fixed INT n s r) where {\
    (/)  = genericDiv \
;   fromRational = genericFromRational}; \
instance (SingI n,SingI s, SingI r) => Real (Fixed INT n s r) where {\
  toRational = toRational . toDouble }; \
instance (SingI n, SingI s, SingI r) => RealFrac (Fixed INT n s r) where {\
  properFraction = genericProperFraction }; \
instance (SingI n, SingI s, SingI r) => Floating (Fixed INT n s r) where {\
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
instance (SingI n, SingI s, SingI r) => RealFloat (Fixed INT n s r) where {\
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

instance (SingI n, Integral a) => Show (Fixed a n s r) where 
    show f@(Fixed a) = 
        let fp = nbFractionalBits f 
            fr = 2**(- fromIntegral fp) :: Double
        in 
        printf ("%f") ((fromIntegral a)*fr)

{-# INLINE withSaturation #-}
withSaturation :: Fixed n s sa r -> Fixed n s Sat r
withSaturation (Fixed a) = Fixed a 

{-# INLINE withoutSaturation #-}
withoutSaturation :: Fixed n s sa r -> Fixed n s Unsat r
withoutSaturation (Fixed a) = Fixed a 

{-# INLINE withRounding #-}
withRounding :: Fixed n s sa r -> Fixed n s sa RO
withRounding (Fixed a) = Fixed a 

{-# INLINE withNoRounding #-}
withNoRounding :: Fixed n s sa r -> Fixed n s sa NR
withNoRounding (Fixed a) = Fixed a 

{-# INLINE genericDiv #-}
genericDiv :: (SingI n, SingI s, Bits a, NumberInfo (SuperInt a), NumberInfo a, RawValue a, Num (BaseValue a), Integral a, Bits (SuperInt a),Integral (SuperInt a), Num (SuperInt a)) 
           => Fixed a n s r 
           -> Fixed a n s r 
           -> Fixed a n s r 
genericDiv f@(Fixed a) (Fixed b) =         
        let la = (fromIntegral a)
            lb = (fromIntegral b)
            (q,_) = (la `shiftL` (nbFractionalBits f)) `quotRem` lb
            result = q
        in
        Fixed (saturateWithMode f $ result)

{-# INLINE genericFromRational #-}
genericFromRational r = 
        let n = numerator r 
            d = denominator r 
        in 
        fromIntegral n / fromIntegral d

class FixedPoint a where
    fromRawValue :: a -> Fixed a n s r 
    toRawValue :: Fixed a n s r -> a

{-# INLINE genericFromDouble #-}
genericFromDouble :: (SingI n, SingI sa, Bits a, Integral (SuperInt a), FixedPoint a, Num a, Num (BaseValue a), Integral a, NumberInfo (SuperInt a), NumberInfo a, RawValue a) 
                  => Double 
                  -> Fixed a n sa r
genericFromDouble a = let la = saturateWithMode ra (floor (a * 2**(fromIntegral $ nbFractionalBits ra)))
                          ra = fromRawValue la
                      in 
                      ra

{-# INLINE genericToDouble #-}
genericToDouble :: (SingI n, Integral a) => Fixed a n s r -> Double 
genericToDouble f@(Fixed a) = fromIntegral a * 2**(- fromIntegral (nbFractionalBits f))


