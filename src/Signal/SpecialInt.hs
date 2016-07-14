{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Signal.SpecialInt(
      SuperInt(..)
    , NumberInfo(..)
    , BaseValue(..)
    , RawValue(..)
    , Int40
    , Int128
    , Word40
    , saturate
    , SaturateConstraint(..)
    , Signed(..)
    , Unsigned(..)
    , asSigned 
    , asUnsigned
    , signExtend
    ) where 

import Data.Int
import Data.Word
import Data.Bits
import Test.Framework(testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit.Base
import Control.DeepSeq
import System.Random
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed((!),Unbox(..))
import qualified Data.Vector.Generic.Mutable as M
import Control.Monad(liftM)
import qualified Data.Vector.Generic as G

type family SuperInt a
type family BaseValue a

type family Signed a 
type family Unsigned a 


class NumberInfo a where 
    nbBits :: a -> Int 
    signed :: a -> Bool
     -- FF ... for unsigned, 7F... for signed
    valueMask :: a -> BaseValue a
    -- Register mask : FFF ...
    registerMask :: a -> BaseValue a

class RawValue a where 
    baseValue :: a -> BaseValue a 
    fromBaseValue :: BaseValue a -> a
    fromSuper :: (Num (BaseValue a), Integral (SuperInt a)) => SuperInt a -> a
    fromSuper = fromBaseValue . fromIntegral 

{-

Misc functions

-}

{-# INLINE unsignedMask #-}
unsignedMask :: (NumberInfo a, RawValue a) => a -> a 
unsignedMask a = (fromBaseValue $ registerMask a)

{-# INLINE signedMask #-}
signedMask :: (NumberInfo a, RawValue a) => a -> a 
signedMask a = (fromBaseValue $ valueMask a)

type SaturateConstraint a = (Bits a, Num a, Integral a, Integral (SuperInt a), NumberInfo (SuperInt a), NumberInfo a,RawValue a, Num (BaseValue a))

{-# INLINE saturate #-}
saturate :: (SaturateConstraint a) 
         => a -- type witness (a cannot be infered from just SuperInt a as SuperInt is not injective)
         -> SuperInt a 
         -> a
saturate w su | s = saturateSigned su
              | otherwise = saturateUnsigned su 
 where
    s = signed w
    saturateSigned su | su > fromIntegral (signedMask w)  = signedMask w
                      | su < - (fromIntegral (signedMask w) + 1) = complement (signedMask w)
                      | otherwise = fromIntegral su 
    saturateUnsigned su | su > fromIntegral (unsignedMask w) = unsignedMask w
                        | otherwise = fromIntegral su


{-

Function for Bounded implementation

-}
typeWitness :: a -> a 
typeWitness _ = undefined 

{-# INLINE genericMaxBound #-}
genericMaxBound :: (NumberInfo a, RawValue a, Num (BaseValue a), Integral (SuperInt a), Bits (SuperInt a), Num (SuperInt a)) => a
genericMaxBound | s = rs
                | otherwise = ru
        where rs = fromSuper $ (1 `shiftL` (nbBits w - 1)) - 1
              ru = fromSuper $ (1 `shiftL` nbBits w) - 1
              w = typeWitness rs 
              s = signed w

{-# INLINE genericMinBound #-}
genericMinBound :: (NumberInfo a, RawValue a, Num (BaseValue a), Integral (SuperInt a), Bits (SuperInt a), Num (SuperInt a)) => a
genericMinBound | s = rs
                | otherwise = ru
        where rs = fromSuper (1 `shiftL` (nbBits w - 1)) 
              ru = fromSuper 0 
              w = typeWitness rs -- type witness
              s = signed w

{-# INLINE genericMask #-}
genericMask :: (NumberInfo a, Bits (BaseValue a), Num (BaseValue a)) => a -> BaseValue a 
genericMask a | s = rs 
              | otherwise = ru 
      where rs = (1 `shiftL` (nbBits a - 1)) - 1
            ru = (1 `shiftL` nbBits a) - 1
            s = signed a

{-# INLINE genericRegisterMask #-}
genericRegisterMask :: (NumberInfo a, Bits (BaseValue a), Num (BaseValue a)) 
                    => a 
                    -> BaseValue a
genericRegisterMask a = (1 `shiftL` nbBits a) - 1

{-

Functions for Bits

-}

{-# INLINE genericShift #-}
genericShift :: (Bits (BaseValue a), RawValue a, NumberInfo a, Num (BaseValue a), Integral (BaseValue a))
             => a 
             -> Int 
             -> a
genericShift ia n = fromBaseValue $ ((signExtend ia) `shift` n) .&. (registerMask ia)

{-# INLINE genericBit #-}
genericBit :: (RawValue a, Bits (BaseValue a), NumberInfo a) 
           => Int 
           -> a
genericBit n = r 
 where 
    r = fromBaseValue $ (bit n) .&. registerMask r

{-# INLINE genericRotate #-}
genericRotate :: (Ord (BaseValue a), NumberInfo a, RawValue a, Bits (BaseValue a), Num (BaseValue a), Integral (BaseValue a)) 
              => a 
              -> Int 
              -> a
x1 `genericRotate`  i | i<0 && signed x1 && x<0
                          = let left = i+nbBits x1 in
                            fromBaseValue $ (((x `shift` i) .&. complement ((-1) `shift` left))
                            .|. (x `shift` left)) .&. registerMask x1
                      | i<0  = fromBaseValue $ ((x `shift` i) .|. (x `shift` (i+nbBits x1))) .&. registerMask x1
                      | i==0 = fromBaseValue $ x
                      | i>0  = fromBaseValue $ ((x `shift` i) .|. (x `shift` (i-nbBits x1))) .&. registerMask x1
 where 
  x = baseValue x1

{-

Functions for Enum

-}

{-# INLINE genericSucc #-}
genericSucc  i | i == maxBound = error "Can't increase the max bound" 
               | otherwise = fromBaseValue (succ (signExtend i)) 

{-# INLINE genericPred #-}           
genericPred i  | i == minBound = error "Can't decrease the min bound" 
               | otherwise = fromBaseValue (pred (signExtend i)) 

{-# INLINE genericToEnum #-}
genericToEnum a = r 
    where 
        r = fromBaseValue $ (toEnum a) .&. valueMask r 

{-
 
Functions for Num

-}

{-# INLINE genericFromInteger #-}
genericFromInteger a | a < 0 = rn
                     | otherwise = rp
        where rp = fromBaseValue $ (fromInteger a) .&. valueMask rp
              rn = fromBaseValue $ (-(fromInteger (-a) .&. valueMask rn))

{-

Integral instance

-}

{-# INLINE genericQuotRem #-}
genericQuotRem :: (Bits (BaseValue a), NumberInfo a, RawValue a, Integral (BaseValue a)) 
               => a -> a -> (a,a)
genericQuotRem ia ib = 
    let (q,r) = quotRem (signExtend ia) (signExtend ib) 
    in 
    (fromBaseValue q, fromBaseValue r)

{-# INLINE genericToInteger #-}
genericToInteger :: (Bits (BaseValue a),NumberInfo a, RawValue a, Integral (BaseValue a)) 
                 => a -> Integer 
genericToInteger a = toInteger (signExtend a)

{-
    
For Ord instance

-}

{-# INLINE signExtend #-}
signExtend :: (NumberInfo a, Integral (BaseValue a), Num (BaseValue a), Bits (BaseValue a), RawValue a) 
           => a -> BaseValue a
signExtend a | s = 
                 if (baseValue a .&. (1 `shiftL` (nbBits a - 1))) /= 0  
                    then (-1 `xor` registerMask a) .|. (fromIntegral (baseValue a))
                    else fromIntegral (baseValue a)
             | otherwise = fromIntegral (baseValue a)
    where 
        s = signed a

{-# INLINE genericCompare #-}
genericCompare :: (NumberInfo a, RawValue a, Integral (BaseValue a),Bits (BaseValue a),Ord (BaseValue a), Num (BaseValue a)) 
               => a 
               -> a 
               -> Ordering 
genericCompare ia ib = la `compare` lb
    where 
        la = signExtend ia 
        lb = signExtend ib
 
{-
 
For Random instance

-}       

{-# INLINE genericRandomR #-}
genericRandomR :: (NumberInfo a, Bits (BaseValue a), Num (BaseValue a), Integral (BaseValue a), Random (BaseValue a),RawValue a, RandomGen g)
               => (a, a) 
               -> g 
               -> (a, g) 
genericRandomR (mi,ma) g = 
  let (na,ng) = randomR ((signExtend mi),(signExtend ma)) g 
  in 
  (fromBaseValue na,ng)

{-# INLINE genericRandom #-}
genericRandom :: (RandomGen g, RawValue a, Random (BaseValue a)) 
              => g 
              -> (a, g)
genericRandom g = 
  let (na,ng) = random g
  in 
  (fromBaseValue na,g)


{-

Instance definitions

-}

#define STANDARD_INT(NB,SUPER) \
type instance SuperInt Int##NB = SUPER; \
type instance BaseValue Int##NB = Int##NB; \
instance NumberInfo Int##NB where {\
    nbBits _ = NB \
;   signed _ = True \
;   valueMask = genericMask \
;   registerMask = genericRegisterMask }; \
instance RawValue Int##NB where {\
    baseValue = id \
;   fromBaseValue = id }; \

#define STANDARD_WORD(NB,SUPER) \
type instance SuperInt Word##NB = SUPER; \
type instance BaseValue Word##NB = Word##NB; \
instance NumberInfo Word##NB where {\
    nbBits _ = NB \
;   signed _ = False \
;   valueMask = genericMask \
;   registerMask = genericRegisterMask }; \
instance RawValue Word##NB where {\
    baseValue = id \
;   fromBaseValue = id }; \



#define FAKE_INSTANCES(INT) \
instance Floating INT where {\
    pi = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   exp = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   log = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   sin = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   cos = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   sinh = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   cosh = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   asin = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   acos = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   atan = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   asinh = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   acosh = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   atanh = error ("Floating instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") };\
instance Fractional INT where  {\
    (/)  = error ("Fractional instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;   fromRational = error ("Fractional instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") }; \
instance RealFrac INT where { \
  properFraction = error ("RealFrac instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") }; \
instance RealFloat INT where { \
   isInfinite = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  isDenormalized = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  isNegativeZero = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  isIEEE = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  isNaN = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  encodeFloat = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  decodeFloat = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  floatRange = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  floatRadix = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  floatDigits = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  exponent = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  significand = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  scaleFloat = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") \
;  atan2 = error ("RealFloat instance for" ++ #INT ++ "declared only because of Complex of fixed point but has no meaning") };


#define INT_INSTANCES(HASH,NB,SIGNED, INT,BASEVALUE,SUPERVALUE) \
newtype INT = INT {from##INT :: BASEVALUE}; \
type instance SuperInt INT = SUPERVALUE; \
type instance BaseValue INT = BASEVALUE; \
instance Random INT where {\
    randomR = genericRandomR \
;   random = genericRandom }; \
instance NFData INT where {\
    rnf a = rnf (baseValue a) };\
instance NumberInfo INT where {\
    nbBits _ = NB \
;   signed _ = SIGNED \
;   valueMask = genericMask \
;   registerMask = genericRegisterMask }; \
instance RawValue INT where {\
    baseValue = from##INT \
;   fromBaseValue = INT }; \
instance Bounded INT where { \
    maxBound = genericMaxBound \
;   minBound = genericMinBound }; \
instance Bits INT where { \
  ia .&. ib = fromBaseValue $ baseValue ia .&. baseValue ib \
; ia .|. ib = fromBaseValue $ baseValue ia .|. baseValue ib \
; ia `xor` ib = fromBaseValue $ baseValue ia `xor` baseValue ib \
; complement ia = fromBaseValue $ complement (baseValue ia) .&. (registerMask ia) \
; shift = genericShift \
; rotate = genericRotate \
; bitSize = nbBits \
; isSigned = signed \
; testBit ia n = testBit (baseValue ia) n \
; bit = genericBit \
; popCount ia = popCount (baseValue ia)}; \
instance Enum INT where { \
     succ  = genericSucc \
;    pred  = genericPred \
;    toEnum = genericToEnum \
;    fromEnum i = fromEnum (baseValue i) \
;    enumFromTo ia ib = map fromBaseValue (enumFromTo (baseValue ia) (baseValue ib)) \
;    enumFromThenTo ia ib ic = map fromBaseValue (enumFromThenTo (baseValue ia) (baseValue ib) (baseValue ic))};\
;instance Num INT where {\
;    ia + ib = fromBaseValue $ (baseValue ia + baseValue ib) .&. registerMask ia \
;    ia - ib = fromBaseValue $ (baseValue ia - baseValue ib) .&. registerMask ia \
;    ia * ib = fromBaseValue $ (baseValue ia * baseValue ib) .&. registerMask ia \
;    abs ia = fromBaseValue (abs (baseValue ia)) \
;    signum ia = fromBaseValue (signum (baseValue ia) .&. registerMask ia) \
;    fromInteger = genericFromInteger }; \
instance Real INT where {\
     toRational a = toRational (signExtend a)};\
instance Integral INT where {\
     quotRem = genericQuotRem \
;    toInteger = genericToInteger };\
instance Ord INT where {\
     compare = genericCompare };\
instance Eq INT where {\
     ia == ib = (baseValue ia) .&. registerMask ia == (baseValue ib) .&. registerMask ia};\
instance Show INT where {\
    show ia = show (signExtend ia)};\
FAKE_INSTANCES(INT)

#define INT_ARRAY_INSTANCES(HASH,INT,BASEVALUE) \
newtype instance U.MVector s INT = MV_##INT (U.MVector s BASEVALUE); \
newtype instance U.Vector    INT = V_##INT (U.Vector BASEVALUE); \
instance M.MVector U.MVector INT where {\
  {-HASH INLINE basicLength HASH-}                                            \
; {-HASH INLINE basicUnsafeSlice HASH-}                                       \
; {-HASH INLINE basicOverlaps HASH-}                                          \
; {-HASH INLINE basicUnsafeNew HASH-}                                         \
; {-HASH INLINE basicUnsafeRead HASH-}                                        \
; {-HASH INLINE basicUnsafeWrite HASH-}                                       \
;   basicLength (MV_##INT v) = M.basicLength v \
;   basicUnsafeSlice a b (MV_##INT v) = MV_##INT (M.basicUnsafeSlice a b v) \
;   basicOverlaps (MV_##INT a) (MV_##INT b) = M.basicOverlaps a b \
;   basicUnsafeNew n = MV_##INT `liftM` M.basicUnsafeNew n \
;   basicUnsafeRead (MV_##INT v) i = fromBaseValue `liftM` M.basicUnsafeRead v i \
;   basicUnsafeWrite (MV_##INT v) i r = M.basicUnsafeWrite v i (signExtend r)}; \
instance G.Vector U.Vector INT where {\
  {-HASH INLINE basicUnsafeFreeze HASH-}                                      \
; {-HASH INLINE basicUnsafeThaw HASH-}                                        \
; {-HASH INLINE basicLength HASH-}                                            \
; {-HASH INLINE basicUnsafeSlice HASH-}                                       \
; {-HASH INLINE basicUnsafeIndexM HASH-}                                      \
;   basicLength (V_##INT v) = G.basicLength v\
;   basicUnsafeFreeze (MV_##INT v) = V_##INT `liftM` G.basicUnsafeFreeze v\
;   basicUnsafeThaw (V_##INT v) =  MV_##INT `liftM` G.basicUnsafeThaw v\
;   basicUnsafeSlice a b (V_##INT v) = V_##INT (G.basicUnsafeSlice a b v)\
;   basicUnsafeIndexM (V_##INT v) i = fromBaseValue `liftM` G.basicUnsafeIndexM v i };\
instance Unbox INT where {};

{-

List of instances

-}

-- | Missing big instances required as SuperInt of standard or special format
-- Those instances cannot be used in fixed point because their super type (Integer)
-- has no bitwidth
INT_INSTANCES(#,256,True, Int256,Integer,Integer)
INT_INSTANCES(#,256,False, Word256,Integer,Integer)

STANDARD_INT(16,Int32)
STANDARD_INT(32,Int64)
STANDARD_INT(64,Int128)

STANDARD_WORD(16,Word32)
STANDARD_WORD(32,Word64)
STANDARD_WORD(64,Word128)

INT_INSTANCES(#,40,True,Int40,Int64,Int128)
INT_ARRAY_INSTANCES(#,Int40,Int64)

INT_INSTANCES(#,40,False, Word40,Word64,Word128)
INT_ARRAY_INSTANCES(#,Word40,Word64)

-- Integer is not supported in an unboxed vector so we can not
-- define vectors for Int128 and Word128
INT_INSTANCES(#,128,True,Int128,Integer,Int256)
INT_INSTANCES(#,128,False, Word128,Integer,Word256)

type instance Signed Word16 = Int16
type instance Signed Word32 = Int32
type instance Signed Word40 = Int40
type instance Signed Word64 = Int64
type instance Signed Word128 = Int128

type instance Signed Int16 = Int16
type instance Signed Int32 = Int32
type instance Signed Int40 = Int40
type instance Signed Int64 = Int64
type instance Signed Int128 = Int128

type instance Unsigned Int16 = Word16
type instance Unsigned Int32 = Word32
type instance Unsigned Int40 = Word40
type instance Unsigned Int64 = Word64
type instance Unsigned Int128 = Word128

type instance Unsigned Word16 = Word16
type instance Unsigned Word32 = Word32
type instance Unsigned Word40 = Word40
type instance Unsigned Word64 = Word64
type instance Unsigned Word128 = Word128

-- | Interpret a number as a signed one (no conversion of value)
asSigned :: (Num (Signed a), Integral a) => a -> Signed a 
asSigned = fromIntegral 

-- | Interpret a number as an unsigned (no conversion of value)
asUnsigned :: (Num (Unsigned a), Integral a) => a -> Unsigned a 
asUnsigned = fromIntegral


{-

Hacks because Unbox (Complex a) is requiring RealFloat a for a reason I do not yet understand

-}

-- -------
-- Complex
-- -------
FAKE_INSTANCES(Int16)
FAKE_INSTANCES(Int32)
FAKE_INSTANCES(Int64)

FAKE_INSTANCES(Word16)
FAKE_INSTANCES(Word32)
FAKE_INSTANCES(Word64)

