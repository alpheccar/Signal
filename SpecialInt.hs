{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module SpecialInt(
      SuperInt(..)
    , SpecialInt(..)
    , Int40(..)
    ) where 

import Data.Int
import Data.Word
import Data.Bits

type family SuperInt a
type family BaseValue a

type instance SuperInt Int8  = Int16
type instance SuperInt Int16 = Int32
type instance SuperInt Int32 = Int64
type instance SuperInt Int64 = Integer

type instance SuperInt Word8 = Word16
type instance SuperInt Word16 = Word32
type instance SuperInt Word32 = Word64

class SpecialInt a where 
    nbBits :: a -> Int 
    signed :: a -> Bool
    fromSuper :: SuperInt a -> a


class RawValue a where 
    baseValue :: a -> BaseValue a 
    -- FF ... for unsigned, 7F... for signed
    valueMask :: a -> BaseValue a
    -- Register mask : FFF ...
    registerMask :: a -> BaseValue a
    fromBaseValue :: BaseValue a -> a


{-

Function for Bounded implementation

-}
typeWitness :: a -> a 
typeWitness _ = undefined 

genericMaxBound :: (SpecialInt a, Bits (SuperInt a), Num (SuperInt a)) => a
genericMaxBound | s = rs
                | otherwise = ru
        where rs = fromSuper $ (1 `shiftL` (nbBits w - 1)) - 1
              ru = fromSuper $ (1 `shiftL` nbBits w) - 1
              w = typeWitness rs 
              s = signed w

genericMinBound :: (SpecialInt a, Bits (SuperInt a), Num (SuperInt a)) => a
genericMinBound | s = rs
                | otherwise = ru
        where rs = fromSuper (1 `shiftL` (nbBits w - 1)) 
              ru = fromSuper 0 
              w = typeWitness rs -- type witness
              s = signed w

genericMask :: (SpecialInt a, Bits (BaseValue a), Num (BaseValue a)) => a -> BaseValue a 
genericMask a | s = rs 
              | otherwise = ru 
      where rs = (1 `shiftL` (nbBits a - 1)) - 1
            ru = (1 `shiftL` nbBits a) - 1
            s = signed a

genericRegisterMask a = (1 `shiftL` nbBits a) - 1

{-

Functions for Enum

-}

genericSucc  i | i == maxBound = error "Can't increase the max bound" 
               | otherwise = fromBaseValue (succ (baseValue i)) 
genericPred i  | i == minBound = error "Can't decrease the min bound" 
               | otherwise = fromBaseValue (pred (baseValue i)) 
genericToEnum a = r 
    where 
        r = fromBaseValue $ (toEnum a) .&. valueMask r 

{-
 
Functions for Num

-}

genericFromInteger a | a < 0 = rn
                     | otherwise = rp
        where rp = fromBaseValue $ (fromInteger a) .&. valueMask rp
              rn = fromBaseValue $ (-(fromInteger (-a) .&. valueMask rn))

{-

Integral instance

-}

genericQuotRem :: (RawValue a, Integral (BaseValue a)) => a -> a -> (a,a)
genericQuotRem ia ib = 
    let (q,r) = quotRem (baseValue ia) (baseValue ib) 
    in 
    (fromBaseValue q, fromBaseValue r)

genericToInteger :: (RawValue a, Integral (BaseValue a)) => a -> Integer 
genericToInteger a = toInteger (baseValue a)

{-
    
For Ord instance

-}

signExtend :: (SpecialInt a, Integral (BaseValue a), Num (BaseValue a), Bits (BaseValue a), RawValue a) 
           => a -> BaseValue a
signExtend a | s = 
                 if (baseValue a .&. (1 `shiftL` (nbBits a - 1))) /= 0  
                    then (-1 `xor` registerMask a) .|. (fromIntegral (baseValue a))
                    else fromIntegral (baseValue a)
             | otherwise = fromIntegral (baseValue a)
    where 
        s = signed a

genericCompare :: (SpecialInt a, RawValue a, Integral (BaseValue a),Bits (BaseValue a),Ord (BaseValue a), Num (BaseValue a)) 
               => a 
               -> a 
               -> Ordering 
genericCompare ia ib = la `compare` lb
    where 
        la = signExtend ia 
        lb = signExtend ib
 
{-
 
For Show instance

-}       


{-

Instance definitions

-}

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


#define INT_INSTANCES(NB,SIGNED, INT,BASEVALUE,SUPERVALUE) \
newtype INT = INT {from##INT :: BASEVALUE}; \
type instance SuperInt INT = SUPERVALUE; \
type instance BaseValue INT = BASEVALUE; \
instance SpecialInt INT where {\
    nbBits _ = NB \
;   signed _ = SIGNED \
;   fromSuper = INT . fromIntegral }; \
instance RawValue INT where {\
    baseValue = from##INT \
;   valueMask = genericMask \
;   registerMask = genericRegisterMask \
;   fromBaseValue = INT }; \
instance Bounded INT where { \
    maxBound = genericMaxBound \
;   minBound = genericMinBound }; \
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
     toRational a = toRational (baseValue a)};\
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
{-

List of instances

-}
INT_INSTANCES(40,True,Int40,Int64,Integer)
INT_INSTANCES(128,True,Int128,Integer,Integer)
INT_INSTANCES(40,False, Word40,Word64,Word64)



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

