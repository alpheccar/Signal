{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Internal(
	  Signal(..)
	, BSignal(..)
	, ss 
	, bb 
	, sb 
	, bs 
	, so 
	, bo
	, os
	, IsSignal(..)
	, duration 
	, nbSamples
	, Time(..)
	, Frequency(..)
	, Dual(..)
	, DualVal(..)
	, samplingRate
	) where 

import Prelude hiding(map,take,length)
import Data.List.Stream
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed((!),Unbox(..))

newtype Time = Time {getT :: Double} deriving(Eq,Ord,Show,Floating,Fractional,Num,Real,RealFloat,RealFrac,Read)
newtype Frequency = Frequency {getF :: Double} deriving(Eq,Ord,Show,Floating,Fractional,Num,Real,RealFloat,RealFrac,Read)

type family Dual a 

type instance Dual Time = Frequency 
type instance Dual Frequency = Time

class DualVal t where 
	dual :: t -> Dual t 

instance DualVal Frequency where 
	dual t = Time (1.0 / getF t)

instance DualVal Time where 
	dual t = Frequency (1.0 / getT t)

class IsSignal s where 
	samplingPeriod :: s t a -> t 

instance IsSignal Signal where 
	samplingPeriod = periodS 

instance IsSignal BSignal where 
	samplingPeriod = periodBS 

duration :: (Num t, Unbox a) => BSignal t a -> t 
duration bs = fromIntegral (nbSamples bs) * (samplingPeriod bs)

samplingRate :: (DualVal t, IsSignal s) => s t a -> Dual t 
samplingRate bs = dual (samplingPeriod bs)

nbSamples :: (Unbox a) => BSignal t a -> Int 
nbSamples bs@(BSignal _ s) = U.length s

data Signal t a = Signal { periodS :: !t
                         , getSamples :: ![a]
                         }

data BSignal t a = BSignal { periodBS :: !t
                           , getSamplesBS :: !(U.Vector a)
                           }

instance Functor (Signal t) where 
	fmap f s = ss (map f) s

ss :: ([a] -> [b]) -> Signal t a -> Signal t b
ss f (Signal r s) = Signal r (f s)
{-# INLINE [0] ss #-}

sb :: Unbox b => ([a] -> U.Vector b) -> Signal t a -> BSignal t b
sb f (Signal r s) = BSignal r (f s)
{-# INLINE [0] sb #-}

bs :: Unbox a => (U.Vector a -> [b]) -> BSignal t a -> Signal t b
bs f (BSignal r s) = Signal r (f s)
{-# INLINE [0] bs #-}

bb :: (Unbox a, Unbox b) => (U.Vector a -> U.Vector b) -> BSignal t a -> BSignal t b
bb f (BSignal r s) = BSignal r (f s)
{-# INLINE [0] bb #-}

so :: ([a] -> b) -> Signal t a -> b
so f (Signal r s) = f s
{-# INLINE [0] so #-}

bo :: Unbox a => (U.Vector a -> b) -> BSignal t a -> b
bo f (BSignal r s) = f s
{-# INLINE [0] bo #-}

os :: t -> [a] -> Signal t a 
os r l = Signal r l 
{-# INLINE [0] os #-}

--
-- a convenient rule for map
--
{-# RULES
    "STREAM ss/ss fusion" forall f g s.
        ss f (ss g s) = ss (\x -> f (g x)) s
 #-}

{-# RULES
    "STREAM bb/bb fusion" forall f g s.
        bb f (bb g s) = bb (\x -> f (g x)) s
 #-}

{-# RULES
    "STREAM sb/ss fusion" forall f g s.
        sb f (ss g s) = sb (\x -> f (g x)) s
 #-}

{-# RULES
    "STREAM bb/sb fusion" forall f g s.
        bb f (sb g s) = sb (\x -> f (g x)) s
 #-}

{-# RULES
    "STREAM bs/bb fusion" forall f g s.
        bs f (bb g s) = bs (\x -> f (g x)) s
 #-}

{-# RULES
    "STREAM ss/bs fusion" forall f g s.
        ss f (bs g s) = bs (\x -> f (g x)) s
 #-}


{-# RULES
    "STREAM so/ss fusion" forall f g s.
        so f (ss g s) = so (\x -> f (g x)) s
 #-}

{-# RULES
    "STREAM bo/bb fusion" forall f g s.
        bo f (bb g s) = bo (\x -> f (g x)) s
 #-}

{-# RULES
    "STREAM ss/os fusion" forall f g r.
        ss f (os r g) = os r (f g)
 #-}
