{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Signal.Internal(
 Signal(..)
 , BSignal(..)
 , duration
 , nbSamples
 , Time(..)
 , Frequency(..)
 , Dual(..)
 , DualVal(..)
 , samplingRate
 , constSignal
 , cs
 , Sampled(..)
 , HasPeriod(..)
 , HasSamples(..)
 ) where

import Prelude hiding(map,take,length,repeat,zipWith)
import Data.List
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed((!),Unbox(..))
import Control.Applicative

newtype Time = Time {getT :: Double} deriving(Eq,Ord,Show,Floating,Fractional,Num,Real,RealFloat,RealFrac,Read)
newtype Frequency = Frequency {getF :: Double} deriving(Eq,Ord,Show,Floating,Fractional,Num,Real,RealFloat,RealFrac,Read)

type family Dual a 

type instance Dual Time = Frequency 
type instance Dual Frequency = Time

class DualVal t where 
        dual :: t -> Dual t
        number :: Dual t -> t -> Int

instance DualVal Frequency where 
        dual t = Time (1.0 / getF t)
        number (Time t) (Frequency f) = floor (t*f)

instance DualVal Time where 
        dual t = Frequency (1.0 / getT t)
        number (Frequency f) (Time t) = floor (t*f)

duration :: (Num t, Unbox a) => t -> BSignal a -> t 
duration period bs = fromIntegral (U.length . getSamplesBS $  bs) * period

samplingRate :: (DualVal t) => t -> Dual t 
samplingRate = dual

class HasSamples m t where 
        nbSamples :: (DualVal t) => m -> t -> Int

instance HasSamples (Sampled t a) t where 
        nbSamples s d = number (rate s) d

--nbSamples :: (Unbox a) => BSignal a -> Int 
--nbSamples bs@(BSignal s) = U.length s

newtype Signal a = Signal { getSamples :: [a] } deriving(Functor)

newtype BSignal a = BSignal { getSamplesBS :: U.Vector a } 

data Sampled t a = Sampled { samplingPeriod :: !t 
                           , getSignal :: !(Signal a)
                           } 

class HasPeriod s where 
        period :: s t a -> t
        rate :: DualVal t => s t a -> Dual t

instance HasPeriod Sampled where 
        period s = samplingPeriod s
        rate s = dual . samplingPeriod $ s

instance Functor (Sampled t) where 
   fmap f (Sampled t s) = Sampled t (fmap f s)

--instance Functor (Signal t) where 
--	fmap f s = ss (map f) s
--

instance Applicative Signal where 
        pure a = Signal (repeat a)
        (Signal f) <*> (Signal l) = Signal $ (zipWith ($) f l)

constSignal :: a -> Signal a 
constSignal a = Signal (repeat a)

cs = constSignal

