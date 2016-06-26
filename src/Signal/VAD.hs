{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Signal.VAD(
      vad
    ) where 

--  let framed = frameWithWinAndOverlap 256 hann 10 s
--      sp = mapS (fft . U.map (:+ 0)) framed 
--

import Prelude hiding(splitAt,(:),foldr1,tail,(++))
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed((!),Unbox(..))
import Data.List.Stream(tail, foldr1, foldl1', splitAt, (++))
import Data.Complex

import GHC.TypeLits
import Data.Int

import Signal
import Signal.Common
import Signal.Fixed
import Signal.Internal
import Signal.SpecialInt
import qualified Signal.Trace as F
import Signal.Transform
import Signal.Windows
import Data.Singletons

ltseF :: Sample a  
      => Int 
      -> Signal (U.Vector a) 
      -> Signal (U.Vector a) 
ltseF n (Signal s) = 
    let (before,remaining) = splitAt n s
        _lste b (h:t) = 
            let (future,tl) = splitAt n t 
                valueMax = U.zipWith max (foldr1 (U.zipWith max) (h:future))  (foldl1' (U.zipWith max) b)  
            in 
            valueMax:(_lste (tail before ++ [h]) t)
    in 
    Signal (_lste before remaining)

lstdD :: Int 
      -> U.Vector Double 
      -> U.Vector Double
      -> Double
lstdD winSize noiseEnergy lste = 
        let d a b | b == 0 = fromDouble 1e12 
                  | otherwise = a / b 
        in
        10 * log(U.sum (U.zipWith (d) lste noiseEnergy) / fromIntegral winSize) / log 10

theTotal :: (SingI r, SingI s, SingI n, KnownNat n)
         => Fixed Int32 n s r 
         -> U.Vector (Fixed Int32 n s r) 
         -> U.Vector (Fixed Int32 n s r) 
         -> Fixed Int40 n s r 
theTotal m lste noiseEnergy = 
           let  d a b | b == 0 = convert m 
                      | otherwise = convert a / convert b
           in
           U.sum (U.zipWith (d) lste noiseEnergy)
{-# INLINE [0] theTotal #-}

lstdF :: (SingI n, SingI s, SingI r, KnownNat n)
      => Int 
      -> U.Vector (Fixed Int32 n s r)
      -> U.Vector (Fixed Int32 n s r)
      -> (Fixed Int32 n s r)
lstdF winSize noiseEnergy lste = 10 * log(convert $ theTotal maxBound lste noiseEnergy/ fromIntegral winSize) / log 10

getDecisionF :: (SingI n, SingI s, SingI r, SingI (n + n), KnownNat n, KnownNat (n + n))
             => Int 
             -> U.Vector (Fixed Int32 (n + n) s r) 
             -> [(U.Vector (Fixed Int32 (n + n) s r), U.Vector (Fixed Int32 (n + n) s r) )] 
             -> [Fixed Int16 n s r]
getDecisionF winSize energy ((c,currentE):r) =  
    let tv = fromDouble 31.0
        tn = fromDouble 27.0
        l = lstdF winSize energy c 
        result | l >= tv = 1 : getDecisionF winSize energy r
               | l <= tn = 0 : getDecisionF winSize currentE r
               | otherwise = 0 : getDecisionF winSize energy r
    in 
    result
getDecisionF winSize energy [] = 0:getDecisionF winSize energy []

getDecisionD :: Int 
             -> U.Vector Double 
             -> [(U.Vector Double, U.Vector Double)]
             -> [Double]
getDecisionD winSize energy ((c,currentE):r) =  
    let tv = fromDouble 31.0
        tn = fromDouble 27.0
        l = lstdD winSize energy c 
        result | l >= tv = 1 : getDecisionD winSize energy r
               | l <= tn = 0 : getDecisionD winSize currentE r
               | otherwise = 0 : getDecisionD winSize energy r
    in 
    result
getDecisionD winSize energy [] = 0:getDecisionD winSize energy []

bandEnergy :: Complex Double -> Double
bandEnergy (x :+ y) = x*x + y*y 

bandEnergyF :: (SingI n, SingI s, SingI r, SingI (n + n), KnownNat (n + n)) => Complex (Fixed Int16 n s r) -> Fixed Int32 (n + n) s r
bandEnergyF (x :+ y) = amul x x + amul y y

class VAD a where 
    vad :: (Sample a, FFT a) 
        => Sampled Time a
        -> Sampled Time a

instance (SingI n, SingI s, SingI r, KnownNat n, SingI (n + n), KnownNat (n+n)) => VAD (Fixed Int16 n s r) where
    vad s = 
        let winSize = 256 
            overlap = 20
            n = 2
            framed = frameWithWinAndOverlap winSize overlap hann s
            energy = mapS (U.map bandEnergyF . fft . U.map (:+ 0)) (getSignal framed)
            noiseEnergy0 = U.generate winSize (const (fromDouble 0.00001))
            lt = ltseF n energy 
        in 
        --mapS (lstd winSize noiseEnergy0) lt
        Sampled (period framed) (onSamples (getDecisionF winSize noiseEnergy0) (zipS lt (dropS n energy)))  

instance VAD Double where
    vad s = 
        let winSize = 256 
            overlap = 20
            n = 2
            framed = frameWithWinAndOverlap winSize overlap hann s
            energy = mapS (U.map bandEnergy . fft . U.map (:+ 0)) (getSignal framed)
            noiseEnergy0 = U.generate winSize (const (fromDouble 0.00001))
            lt = ltseF n energy 
        in 
        --mapS (lstd winSize noiseEnergy0) lt
        Sampled (period framed) (onSamples (getDecisionD winSize noiseEnergy0) (zipS lt (dropS n energy))) 
