{-# LANGUAGE ConstraintKinds #-}
module VAD(
      vad
    ) where 

--  let framed = frameWithWinAndOverlap 256 hann 10 s
--      sp = mapS (fft . U.map (:+ 0)) framed 
--

import Prelude hiding(splitAt,(:),foldl1')
import Internal
import Signal 
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed((!),Unbox(..))
import Data.List.Stream
import Common 
import Windows
import Data.Complex 
import Transform

ltseF :: Int 
      -> Signal t (U.Vector Double) 
      -> Signal t (U.Vector Double) 
ltseF n (Signal r s) = 
    let (before,remaining) = splitAt n s
        _lste b r = 
            let (future,tl) = splitAt n r 
                valueMax = U.zipWith max (foldl1' (U.zipWith max) future)  (foldl1' (U.zipWith max) b)  
            in 
            valueMax:(_lste future tl)
    in 
    Signal r (_lste before remaining)

lstd :: Int 
     -> U.Vector Double 
     -> U.Vector Double
     -> Double 
lstd winSize noiseEnergy lste = 
    10 * log(U.sum (U.zipWith (/) lste noiseEnergy) / fromIntegral winSize) / log 10

getDecision :: Int 
            -> U.Vector Double 
            -> [(U.Vector Double, U.Vector Double)]
            -> [Double] 
getDecision winSize energy ((c,currentE):r) =  
    let t = 30.0
        l = lstd winSize energy c 
    in 
    if l > t 
        then 1.0 : getDecision winSize energy r
        else 0.0 : getDecision winSize currentE r
getDecision winSize energy [] = 0:getDecision winSize energy []

vad :: Signal Time Double 
    -> Signal Time Double
vad s = 
    let winSize = 256 
        overlap = 20
        n = 1
        framed = frameWithWinAndOverlap winSize overlap hann s
        bandEnergy (x :+ y) = x*x+y*y
        energy = mapS (U.map bandEnergy . fft . U.map (:+ 0)) framed
        noiseEnergy0 = U.generate winSize (const 0.00001)
        lt = ltseF n energy 
    in 
    --mapS (lstd winSize noiseEnergy0) lt
    onSamples (getDecision winSize noiseEnergy0) (zipS lt (dropS n energy))