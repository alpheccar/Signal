{-# LANGUAGE BangPatterns #-}
module Signal.Generators(
          uniformSamples
        , randomSamples
        ) where

import Prelude hiding(unfoldr)
import System.Random
import Signal.Common
import Signal.Internal

uniformSamples :: Num a  
               => a -- ^ Sampling period 
               -> a -- Starting value
               -> Signal a 
uniformSamples sampling start = Signal (unif start)
 where 
   unif !s = s : unif (s + sampling)


randomSamples :: (Random a) 
              => a
              -> a 
              -> IO (Signal a)
randomSamples mi ma = do 
        g <- newStdGen
        let l = randomRs (mi,ma) g
        return $ Signal l
