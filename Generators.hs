{-# LANGUAGE BangPatterns #-}
module Generators(
	  uniformSamples
	, randomSamples
	) where 

import Prelude hiding(unfoldr)
import Common
import Internal
import System.Random 
import Data.List.Stream

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