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
               => a 
               -> a
               -> Signal a a 
uniformSamples sampling start = Signal sampling (unif start)
 where 
   unif !s = s : unif (s + sampling)


randomSamples :: (Random a) 
              => t 
              -> a
              -> a 
              -> IO (Signal t a)
randomSamples r mi ma = do 
	g <- newStdGen 
	let l = randomRs (mi,ma) g
	return $ Signal r $ l