{-# LANGUAGE BangPatterns #-}
module Generators(
	  uniformSamples
	, randomSamples
	) where 

import Common
import Internal
import Data.Stream(stream,unstream,Stream(..),Step(..),L(..))
import System.Random 
import Data.List.Stream

uniformSamples :: Num a 
               => a -- ^ Period
               -> a -- Start
               -> Signal a a
uniformSamples sampling start = Signal sampling $ unstream (uniformS sampling start)
 where 
 	uniformS sampling start = Stream (nextS sampling) (L start) 
 	nextS sampling (L !s) = Yield s (L (s + sampling))


randomSamples :: (Random a) 
              => t 
              -> a
              -> a 
              -> IO (Signal t a)
randomSamples r mi ma = do 
	g <- newStdGen 
	let l = randomRs (mi,ma) g
	return $ Signal r $ l