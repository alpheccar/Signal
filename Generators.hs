{-# LANGUAGE BangPatterns #-}
module Generators(
	uniformSamples
	) where 

import Common
import Internal
import Data.Stream(stream,unstream,Stream(..),Step(..),L(..))

uniformSamples :: Num a 
               => a -- ^ Period
               -> a -- Start
               -> Signal a
uniformSamples sampling start = Signal $ unstream (uniformS sampling start)
 where 
 	uniformS sampling start = Stream (nextS sampling) (L start) 
 	nextS sampling (L !s) = Yield s (L (s + sampling))
