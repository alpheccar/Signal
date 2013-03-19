{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Filter(
      FIR(..)
    ) where 

import Signal
import Math.Polynomial 
import Common 
import Noise 

data FIR g c = FIR (Poly c) 

delay :: s -> Signal s -> Signal s 
delay a s = consS a s 

filterWith :: Num f 
           => [f] 
           -> Signal f 
           -> Signal f 
filterWith (a:l) s = zipWithS (+) (mapS (a*) s) (filterWith l (delay 0 s)) 
filterWith [] s = s 

instance Structure (FIR g) where 
    doubleVersion (FIR p) = FIR $ fmap toDouble p 
    transferFunction (FIR p) s = 
        let c = polyCoeffs LE p 
        in 
        filterWith c s 