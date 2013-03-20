{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Filter(
      FIR(..)
    ) where 

import Signal
import Math.Polynomial 
import Common 
import Noise 
import Fixed 
import GHC.TypeLits
import SpecialInt
import Data.Bits

data FIR p i o where 
 FIR :: (SingI (nc + ni), SingI nc, SingI ng, SingI ni, SingI no, SingI s, SingI r, HasDoubleRepresentation (Fixed a nc s r), Num (Fixed ag (nc + ni) s r), Num (Fixed a nc s r), Eq a, Conversion ag ao, ConvertConstraint ag ao, AMulConstraint a ag)
     => Poly (Fixed a nc s r) 
     -> FIR ((Fixed ag ng s r),(Fixed a nc s r)) (Fixed a ni s r) (Fixed ao no s r)
 FIRD :: Poly Double 
      -> FIR Double Double Double

delay :: s -> Signal s -> Signal s 
delay a s = consS a s 

guardType :: FIR ((Fixed ag ng s r),(Fixed a nc s r)) i (Fixed ao no s r) 
          -> Fixed ag n s r 
          -> Fixed ag n s r 
guardType _ = id

filterDWith :: [Double] 
            -> Signal Double 
            -> Signal Double 
filterDWith (a:l) s | null l = mapS (a*) s
                    | otherwise = zipWithS (+) (mapS (a*) s) (filterDWith l (delay 0 s)) 
filterDWith [] s = s 


_filterWith :: (AMulConstraint a ag, SingI (na + ni), SingI s, SingI r, SingI na, SingI ni, Num (Fixed a ni s r), Num (Fixed ag (na + ni) s r))
            => FIR ((Fixed ag ng s r),(Fixed a na s r)) (Fixed a ni s r) (Fixed ao no s r)
            -> [Fixed a na s r] 
            -> Signal (Fixed a ni s r) 
            -> Signal (Fixed ag (na + ni) s r) 
_filterWith f (a:l) s | null l = mapS (amul a) s
                      | otherwise = zipWithS (+) (mapS (amul a) s) (_filterWith f l (delay 0 s)) 
_filterWith _ [] s = error "Empty polynomial can't be used for filtering"

filterWith :: (Conversion ag ao, ConvertConstraint ag ao, Num (Fixed a ni s r), Num (Fixed ag (na + ni) s r), SingI r, SingI s, SingI (na + ni), SingI ni, SingI na, SingI no, AMulConstraint a ag) 
           => FIR ((Fixed ag ng s r),(Fixed a na s r)) (Fixed a ni s r) (Fixed ao no s r) 
           -> [Fixed a na s r] 
           -> Signal (Fixed a ni s r) 
           -> Signal (Fixed ao no s r)
filterWith f l = mapS convert . _filterWith f l

instance Structure FIR where 
    doubleVersion (FIR p) = FIRD $ fmap toDouble p 
    transferFunction f@(FIR p) s = 
        let c = polyCoeffs LE $ p 
        in 
        filterWith f c s 
    transferFunction (FIRD p) s = 
        let c = polyCoeffs LE p 
        in 
        filterDWith c s 