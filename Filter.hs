{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Filter(
      FIR(..)
    , bode
    , phasePlot
    ) where 

import Signal
import Math.Polynomial 
import Common 
import Noise 
import Fixed 
import GHC.TypeLits
import SpecialInt
import Data.Bits
import Plot
import Data.Complex
import Text.Printf

import Debug.Trace

debug a = trace (show a) a

data FIR p i o where 
 FIR :: (SingI (nc + ni), SingI nc, SingI ni, SingI no, SingI s, SingI r, HasDoubleRepresentation (Fixed a nc s r), Num (Fixed ag (nc + ni) s r), Num (Fixed a nc s r), Eq a, Conversion ag ao, ConvertConstraint ag ao, AMulConstraint a ag)
     => Poly (Fixed a nc s r) 
     -> FIR ((Fixed ag (nc + ni) s r),(Fixed a nc s r)) (Fixed a ni s r) (Fixed ao no s r)
 FIRD :: Poly Double 
      -> FIR Double Double Double

polyRepresentation :: FIR p i o -> Poly Double 
polyRepresentation (FIRD p) = p 
polyRepresentation (FIR f) = fmap toDouble f

instance Show p => Show (FIR p i o) where 
  show (FIR c) = show c 
  show (FIRD d) = show d

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

bode :: (Sample o,Sample i)
     => FIR p i o -> StyledSignal Double Double
bode f = 
    let fd = polyRepresentation f
        log10 x = log x / log 10
        polyF = evalPoly (fmap (:+ 0) fd)
        transferFunction = \t -> 20 * log10 (magnitude . polyF . cis $ -t) 
        dw = 0.01
        values = map transferFunction [0,dw..pi]
        nb = length values
        ticksWithPhase n = printf "%.2f pi" (n / fromIntegral nb :: Double)
    in 
    (plotSpectrum nb [ AS $ fromListS 0 values]) 
                 `withNewStyle` (\s -> s { verticalLabel = Just "Filter Gain (dB)"
                                         , title = Just "Bode Plot"
                                         , horizontalTickRepresentation = ticksWithPhase
                                         })

phasePlot :: (Sample o,Sample i)
     => FIR p i o -> StyledSignal Double Double
phasePlot f = 
    let fd = polyRepresentation f
        log10 x = log x / log 10
        polyF = evalPoly (fmap (:+ 0) fd)
        transferFunction = \t -> phase . polyF . cis $ -t
        dw = 0.01
        values = map transferFunction [0,dw..pi]
        nb = length values
        ticksWithPhase n = printf "%.2f pi" (n / fromIntegral nb :: Double)
    in 
    (plotSpectrum nb [ AS $ fromListS 0 values]) 
                 `withNewStyle` (\s -> s { verticalLabel = Just "Filter Phase"
                                         , title = Just "Phase Plot"
                                         , horizontalTickRepresentation = ticksWithPhase
                                         })
