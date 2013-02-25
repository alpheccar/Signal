{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Test(
	  testa
	, test1
	, test2
	) where 

import Plot
import Graphics.PDF hiding(Orientation(..))
import Transform
import Signal
import Generators
import Fixed
import TestCases
import Common 
import Windows
import qualified Data.Vector.Unboxed as U
import qualified Numeric.GSL.Fourier as F


testa :: Signal Int -> Signal Int
--testa = map (+11) . map (+23)
testa =  mapS (+11) . mapS (+23) 

test1 = takeS 10  . testa $ fromListS ([1..] :: [Int])
test2 = takeS 11  . consS 26 . testa $ fromListS ([1..] :: [Int])


duration :: Time
duration = 4.0

samplingPeriod :: Time 
samplingPeriod = Time 0.01

samplingFrequency :: Frequency 
samplingFrequency = Frequency (1.0 / 0.01)

theTimes = uniformSamples samplingPeriod 0.0

-- | This signal is working for any type with a Double representation
genericSignal :: forall a. (Fractional a, HasDoubleRepresentation a) => Signal a
genericSignal = mapS (\t ->  2*(fromDouble $ 1.5*sin (2*pi*getT t)) ) theTimes

constSignal :: Signal Double
constSignal = mapS (const 1.0) theTimes 

mySignalA :: Signal Double 
mySignalA = genericSignal 

mySignalC :: Signal (Fixed Int16 14 Saturated) 
mySignalC = genericSignal

mySignalD :: Signal (Fixed Int16 3 Saturated) 
mySignalD = genericSignal

mySignalE:: Signal (Fixed Int16 14 Unsaturated) 
mySignalE = genericSignal

mySignalB = mapS (\t -> 0.8*cos (2*pi*getT t*30)*(1.0 + 0.8*cos(2*pi*getT t*10))) theTimes

win :: Signal Double
win = fromListS $ map (\i -> tukey 0.3 100 i 1.0) [0..99]

plotStyle =  
	let lightBlue = Rgb 0.6 0.6 1.0
	    lightRed = Rgb 1.0 0.6 0.6
	    lightGreen = Rgb 0.6 1.0 0.6
	in
	(defaultPlotStyle { title = Just "Temporal"
        	          , signalStyles = [ defaultSignalStyle 1.0 lightBlue
        	                           , defaultSignalStyle 1.0 lightRed
        	                           , defaultSignalStyle 1.0 lightGreen
        	                           ]
        	          , verticalLabel = Just "Amplitude"
        	          })  
fftStyle = 
	let lightBlue = Rgb 0.6 0.6 1.0
	    lightRed = Rgb 1.0 0.6 0.6
	    lightGreen = Rgb 0.6 1.0 0.6
	in
	plotStyle { verticalLabel = Just "Energy", title = Just "Frequential", horizontalLabel = Just "Hz"
                     , signalStyles = [ defaultSignalStyle 1.0 lightBlue
        	                          , defaultSignalStyle 1.0 lightRed
        	                          , defaultSignalStyle 1.0 lightGreen
        	                          ]
        	         }

pict = display $ discreteSignalsWithStyle (takeWhileS (<= duration) theTimes) plotStyle [ AS mySignalA
                                                                                        , AS mySignalC
                                                                                        , AS mySignalD
                                                                                        , AS mySignalE] 

pictwin = display $ discreteSignalsWithStyle ([0..99] :: [Double]) plotStyle [AS win] 

spectruma :: Signal Double
(freqR,spectruma) = spectrum samplingFrequency duration (noWindow) mySignalA

spectrumConst :: Signal Double
(_,spectrumConst) = spectrum samplingFrequency duration (noWindow) (mapS toDouble constSignal)  

-- To debug : fixed point typing problem
spectrumc :: Signal Double
(_,spectrumc) = spectrum samplingFrequency duration (tukey 0.3) (mapS toDouble mySignalC)  

-- To debug : fixed point typing problem
spectrumd :: Signal Double
(_,spectrumd) = spectrum samplingFrequency duration (tukey 0.3) mySignalD  

spectrumb :: Signal Double
(_,spectrumb) = spectrum samplingFrequency duration (tukey 0.3) mySignalB

frequencies :: Signal Frequency
frequencies = uniformSamples freqR 0.0

pictb = display $ discreteSignalsWithStyle (takeWhileS (<= 100.0) frequencies) fftStyle [ AS spectruma 
                                                                                        , AS spectrumc 
                                                                                        , AS spectrumd]  
