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

testa :: Signal Int -> Signal Int
--testa = map (+11) . map (+23)
testa =  mapS (+11) . mapS (+23) 

test1 = takeS 10  . testa $ fromListS ([1..] :: [Int])
test2 = takeS 11  . consS 26 . testa $ fromListS ([1..] :: [Int])


duration :: Double
duration = 4.0
theTimes = uniformSamples 0.01 0.0

-- | This signal is working for any type with a Double representation
genericSignal :: forall a. (Fractional a, HasDoubleRepresentation a) => Signal a
genericSignal = mapS (\t ->  2*(fromDouble $ 1.5*sin (2*pi*t)) ) theTimes

mySignalA :: Signal Double 
mySignalA = genericSignal 

mySignalC :: Signal (Fixed Int16 14 Saturated) 
mySignalC = genericSignal

mySignalD :: Signal (Fixed Int16 3 Saturated) 
mySignalD = genericSignal

mySignalB = mapS (\t -> 0.8*cos (2*pi*t*30)*(1.0 + 0.8*cos(2*pi*t*10))) theTimes

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
fftStyle = plotStyle {verticalLabel = Just "Energy", title = Just "Frequential", horizontalLabel = Just "Hz"}

pict = display $ discreteSignalsWithStyle (takeWhileS (<= duration) theTimes) [AS mySignalA,AS mySignalC, AS mySignalD] plotStyle 

n = duration / 0.01
spectruma :: Signal Double
spectruma = mapS fromDouble . fromVectorS . spectrum 0.01 . takeVectorS (floor n) . mapS toDouble $ mySignalA

spectrumc :: Signal Double
spectrumc = mapS fromDouble . fromVectorS . spectrum 0.01 . takeVectorS (floor n) . mapS toDouble $ mySignalC  

spectrumb :: Signal Double
spectrumb = mapS fromDouble . fromVectorS . spectrum 0.01 . takeVectorS (floor n) . mapS toDouble $ mySignalB

frequencies :: Signal Double
frequencies = (uniformSamples (1.0 / duration) 0.0)

pictb = display $ discreteSignalsWithStyle (takeWhileS (<= 100.0) frequencies) [AS spectruma,AS spectrumc] fftStyle 
