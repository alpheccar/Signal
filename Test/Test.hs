{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Test(
	  testa
	, test1
	, test2
	) where 

import Plot
import Graphics.PDF hiding(Orientation(..), Vertical)
import Transform
import Signal
import Generators
import Fixed
import TestCases
import Common 
import Windows
import qualified Data.Vector.Unboxed as U
import qualified Numeric.GSL.Fourier as F
import Trace
import System.Random 
import Displayable
import Control.DeepSeq
import Control.Applicative((<$>))
import AudioFile 
import Playable 
import Viewer(play)

import qualified Debug.Trace as T

debug a = T.trace (show a) a

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
genericSignal :: forall a. Sample a => Signal a
genericSignal = mapS (\t ->  2*(fromDouble $ 1.5*sin (2*pi*getT t) + 0.4*sin(2*pi*20*getT t))) theTimes

constSignal :: Signal Double
constSignal = mapS (const 1.0) theTimes 

mySignalA :: Signal Double 
mySignalA = genericSignal 

mySignalC :: Signal (Fixed Int16 14 Sat NR) 
mySignalC = genericSignal

mySignalD :: Signal (Fixed Int16 3 Sat NR) 
mySignalD = genericSignal

mySignalE:: Signal (Fixed Int16 14 Unsat NR) 
mySignalE = genericSignal

mySignalB = mapS (\t -> 0.8*cos (2*pi*getT t*30)*(1.0 + 0.8*cos(2*pi*getT t*10))) theTimes

win :: Signal Double
win = fromListS $ map (\i -> tukey 0.3 100 i 1.0) [0..99]

sound = do
    r <- randomSamples (-0.3) (0.3)
    let f = Frequency 16000 
        t = Time (1.0 / getF f)
        theTimes = uniformSamples t 0.0

        mySignal = zipWithS (+) r (mapS (\t -> sin(2*pi*getT t*4000)*sin(2*pi*0.25*getT t)) theTimes) :: Signal Double
        mySignal1 = (mapS (\t -> 0.5*sin(2*pi*getT t*4000)*cos(2*pi*0.25*getT t)) theTimes) :: Signal Double
    writeMono "sound_mono.wav" f 4.0 mySignal1
    writeStereo "sound_stereo.wav" f 4.0 mySignal mySignal1

bigPict = do 
    let f = Frequency 16000 
        tr = Time (1.0 / getF f)
        theTimes = uniformSamples tr 0.0
        mySignal = (mapS (\t -> sin(2*pi*getT t*4000)*sin(2*pi*0.25*getT t)) theTimes) :: Signal Double


    --print $ takeS 160 mySignal
    display $ discreteSignalsWithStyle (floor $ getT duration * getF f)  plotStyle (theTimes) [ AS mySignal]

wav = do 
  (s,f) <- readMono "Test.wav" :: IO (Signal Double, Frequency)
  let tr = Time (1.0 / getF f)
      theTimes = uniformSamples tr 0.0
  display $ discreteSignalsWithStyle (floor $ 2.0 * getF f)  plotStyle (theTimes) [ AS s]

playWav = do 
  (s,f) <- readMono "Test.wav" :: IO (Signal Double, Frequency)
  play ((getF f),(takeS (floor (2 * f)) . mapS toDouble $ s))

lightBlue = Rgb 0.6 0.6 1.0
lightRed = Rgb 1.0 0.6 0.6
lightGreen = Rgb 0.6 1.0 0.6
lightYellow = Rgb 1.0 1.0 0.6

plotStyle =  
	(defaultPlotStyle { title = Just "Temporal"
        	          , signalStyles = [ defaultSignalStyle 0.8 lightBlue
        	                           , defaultSignalStyle 0.8 lightRed
        	                           , defaultSignalStyle 0.8 lightGreen
                                     , defaultSignalStyle 0.8 lightYellow
        	                           ]
        	          , verticalLabel = Just "Amplitude"
        	          })  
fftStyle = 
	plotStyle { verticalLabel = Just "Energy", title = Just "Frequential", horizontalLabel = Just "Hz"
                     , signalStyles = [ defaultSignalStyle 0.8 lightBlue
        	                            , defaultSignalStyle 0.8 lightRed
        	                            , defaultSignalStyle 0.8 lightGreen
                                      , defaultSignalStyle 0.8 lightYellow
        	                          ]
        	         }

pict = discreteSignalsWithStyle (floor $ getT duration * getF samplingFrequency) 
                                                                      plotStyle theTimes [ AS mySignalA
                                                                                         , AS mySignalC
                                                                                         , AS mySignalD
                                                                                         , AS mySignalE] 

pictwin = display $ discreteSignalsWithStyle 100 plotStyle (fromListS ([0..99] :: [Double])) [AS win] 

linearSignal :: forall a. Sample a => Signal a 
linearSignal = mapS (\t -> let x = (fromDouble $ getT t) in x*x) theTimes

linearS :: Signal Double
linearS = linearSignal

la :: Signal (Fixed Int16 4 Sat NR)
la = linearSignal

pictramp = display $ discreteSignalsWithStyle (floor $ getT duration * getF samplingFrequency)
                                                                      plotStyle theTimes [ AS linearS
                                                                                         , AS la
                                                                                         ]
randomSig :: (Show a, NFData a, Random a,Sample a, Resolution a) => a -> a -> IO ()
randomSig a b = do 
    clearTrace
    let sig = do 
        s <- randomSamples a b 
        let g = mapS (\t -> fromDouble (sin (2*pi*getT t))) theTimes
            r = zipWithS (+) s g 
        return $ trace "test" r
    sig >>= forceSignal 4000
    --s <- sig
    --display $ discreteSignalsWithStyle (floor $ getT duration * getF samplingFrequency) plotStyle theTimes [AS $ trace "test" s]

spectruma :: Signal Double
(freqR,spectruma) = spectrum samplingFrequency duration (noWindow) mySignalA

spectrumConst :: Signal Double
(_,spectrumConst) = spectrum samplingFrequency duration (noWindow) (mapS toDouble constSignal)  

-- To debug : fixed point typing problem
spectrumc :: Signal Double
(_,spectrumc) = spectrum samplingFrequency duration noWindow (mapS toDouble mySignalC)  

-- To debug : fixed point typing problem
spectrumd :: Signal Double
(_,spectrumd) = spectrum samplingFrequency duration noWindow mySignalD  

spectrumb :: Signal Double
(_,spectrumb) = spectrum samplingFrequency duration noWindow mySignalB

spectrume:: Signal Double
(_,spectrume) = spectrum samplingFrequency duration noWindow (mapS toDouble mySignalE)

frequencies :: Signal Frequency
frequencies = uniformSamples freqR 0.0

pictb = discreteSignalsWithStyle (floor $ 100.0 / getF freqR) fftStyle 
                                                                            frequencies [ AS spectruma 
                                                                                        , AS spectrumc 
                                                                                        , AS spectrumd
                                                                                        , AS spectrume]  
