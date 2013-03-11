{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Main(
	  main
	
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
import Spectrogram 
import VAD

import qualified Debug.Trace as T

debug a = T.trace (show a) a

testa :: Signal t Int -> Signal t Int
--testa = map (+11) . map (+23)
testa =  mapS (+11) . mapS (+23) 


dr :: Time
dr = 4.0

sp :: Time 
sp = Time 0.01

samplingFrequency :: Frequency 
samplingFrequency = Frequency (1.0 / 0.01)

theTimes :: Signal Time Time
theTimes = uniformSamples sp 0.0

-- | This signal is working for any type with a Double representation
genericSignal :: forall a. Sample a => Signal Time a
genericSignal = mapS (\t ->  2*(fromDouble $ 1.5*sin (2*pi*getT t) + 0.4*sin(2*pi*20*getT t))) theTimes

constSignal :: Signal Time Double
constSignal = mapS (const 1.0) theTimes 

mySignalA :: Signal Time Double 
mySignalA = genericSignal 

mySignalC :: Signal Time (Fixed Int16 14 Sat NR) 
mySignalC = genericSignal

mySignalD :: Signal Time (Fixed Int16 3 Sat NR) 
mySignalD = genericSignal

mySignalE:: Signal Time (Fixed Int16 14 Unsat NR) 
mySignalE = genericSignal

mySignalB = mapS (\t -> 0.8*cos (2*pi*getT t*30)*(1.0 + 0.8*cos(2*pi*getT t*10))) theTimes

winv :: Signal Int Double
winv = 
  fromListS 1 0 $ map (\i -> cossq 100 i 1.0) [0..99]

soundTest = do
    let f = Frequency 16000 
        t = dual f
        theTimes = uniformSamples t 0.0
    r <- randomSamples t (-0.3) (0.3)

    let mySignal = zipWithS (+) r (mapS (\t -> sin(2*pi*getT t*4000)*sin(2*pi*0.25*getT t)) theTimes) :: Signal Time Double
        mySignal1 = (mapS (\t -> 0.5*sin(2*pi*getT t*4000)*cos(2*pi*0.25*getT t)) theTimes) :: Signal Time Double
    writeMono "sound_mono.wav" 4.0 mySignal1
    writeStereo "sound_stereo.wav" 4.0 mySignal mySignal1

bigPict = do 
    let f = Frequency 16000 
        tr = dual f
        theTimes = uniformSamples tr 0.0
        mySignal = (mapS (\t -> sin(2*pi*getT t*4000)*sin(2*pi*0.25*getT t)) theTimes) :: Signal Time Double


    --print $ takeS 160 mySignal
    display $ discreteSignalsWithStyle (floor $ getT dr * getF f)  plotStyle (theTimes) [ AS mySignal]

wav = do 
  s <- readMono "Test.wav" :: IO (Signal Time Double)
  let tr = dual (samplingRate s)
      theTimes = uniformSamples tr 0.0
  display $ discreteSignalsWithStyle (floor $ 2.0 * getF (samplingRate s))  plotStyle 
                 (theTimes) 
                 [ AS s]

myTest = do
  --s <- readMono "Test.wav" :: IO (Signal Time Double)
  let tr = dual (Frequency 44100)
      theTimes = {-# SCC "theTimes" #-} uniformSamples tr 0.0 :: Signal Time Time
      --s = mapS (\t -> 0.01*sin (2*pi*4000*getT t)) theTimes
      --si = vad s
      sv = samplingPeriod theTimes
      ----theFrames = uniformSamples sv 0
      --myLen !s (!a:l) = myLen (s+1) l
      --myLen !s [] = s
  display $ discreteSignalsWithStyle (floor $ 6.0 / getT sv)  plotStyle { horizontalBounds = Just (0,6.0)
                                                                        , verticalBounds = Just (0,6.0)
                                                                        }
                (theTimes) [ AS theTimes]


playWav = do 
  s <- readMono "Test.wav" :: IO (Signal Time Double)
  playS (Time 2.0) s

-- PROBLEM
wavSpect = do
  s0 <- readMono "Test.wav" :: IO (Signal Time (Fixed Int16 10 Sat NR))
  r <- randomSamples (samplingPeriod s0) (fromDouble $ -0.01) (fromDouble 0.01)
  let tr = dual (samplingRate s0)
      theTimes = {-# SCC "theTimes" #-} uniformSamples tr 0.0
      ampl = mapS (\t -> (fromDouble $ 1+0.2*sin(2*pi*getT t))) theTimes
      s = {-# SCC "theSignal" #-} zipWithS (+) s0 (zipWithS (*) ampl r)
      spect = {-# SCC "theSpectrogram" #-} spectrogram (mapS toDouble s) (Time 6.0) hann 20
      v = {-# SCC "theVad" #-} vad s
      sv = samplingPeriod v
      theFrames = {-# SCC "theFrames" #-} uniformSamples sv 0
      pict = {-# SCC "pict" #-} discreteSignalsWithStyle (floor $ 6.0 * getF (samplingRate s))  plotStyle 
                                (theTimes) [ AS s]
      pictv = {-# SCC "pictv" #-} discreteSignalsWithStyle (floor $ 6.0 / getT sv)  ( plotStyle {title = Just "VAD"
                                                                , interpolation = False})
                                                                  (theFrames) [ AS v]
  display $ {-# SCC "Vertical" #-} Vertical 0 [pict,pictv,spect]

debugFFT = do 
  s <- readMono "Test.wav" :: IO (Signal Time Double)
  let dr = Time 1.0
      spectruma = spectrum (Time 1.0) (noWindow) s 
      freqR = samplingPeriod spectruma 
      f = samplingRate s
      frequencies = uniformSamples freqR 0.0
      pictb = discreteSignalsWithStyle (floor $ getF f / getF freqR) fftStyle frequencies [ AS spectruma ]  
  display pictb

-- PROBLEM
overlapTest = do 
  let theTimes = uniformSamples 1 0.0 :: Signal Time Time
      s = mapS (\t -> 1.0) theTimes :: Signal Time Double
      nb = 1000 
      s1 = frameWithWinAndOverlap 100 50 hann  $ s
      s2 = flattenWithOverlapS 100 50 s1
  display $ discreteSignalsWithStyle nb plotStyle theTimes [AS s2]

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

pict = discreteSignalsWithStyle (floor $ getT dr * getF samplingFrequency) 
                                                                      plotStyle theTimes [ AS mySignalA
                                                                                         , AS mySignalC
                                                                                         , AS mySignalD
                                                                                         , AS mySignalE] 

pictwin = display $ discreteSignalsWithStyle 100 plotStyle (fromListS 1 0 ([0..99] :: [Int])) [AS winv] 

linearSignal :: forall a. Sample a => Signal Time a 
linearSignal = mapS (\t -> let x = (fromDouble $ getT t) in x*x) theTimes

linearS :: Signal Time Double
linearS = linearSignal

la :: Signal Time (Fixed Int16 4 Sat NR)
la = linearSignal

pictramp = display $ discreteSignalsWithStyle (floor $ getT dr * getF samplingFrequency)
                                                                      plotStyle theTimes [ AS linearS
                                                                                         , AS la
                                                                                         ]
randomSig :: (Show a, NFData a, Random a,Sample a, Resolution a) => a -> a -> IO ()
randomSig a b = do 
    clearTrace
    let sig = do 
        s <- randomSamples sp a b 
        let g = mapS (\t -> fromDouble (sin (2*pi*getT t))) theTimes
            r = zipWithS (+) s g 
        return $ trace "test" r
    sig >>= forceSignal 4000
    --s <- sig
    --display $ discreteSignalsWithStyle (floor $ getT duration * getF samplingFrequency) plotStyle theTimes [AS $ trace "test" s]

spectruma :: Signal Frequency Double
spectruma = spectrum dr (noWindow) mySignalA

spectrumConst :: Signal Frequency Double
spectrumConst = spectrum dr (noWindow) (mapS toDouble constSignal)  

-- To debug : fixed point typing problem
spectrumc :: Signal Frequency Double
spectrumc = spectrum dr noWindow (mapS toDouble mySignalC)  

-- To debug : fixed point typing problem
spectrumd :: Signal Frequency Double
spectrumd = spectrum dr noWindow mySignalD  

spectrumb :: Signal Frequency Double
spectrumb = spectrum dr noWindow mySignalB

spectrume:: Signal Frequency Double
spectrume = spectrum dr noWindow (mapS toDouble mySignalE)

frequencies :: Signal Frequency Frequency
frequencies = uniformSamples (samplingPeriod spectruma) 0.0

pictb = discreteSignalsWithStyle (floor $ 100.0 / getF (samplingPeriod spectruma)) fftStyle 
                                                                            frequencies [ AS spectruma 
                                                                                        , AS spectrumc 
                                                                                        , AS spectrumd
                                                                                        , AS spectrume]  

main = myTest
