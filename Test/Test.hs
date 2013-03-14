{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Main(
	  main
	
	) where 

import Prelude hiding((++))
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
import Data.List.Stream((++))
import System.Timeout

import qualified Debug.Trace as T

debug a = T.trace (show a) a

testa :: Signal Int -> Signal Int
--testa = map (+11) . map (+23)
testa =  mapS (+11) . mapS (+23) 


dr :: Time
dr = 4.0

sp :: Time 
sp = Time 0.01

samplingFrequency :: Frequency 
samplingFrequency = Frequency (1.0 / 0.01)

theTimes :: Signal Time
theTimes = uniformSamples sp 0.0

-- | This signal is working for any type with a Double representation
genericSignal :: forall a. Sample a => Signal a
genericSignal = mapS (\t ->  2*(fromDouble $ 1.5*sin (2*pi*getT t) + 0.4*sin(2*pi*20*getT t))) theTimes

aConstSignal :: Signal Double
aConstSignal = mapS (const 1.0) theTimes 

mySignalA :: Signal Double 
mySignalA = genericSignal 

mySignalC :: Signal (Fixed Int16 14 Sat NR) 
mySignalC = genericSignal

mySignalD :: Signal (Fixed Int16 3 Sat NR) 
mySignalD = genericSignal

mySignalE:: Signal (Fixed Int16 14 Unsat NR) 
mySignalE = genericSignal

mySignalB = mapS (\t -> 0.8*cos (2*pi*getT t*30)*(1.0 + 0.8*cos(2*pi*getT t*10))) theTimes

winv :: Signal Double
winv = 
  fromListS 0 $ map (\i -> cossq 100 i 1.0) [0..99]

soundTest = do
    let f = Frequency 16000 
        t = dual f
        theTimes = uniformSamples t 0.0
    r <- randomSamples (-0.3) (0.3)

    let mySignal = zipWithS (+) r (mapS (\t -> sin(2*pi*getT t*4000)*sin(2*pi*0.25*getT t)) theTimes) :: Signal Double
        mySignal1 = (mapS (\t -> 0.5*sin(2*pi*getT t*4000)*cos(2*pi*0.25*getT t)) theTimes) :: Signal Double
    writeMono "sound_mono.wav" 4.0 $ Sampled t mySignal1
    writeStereo "sound_stereo.wav" 4.0 $ Sampled t (zipS mySignal mySignal1)

bigPict = do 
    let f = Frequency 16000 
        tr = dual f
        theTimes = uniformSamples tr 0.0
        mySignal = (mapS (\t -> sin(2*pi*getT t*4000)*sin(2*pi*0.25*getT t)) theTimes) :: Signal Double


    --print $ takeS 160 mySignal
    display $ discreteSignalsWithStyle (floor $ getT dr * getF f)  plotStyle (AS theTimes) [ AS mySignal]

wav = do 
  s <- readMono "Test.wav" :: IO (Sampled Time Double)
  let theTimes = uniformSamples (period s) 0.0
  display $ discreteSignalsWithStyle (floor $ 2.0 * getF (rate s))  plotStyle 
                 (AS theTimes) 
                 [ AS s]

myTest = do
  --s <- readMono "Test.wav" :: IO (Signal Time Double)
  let tr = dual (Frequency 44100)
      theTimes = {-# SCC "theTimes" #-} uniformSamples tr 0.0 :: Signal Time
      --s = mapS (\t -> 0.01*sin (2*pi*4000*getT t)) theTimes
      --si = vad s
      sv = tr
      ----theFrames = uniformSamples sv 0
      --myLen !s (!a:l) = myLen (s+1) l
      --myLen !s [] = s
  display $ discreteSignalsWithStyle (floor $ 6.0 / getT sv)  plotStyle { horizontalBounds = Just (0,6.0)
                                                                        , verticalBounds = Just (0,6.0)
                                                                        }
                (AS theTimes) [ AS theTimes]


playWav = do 
  s <- readMono "Test.wav" :: IO (Sampled Time Double)
  playS (Time 2.0) s

-- PROBLEM
wavSpect = do
  s0 <- readMono "Test.wav" :: IO (Sampled Time (Fixed Int16 10 Sat NR))
  r <- randomSamples (fromDouble $ -0.01) (fromDouble 0.01)
  let duration = 6.0
      theTimes = {-# SCC "theTimes" #-} uniformSamples (period s0) 0.0
      ampl = mapS (\t -> (fromDouble $ 1+0.2*sin(2*pi*getT t))) theTimes
      s = {-# SCC "theSignal" #-} zipWithS (+) (getSignal s0) (zipWithS (*) ampl r)
      spect = {-# SCC "theSpectrogram" #-} spectrogram (Sampled (period s0) (mapS toDouble s)) (Time duration) hann 20
      v = {-# SCC "theVad" #-} vad $ Sampled (period s0) s
      theFrames = {-# SCC "theFrames" #-} uniformSamples (period v) 0
      pict = {-# SCC "pict" #-} discreteSignalsWithStyle (floor $ duration * getF (rate s0))  plotStyle 
                                (AS theTimes) [ AS s]
      pictv = {-# SCC "pictv" #-} discreteSignalsWithStyle (floor $ duration / getT (period v))  
                                    ( plotStyle {title = Just "VAD" , interpolation = False})
                                      (AS theFrames) [ AS v]
  display $ {-# SCC "Vertical" #-} Vertical 0 [pict,pictv,spect]

debugFFT = do 
  s <- readMono "Test.wav" :: IO (Sampled Time Double)
  let dr = Time 1.0
      spectruma = spectrum (noWindow) (Time 1.0) s
      f = rate s
      frequencies = uniformSamples (period spectruma) 0.0
      pictb = discreteSignalsWithStyle (floor $ getF f / getF (period spectruma)) fftStyle (AS frequencies) [ AS spectruma ]  
  display pictb

-- PROBLEM
overlapTest = do 
  let theTimes = uniformSamples 1 0.0 :: Signal Time
      s = mapS (\t -> 1.0) theTimes :: Signal Double
      nb = 1000 
      s1 = frameWithWinAndOverlap 100 50 hann (Sampled 1 s)
      s2 = flattenWithOverlapS 100 50 s1
  display $ discreteSignalsWithStyle nb plotStyle (AS theTimes) [AS s2]

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
                                                                      plotStyle (AS theTimes) [ AS mySignalA
                                                                                              , AS mySignalC
                                                                                              , AS mySignalD
                                                                                              , AS mySignalE] 

pictwin = display $ discreteSignalsWithStyle 100 plotStyle (AS $ fromListS 0 ([0..99] :: [Int])) [AS winv] 

linearSignal :: forall a. Sample a => Signal a 
linearSignal = mapS (\t -> let x = (fromDouble $ getT t) in x*x) theTimes

linearS :: Signal Double
linearS = linearSignal

la :: Signal (Fixed Int16 4 Sat NR)
la = linearSignal

pictramp = display $ discreteSignalsWithStyle (floor $ getT dr * getF samplingFrequency)
                                                                      plotStyle (AS theTimes) [ AS linearS
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

spectruma :: Sampled Frequency Double
spectruma = spectrum  (noWindow) dr $ Sampled sp mySignalA

spectrumConst :: Sampled Frequency Double
spectrumConst = spectrum (noWindow) dr $ Sampled sp (mapS toDouble aConstSignal)  

-- To debug : fixed point typing problem
spectrumc :: Sampled Frequency Double
spectrumc = spectrum noWindow dr $ Sampled sp (mapS toDouble mySignalC)  

-- To debug : fixed point typing problem
spectrumd :: Sampled Frequency Double
spectrumd = spectrum noWindow dr $ Sampled sp mySignalD  

spectrumb :: Sampled Frequency Double
spectrumb = spectrum noWindow dr $ Sampled sp mySignalB

spectrume:: Sampled Frequency Double
spectrume = spectrum noWindow dr $ Sampled sp (mapS toDouble mySignalE)

frequencies :: Signal Frequency
frequencies = uniformSamples (period spectruma) 0.0

pictb = discreteSignalsWithStyle (nbSamples spectruma (Frequency 100.0))  fftStyle 
                                                                            (AS frequencies) [ AS spectruma 
                                                                                             , AS spectrumc 
                                                                                             , AS spectrumd
                                                                                             , AS spectrume]  

main = do 
  wavSpect
  return ()

testInt40 = 
  let a = 3 :: Fixed Int40 8 Sat NR 
      v = U.generate 4 (\i -> (fromIntegral i) :: Fixed Int40 8 Sat NR) 
      z = U.map (/ a) v 
  in 
  print . U.toList $ z

