{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module Main(
          main

        ) where

import Prelude hiding((++))
import Signal.Plot
import Graphics.PDF hiding(Orientation(..), Vertical)
import Signal.Transform
import Signal
import Signal.Generators
import Signal.Fixed
import Signal.TestCases
import Signal.Common
import Signal.Windows
import qualified Data.Vector.Unboxed as U
import qualified Numeric.GSL.Fourier as F
import Signal.Trace
import System.Random 
import HaskellViewer.Displayable
import Control.DeepSeq
import Control.Applicative((<$>))
import Signal.AudioFile
import HaskellViewer.Playable
import HaskellViewer.Viewer(play)
import Signal.Spectrogram
import Signal.VAD
import Data.List.Stream((++))
import System.Timeout
import Signal.Noise
import System.IO
import Signal.Filter
import Math.Polynomial 

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

testBode = do 
  let nb = 5 
      fi = FIRD (poly LE (replicate nb (1.0 / fromIntegral nb))) :: FIR Double Double Double
  display $ Vertical 0 [bode fi, phasePlot fi] 

testFIR = do
  let nb = 3
      fi = FIRD (poly LE (replicate nb (1.0 / fromIntegral nb))) :: FIR Double Double Double
      coefs = map fromDouble $ replicate nb (1.0 / fromIntegral nb) :: [Fixed Int16 6 Sat RO]
      fif = FIR (poly LE coefs) :: FIR ((Fixed Int32 12 Sat RO),(Fixed Int16 6 Sat RO)) (Fixed Int16 6 Sat RO) (Fixed Int16 6 Sat RO)
      s = fromListS 0 (replicate 100 1.0) :: Signal Double 
      --s = fromListS 0 [1] :: Signal Double
      fs = (transferFunction fi) s
      p = plotSignals 1000 1 [AS fs]
      amplitude = 1.0
  r <- randomSamples (fromDouble $ -amplitude) (fromDouble amplitude) :: IO (Signal (Fixed Int16 6 Sat RO))
  let theTimes = uniformSamples (Time 1.0) 0.0 :: Signal Time
      --s = mapS (\t -> fromDouble $ 0.5*sin(2*pi*getT t / 1000) ) theTimes
  qn <- quantizationNoise r fif  
  let nbPoints = 1024
      h = histogram (takeS nbPoints qn)
      spect = spectrum  (noWindow) nbPoints $ Sampled 1 qn
      s = plotSpectrum nbPoints [ AS spect] 
  display $ Vertical 0 [p,h, s]

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
    display $ plotSignals (floor $ getT dr * getF f)  tr [ AS mySignal]

wav = do 
  s <- readMono "Test.wav" :: IO (Sampled Time Double)
  let theTimes = uniformSamples (period s) 0.0
  display $ plotSignals (floor $ 2.0 * getF (rate s))  (period s) [ AS s]

avad = do
  s <- readMono "Test.wav" :: IO (Sampled Time Double)
  let v = vad s
  let theTimes = uniformSamples (period s) 0.0
  display $ plotSignals (floor $ 2.0 * getF (rate s))  (period s) [ AS s, AS v]


myTest = do
  s <- readMono "Test.wav" :: IO (Sampled Time Double)
  let tr = dual (Frequency 44100)
--       theTimes = {-# SCC "theTimes" #-} uniformSamples s 0.0 :: Signal Time
      theTimes = {-# SCC "theTimes" #-} uniformSamples tr 0.0 :: Signal Time
      --s = mapS (\t -> 0.01*sin (2*pi*4000*getT t)) theTimes
      si = vad s
--       sv = tr
      sv = si
--       theFrames = uniformSamples sv 0
      --myLen !s (!a:l) = myLen (s+1) l
      --myLen !s [] = s
--   display $ discreteSignalsWithStyle (floor $ 6.0 / getT sv)  plotStyle { horizontalBounds = Just (0,6.0)
  display $ discreteSignalsWithStyle (floor $ 6.0 /  samplingPeriod s)  plotStyle { horizontalBounds = Just (0,6.0)
                                                                        , verticalBounds = Just (0,6.0)
                                                                        }
                [ AS theTimes]


playWav = do 
  s <- readMono "Test.wav" :: IO (Sampled Time Double)
  playS (Time 2.0) s

testHist = do 
    r <- randomSamples (fromDouble $ -0.01) (fromDouble 0.01) :: IO (Signal Double)
    let l = takeS 10000 r
    display $ histogram l 

-- For testing ONLY
data Amp f i o where 
 Amp :: f -> Amp f f f 

instance Structure Amp where 
    doubleVersion (Amp f) = Amp (toDouble f)
    transferFunction (Amp f) = mapS (f * )

writeSignal :: [Double] -> FilePath -> IO ()
writeSignal l f = do 
    h <- openFile f WriteMode 
    writeS h l 
    hClose h 
 where 
  writeS h ([]) = return ()
  writeS h (a:b) = do 
    hPutStrLn h (show a)
    writeS h b 

testQuantMult = do 
  let amplitude = 1.0 
  ra <- randomSamples (fromDouble $ -amplitude) (fromDouble amplitude) :: IO (Signal (Fixed Int16 8 Sat RO))
  rb <- randomSamples (fromDouble $ -amplitude) (fromDouble amplitude) :: IO (Signal (Fixed Int16 8 Sat RO))
  let sf = zipWithS (*) ra rb 
      sd = zipWithS (*) (mapS toDouble ra) (mapS toDouble rb)
      sq x = x * x 
      qerr :: Double -> Double -> Double
      qerr a b =  (a-b)
      quanterrors = zipWithS qerr (mapS toDouble sf) sd 
      nb = 1024
      h = histogram  (takeS nb quanterrors) 
      spect = spectrum  (noWindow) nb $ Sampled 1 quanterrors
      s = plotSpectrum nb [ AS spect] 

  display $ Vertical 0 [h,s]

testQuant = do 
  let amplitude = 1.0
  r <- randomSamples (fromDouble $ -amplitude) (fromDouble amplitude) :: IO (Signal (Fixed Int16 8 Sat NR))
  let theTimes = uniformSamples (Time 1.0) 0.0 :: Signal Time
      s = mapS (\t -> fromDouble $ 0.5*sin(2*pi*getT t / 1000) ) theTimes
      --ts = zipWithS (+) r s
      a = Amp (smallestValue (undefined :: Fixed Int16 8 Sat NR))
  qn <- quantizationNoise s a 
  let nb = 10000
      nbf = 8192
      spect = spectrum  (noWindow) nbf $ Sampled 1 qn
      frequencies = uniformSamples (period spectruma) 0.0
      h = histogram (takeS nb qn)
      s = plotSpectrum nbf [ AS spect] 
      n = plotSignals nb 1.0 [AS qn]
      debugS = takeS nbf qn 
  --writeSignal debugS "debugs.dat"
  display $ Vertical 0 [h,s,n] 

-- PROBLEM
wavSpect = do
  s0 <- readMono "Test.wav" :: IO (Sampled Time (Fixed Int16 10 Sat NR))
  r <- randomSamples (fromDouble $ -0.01) (fromDouble 0.01)
  let p = period ({-# SCC "s0" #-} s0)
      ra = rate s0
      duration = 6.0
      theTimes = {-# SCC "theTimes" #-} uniformSamples p 0.0
      ampl = mapS (\t -> (fromDouble $ 1+0.2*sin(2*pi*getT t))) theTimes
      s = {-# SCC "theSignal" #-} zipWithS (+) (getSignal s0) (zipWithS (*) ampl ({-# SCC "r" #-} r))
      spect = {-# SCC "theSpectrogram" #-} spectrogram (Sampled p (mapS toDouble s)) (Time duration) hann 20
      v = {-# SCC "theVad" #-} vad $ Sampled p s
      theFrames = {-# SCC "theFrames" #-} uniformSamples (period v) 0
      pict = {-# SCC "pict" #-} plotSignals (floor $ duration * getF ra)  p [ AS s]
      pictv = {-# SCC "pictv" #-} plotSignals (floor $ duration / getT (period v))  (period v) [ AS v]
  display $ {-# SCC "Vertical" #-} Vertical 0 [pict,pictv,spect]

debugFFT = do 
  s <- readMono "Test.wav" :: IO (Sampled Time Double)
  let dr = Time 1.0
      spectruma = spectrum (noWindow) (floor $ rate s) s
      f = rate s
      frequencies = uniformSamples (period spectruma) 0.0
      pictb = plotSpectrum (floor $ getF f / getF (period spectruma)) [ AS spectruma ]  
  display pictb

-- PROBLEM
overlapTest = do 
  let theTimes = uniformSamples 1 0.0 :: Signal Time
      s = mapS (\t -> 1.0) theTimes :: Signal Double
      nb = 1000 
      s1 = frameWithWinAndOverlap 100 50 hann (Sampled 1 s)
      s2 = flattenWithOverlapS 100 50 s1
  display $ plotSignals nb (period s2)  [AS s2]

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

pict = display $ plotSignals (floor $ getT dr * getF samplingFrequency) sp [ AS mySignalA
                                                                           , AS mySignalC
                                                                           , AS mySignalD
                                                                           , AS mySignalE] 

pictwin = display $ plotSignals 100 (Time 1.0) [AS winv] 

linearSignal :: forall a. Sample a => Signal a 
linearSignal = mapS (\t -> let x = (fromDouble $ getT t) in x*x) theTimes

linearS :: Signal Double
linearS = linearSignal

la :: Signal (Fixed Int16 4 Sat NR)
la = linearSignal

pictramp = display $ plotSignals  (floor $ getT dr * getF samplingFrequency) (Time 1.0) [ AS linearS
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
spectruma = spectrum  (noWindow) (floor (getT dr / getT sp)) $ Sampled sp mySignalA

spectrumConst :: Sampled Frequency Double
spectrumConst = spectrum (noWindow) (floor (getT dr / getT sp)) $ Sampled sp (mapS toDouble aConstSignal)  

-- To debug : fixed point typing problem
spectrumc :: Sampled Frequency Double
spectrumc = spectrum noWindow (floor (getT dr / getT sp)) $ Sampled sp (mapS toDouble mySignalC)  

-- To debug : fixed point typing problem
spectrumd :: Sampled Frequency Double
spectrumd = spectrum noWindow (floor (getT dr / getT sp)) $ Sampled sp mySignalD  

spectrumb :: Sampled Frequency Double
spectrumb = spectrum noWindow (floor (getT dr / getT sp)) $ Sampled sp mySignalB

spectrume:: Sampled Frequency Double
spectrume = spectrum noWindow (floor (getT dr / getT sp)) $ Sampled sp (mapS toDouble mySignalE)

frequencies :: Signal Frequency
frequencies = uniformSamples (period spectruma) 0.0


pictb = display $ plotSpectrum nb  [ AS spectruma 
                                   , AS spectrumc 
                                   , AS spectrumd
                                   , AS spectrume]  
    where 
      nb = (nbSamples spectruma (Frequency 100.0))

main = do 
  wavSpect
  return ()

testInt40 = 
  let a = 3 :: Fixed Int40 8 Sat NR 
      v = U.generate 4 (\i -> (fromIntegral i) :: Fixed Int40 8 Sat NR) 
      z = U.map (/ a) v 
  in 
  print . U.toList $ z

