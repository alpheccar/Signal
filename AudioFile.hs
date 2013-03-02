{-# LANGUAGE ConstraintKinds #-}
module AudioFile(
	  writeMono
	, writeStereo
	, readMono
	) where 

import Prelude hiding(length,map,zip,head)
import Data.WAVE 
import Signal
import Common
import Data.List.Stream

wave f c l samples = WAVE 
 { waveHeader = WAVEHeader { waveNumChannels = c
                           , waveFrameRate = floor (getF f)
                           , waveBitsPerSample = 32
                           , waveFrames = Just l
  	                       }
 , waveSamples = samples 
 }

writeMono :: Sample a 
          => FilePath 
          -> Frequency -- ^ sampling frequency
          -> Time -- ^ Sample duration
          -> Signal a
          -> IO () 
writeMono name f duration signal = 
	let d = floor (getT duration * getF f)
	    boundedSignal = takeS d signal
	    samples = map (\x -> [doubleToSample . toDouble $ x]) boundedSignal 
	    w = wave f 1 d samples 
	in 
	putWAVEFile name w

writeStereo :: (Sample a, Sample b) 
            => FilePath 
            -> Frequency -- ^ sampling frequency
            -> Time -- ^ Duration
            -> Signal a -- ^ Left 
            -> Signal b -- ^ Right
            -> IO () 
writeStereo name f duration signall signalr = 
	let d = floor (getT duration * getF f)
	    boundedSignal = takeS d . zipS signall $ signalr
	    samples = map (\(x,y) -> [doubleToSample . toDouble $ x, doubleToSample . toDouble $ y]) boundedSignal 
	    w = wave f 2 d samples 
	in 
	putWAVEFile name w

readMono :: Sample a 
         => FilePath 
         -> IO (Signal a, Frequency)
readMono name = do
	w <- getWAVEFile name
	let f = Frequency (fromIntegral $ waveFrameRate (waveHeader w)) 
	    s = fromListS . map (fromDouble . sampleToDouble . head)  . waveSamples $ w
	return (s,f)