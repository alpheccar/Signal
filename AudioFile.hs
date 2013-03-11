{-# LANGUAGE ConstraintKinds #-}
module AudioFile(
	  writeMono
	, writeStereo
	, readMono
	) where 

import Prelude hiding(length,map,zip,head,take)
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
          -> Time -- ^ Sample duration
          -> Signal Time a
          -> IO () 
writeMono name duration signal = 
	let d = floor (getT duration * getF (samplingRate signal))
	    boundedSignal = take d . getSamples $ signal
	    samples = map (\x -> [doubleToSample . toDouble $ x]) boundedSignal 
	    w = wave (samplingRate signal) 1 d samples 
	in 
	putWAVEFile name w

writeStereo :: (Sample a, Sample b) 
            => FilePath 
            -> Time -- ^ Duration
            -> Signal Time a -- ^ Left 
            -> Signal Time b -- ^ Right
            -> IO () 
writeStereo name duration signall signalr = 
	let d = floor (getT duration * getF (samplingRate signall))
	    boundedSignal = take d . getSamples . zipS signall $ signalr
	    samples = map (\(x,y) -> [doubleToSample . toDouble $ x, doubleToSample . toDouble $ y]) boundedSignal 
	    w = wave (samplingRate signall) 2 d samples 
	in 
	putWAVEFile name w

readMono :: Sample a 
         => FilePath 
         -> IO (Signal Time a)
readMono name = do
	w <- getWAVEFile name
	let f = Frequency (fromIntegral $ waveFrameRate (waveHeader w)) 
	    t = dual f
	return . fromListS t 0 . map (fromDouble . sampleToDouble . head)  . waveSamples $ w