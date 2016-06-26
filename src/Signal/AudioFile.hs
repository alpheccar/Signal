{-# LANGUAGE ConstraintKinds #-}
module Signal.AudioFile(
          writeMono
        , writeStereo
        , readMono
        ) where

import Prelude hiding(length,map,zip,head,take)
import Data.List.Stream (map, head)
import Data.WAVE

import Signal
import Signal.Common

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
          -> Sampled Time a
          -> IO () 
writeMono name duration signal = 
        let theRate = rate signal
            d = floor (getT duration * getF theRate)
            boundedSignal = takeS d  $ (getSignal signal)
            samples = map (\x -> [doubleToSample . toDouble $ x]) boundedSignal
            w = wave theRate 1 d samples
        in
        putWAVEFile name w

writeStereo :: (Sample left, Sample right) 
            => FilePath 
            -> Time -- ^ Duration
            -> Sampled Time (left,right) 
            -> IO () 
writeStereo name duration signal = 
        let theRate = rate signal
            d = floor (getT duration * getF theRate)
            boundedSignal = takeS d (getSignal signal)
            samples = map (\(x,y) -> [doubleToSample . toDouble $ x, doubleToSample . toDouble $ y]) boundedSignal
            w = wave theRate 2 d samples
        in
        putWAVEFile name w

readMono :: Sample a 
         => FilePath 
         -> IO (Sampled Time a)
readMono name = do
        w <- getWAVEFile name
        let f = Frequency (fromIntegral $ waveFrameRate (waveHeader w))
            t = dual f
        return $ Sampled t (fromListS 0 . map (fromDouble . sampleToDouble . head)  . waveSamples $ w)
