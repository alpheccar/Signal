{-# LANGUAGE ConstraintKinds #-}
module Spectrogram(
     spectrogram
    ) where 

import Signal 
import Windows
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed((!))
import Data.Complex
import Data.Bits
import Common
import Transform
import Graphics.PDF
import Plot
import Control.Applicative((<$>))
import Data.List(foldl1')

import Debug.Trace 

debug a = trace (show a) a 

_spectrum :: (Sample a, Num a)
          => Int 
          -> Double 
          -> U.Vector a 
          -> U.Vector Double
_spectrum n t d = 
    let l = 1 `shiftL` n :: Int
        complexd = U.map (:+ 0) . U.map toDouble $ d
        m (x :+ y) = 
            let x' = toDouble x 
                y' = toDouble y 
            in
            t*(x'*x' + y'*y') / fromIntegral l
    in 
    U.map m . fft  $ complexd

prologSpect l b Nothing _ = return () 
prologSpect l b (Just r) _ = do
    withNewContext $ do
        applyMatrix $ translate (l :+ b)
        drawXObject r

genPicture :: ((Double, Double) -> Double)
           -> Int 
           -> Int 
           -> CoordinateMapping Double Double
           -> PDF (Maybe (PDFReference RawImage))
genPicture value w h (plotToPixel, pixelToPlot) = Just <$> do
    runPixmap w h $ do
        let drawValue p = do
            let pl = pixelToPlot p
                v = log (1 + value pl) / log 2.0 
                col = Rgb v 0 0
            setColor col 1.0 
            pixel p
        
        mapM_ drawValue ( [ fromIntegral col :+ fromIntegral row | col <- [0..w-1], row <- [0..h-1]])
                                       
-- We display only up to sampling frequency / 2 because the spectrum is even.
-- So, it allow a zooming a the frequency that matters instead of displaying redudant information
spectrogram :: (Sample a, Show a) 
            => Signal Time a 
            -> Time
            -> (Int -> Int -> a -> a)
            -> Int 
            -> StyledSignal Double Double
spectrogram signal duration window overlap = 
    let samplingF = samplingRate signal
        winSize :: Int
        winExp = 8
        winSize = 1 `shiftL` winExp
        freqResolution = (getF samplingF) / fromIntegral winSize
        frames = frameWithWinAndOverlap winSize overlap window signal
        nbFrames :: Int
        nbFrames = (floor (getT duration * getF (samplingRate frames)))
        listOfSpectra = take nbFrames . getSamples . mapS (U.slice 0 (winSize `shiftR` 1)) . 
                        mapS (_spectrum winExp (1.0 / getF samplingF)) $ frames 
        theSpectrum = U.concat listOfSpectra
                      
        thePeak = U.maximum theSpectrum
        energyLocation = foldl1' (U.zipWith max)  listOfSpectra
        (lowest,_) = U.span (<= thePeak * 0.1) energyLocation 
        (highest,_) = U.span (<= thePeak * 0.1) (U.reverse energyLocation)
        startFrequency = fromIntegral (U.length lowest) / fromIntegral winSize * getF samplingF
        stopFrequency = fromIntegral (U.length energyLocation - U.length highest) / fromIntegral winSize * getF samplingF
        normalizedSpectrum = U.map (/ thePeak) theSpectrum
        totalElements = U.length normalizedSpectrum
        value (i,j) = 
            let pos = floor i*(winSize `shiftR` 1) + floor (j / freqResolution)
            in 
            if pos >= totalElements 
                then 
                    normalizedSpectrum ! (totalElements - 1)
                else 
                    normalizedSpectrum ! pos
        left = leftMargin (defaultPlotStyle)
        bottom = bottomMargin (defaultPlotStyle)
        style = defaultPlotStyle { title = Just "Spectrogram"
                                 , horizontalBounds = Just (0, fromIntegral nbFrames)
                                 , verticalBounds = Just (startFrequency,stopFrequency)
                                 , prolog = prologSpect left bottom
                                 , prologRsrc = genPicture value
                                 , horizontalLabel = Just "frame"
                                 , verticalLabel = Just "Hz"
                                 }
        l=[] :: [[(Double,Double)]]
    in
    signalsWithStyle False l style 