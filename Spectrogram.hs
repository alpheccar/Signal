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
                v = value pl 
                col = Rgb v 0 0
            setColor col 1.0 
            pixel p
        
        mapM_ drawValue ( [ fromIntegral col :+ fromIntegral row | col <- [0..w-1], row <- [0..h-1]])
                                       
-- We display only up to sampling frequency / 2 because the spectrum is even.
-- So, it allow a zooming a the frequency that matters instead of displaying redudant information
spectrogram :: (Sample a, Show a) 
            => Signal a 
            -> Time
            -> Frequency
            -> (Int -> Int -> a -> a)
            -> Int 
            -> StyledSignal Double Double
spectrogram signal duration samplingF window overlap = 
    let winSize :: Int
        winExp = 8
        winSize = 1 `shiftL` winExp
        freqResolution = (getF samplingF) / fromIntegral winSize
        frames = frameWithWinAndOverlap winSize window overlap signal
        nbFrames :: Int
        nbFrames = (floor (getT duration * getF samplingF / fromIntegral winSize))
        theSpectrum = U.concat . takeS nbFrames . mapS (U.slice 0 (winSize `shiftR` 1)) . mapS (_spectrum winExp (1.0 / getF samplingF)) $ frames 
        thePeak = U.maximum theSpectrum
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
                                 , verticalBounds = Just (0,getF samplingF / 2.0)
                                 , prolog = prologSpect left bottom
                                 , prologRsrc = genPicture value
                                 , horizontalLabel = Just "frame"
                                 , verticalLabel = Just "Hz"
                                 }
        l=[] :: [[(Double,Double)]]
    in
    signalsWithStyle False l style 