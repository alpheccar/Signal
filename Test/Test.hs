{-# LANGUAGE BangPatterns #-}
module Test(
	  testa
	, test1
	, test2
	) where 

import Prelude hiding(zip,head,tail,map,maximum,minimum,repeat,takeWhile,take)
import Data.List.Stream
import Plot
import Graphics.PDF hiding(Orientation(..))
import Transform
import qualified Data.Vector.Unboxed as U
import Data.Stream(stream,unstream,Stream(..),Step(..),L(..))
import Data.Vector.Unboxed((!),Unbox(..))

testa :: [Int] -> [Int]
--testa = map (+11) . map (+23)
testa =  map (+11) . map (+23) 

test1 = take 10  . testa $ ([1..] :: [Int])
test2 = take 11  . (:) 26 . testa $ ([1..] :: [Int])

uniformSamples :: Double -> Double -> [Double]
uniformSamples sampling start = unstream (uniformS sampling start)
 where 
 	uniformS sampling start = Stream (nextS sampling) (L start) 
 	nextS sampling (L !s) = Yield s (L (s + sampling))

takeVector :: Unbox a => Int -> [a] -> U.Vector a
takeVector nb = U.fromList . take nb 

fromVectorS v = unstream repeatVector
 where 
 	repeatVector = Stream nextS (L 0)
 	nextS (L !i) | i < U.length v = Yield (v!i) (L (i+1))
 	             | otherwise = Skip (L 0)

duration :: Double
duration = 4.0
theTimes = uniformSamples 0.01 0.0
mySignalA = map (\t -> sin (2*pi*t)) theTimes
mySignalB = map (\t -> 0.8*cos (2*pi*t*30)*(1.0 + 0.8*cos(2*pi*t*10))) theTimes

plotStyle =  
	let lightBlue = Rgb 0.6 0.6 1.0
	    lightRed = Rgb 1.0 0.6 0.6
	in
	(defaultPlotStyle { title = Just "Temporal"
        	          , signalStyles = [defaultSignalStyle 1.0 lightBlue, defaultSignalStyle 1.0 lightRed]
        	          , verticalLabel = Just "Amplitude"
        	          })  
fftStyle = plotStyle {verticalLabel = Just "Energy", title = Just "Frequential", horizontalLabel = Just "Hz"}

pict = display $ discreteSignalsWithStyle (takeWhile (<= duration) theTimes) [mySignalA,mySignalB] plotStyle 

n = duration / 0.01
spectruma = fromVectorS . myFFT 0.01 . takeVector (floor n) $ mySignalA 
spectrumb = fromVectorS . myFFT 0.01 . takeVector (floor n) $ mySignalB
frequencies :: [Double]
frequencies = (uniformSamples (1.0 / duration) 0.0)

pictb = display $ discreteSignalsWithStyle (takeWhile (<= 100.0) frequencies) [spectruma,spectrumb] fftStyle 
