{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module Noise(
      histogram
    , Structure(..)
    , quantizationNoise
    ) where 

import qualified Statistics.Sample.Histogram as H
import qualified Data.Vector.Unboxed as U
import Plot 
import Graphics.PDF 
import Common 
import Generators
import Signal 
import System.Random 
import Text.Printf 

nbBins = 20 

drawHist :: Int 
         -> U.Vector Double  
         -> U.Vector Double 
         -> Int 
         -> Int
         -> CoordinateMapping Double Double 
         -> Draw () 
drawHist nb v b wi hi (toPoint,_) = do
    let binWidth :: Double
        binWidth = fromIntegral wi / fromIntegral nb
        d = binWidth / 2.0
        drawBin p = do 
           fillColor (Rgb 0.8 0.8 1.0)
           let (x :+ y) = toPoint p
               r = Rectangle ((x - d) :+ 0) ((x + d) :+ y)
           fill r 
           stroke r
           return () 
    mapM_ drawBin $ (U.toList $ U.zip v b)

histogram :: (U.Unbox a, HasDoubleRepresentation a)  
          => [a] 
          -> StyledSignal Double Double
histogram l = do
    let (s,v1) = H.histogram nbBins (U.fromList . map toDouble $ l)
        v = U.map (/ (U.sum v1)) v1
        nb = U.length s
        mas = U.maximum s 
        mis = U.minimum s
        d = (mas - mis) / fromIntegral nb / 2.0
        detailed x = printf "%.2g" x
        style = defaultPlotStyle { title = Just "Histogram"
                                 , horizontalLabel = Nothing 
                                 , verticalLabel = Nothing 
                                 , horizontalBounds = Just (mis-d,mas+d)  
                                 , verticalBounds = Just (U.minimum v, U.maximum v)
                                 , epilog = drawHist nb s v
                                 , axis = False
                                 , horizontalTickRepresentation = detailed
                                 }
    discreteSignalsWithStyle (U.length s) style [] 

class Structure m where 
    doubleVersion :: (Sample i, Sample o) => m f i o -> m Double Double Double
    transferFunction :: (Sample i, Sample o) => m f i o -> Signal i -> Signal o        

quantizationNoise :: (Structure m , Random i, Sample o, Sample i) 
                  => Signal i
                  -> m p i o 
                  -> IO (Signal Double)
quantizationNoise r structure = do
    let ds = transferFunction (doubleVersion structure) (mapS toDouble r) 
        fs = mapS toDouble . transferFunction structure $ r
        quadraticError a b = (a-b)*(a-b) 
    return $ zipWithS quadraticError ds fs


