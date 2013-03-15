{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Trace(
      clearTrace
    , traceNames
    , CanBeTraced
    , traceValues
    , trace
    , traceSample
    , forceSignal
    ) where 

import Common(HasDoubleRepresentation(..))
import System.IO.Unsafe
import qualified Data.Map as M
import Data.IORef
import Control.Applicative((<$>))
import Signal(Signal,mapS,fromListS,takeS)
import Fixed(Resolution(..))
import Statistics.Sample.KernelDensity 
import qualified Data.Vector.Unboxed as U 
import Viewer
import Plot
import Graphics.PDF
import Displayable
import Internal
import Control.DeepSeq
import Text.Printf
import qualified Graphics.PDF as PDF


import qualified Debug.Trace as T
debug a = T.trace (show a) a

data CanBeTraced = forall a. (HasDoubleRepresentation a,Resolution a) => CBT !a

instance HasDoubleRepresentation CanBeTraced where 
    toDouble (CBT a) = toDouble a
    fromDouble = error "Can't create a CanBeTraced from a double"

instance Resolution CanBeTraced where 
    smallestValue (CBT a) = CBT (smallestValue a)
    maxValue (CBT a) = CBT (maxValue a)
    minValue (CBT a) = CBT (minValue a)
    signedFormat (CBT a) = signedFormat a
    bitWidth (CBT a) = bitWidth a

{-# NOINLINE globalTrace #-}
globalTrace  :: IORef (M.Map String [CanBeTraced])
globalTrace = unsafePerformIO $ newIORef (M.empty)

clearTrace :: IO ()
clearTrace = writeIORef globalTrace M.empty

traceNames :: IO [String]
traceNames = M.keys <$> (readIORef  globalTrace) 

getTrace :: String -> IO (Maybe [CanBeTraced])
getTrace name = M.lookup name <$> (readIORef globalTrace)

removeZeroAndLog :: [Double] -> [Double] -> ([Double],[Double])
removeZeroAndLog a b = unzip . map (\(t,x) -> (t, log x)) . filter ((> 0) . snd ) . zip a $ b 

traceValues :: String -> IO ()
traceValues s = do 
    let statStyle = LabelStyle 10 Centered PDF.S
    r <- getTrace s
    case r of 
        Nothing -> return ()
        Just l -> do
            let h = head l 
                sm = toDouble (smallestValue h)
                mi = toDouble (minValue h)
                ma = toDouble (maxValue h) 
                triplePartition ta tb !la !lb !lc [] = (la,lb,lc)
                triplePartition ta tb !la !lb !lc (a:h) | ta a = triplePartition ta tb (a:la) lb lc h
                                                        | tb a = triplePartition ta tb la (a:lb) lc h
                                                        | otherwise = triplePartition ta tb la lb (a:lc) h 
                (negatives, nulls, positives) = triplePartition (<0) (==0) [] [] [] (map toDouble l)
                positiveSat | null positives = 0 :: Double
                            | otherwise = 100*fromIntegral (length (filter (== ma) positives)) / fromIntegral (length positives)
                negativeSat | null negatives = 0 :: Double
                            | otherwise = 100*fromIntegral (length (filter (== mi) negatives)) / fromIntegral (length negatives)
                nullValues | null nulls = 0 :: Double
                           | otherwise = 100*fromIntegral (length nulls) / fromIntegral (length l)
                drawHist bmi bma l h label theTitle = do
                            let targetLine wi hi (ptF,_) = do 
                                    withNewContext $ do
                                        strokeColor $ Rgb 1.0 0.8 0.8
                                        let start = (log bma) - (log bmi)
                                            (xa :+ ya) = ptF ((log bmi),(-start))
                                            (xb :+ yb) = ptF ((log bma),0)
                                        stroke $ Line  xa ya xb yb
                                tickv mav mbv = 
                                    map (\t -> (fromIntegral t)/(fromIntegral 10)*(mbv-mav) + mav) ([0..10] :: [Int])
                                tickh _ _ = 
                                    let mav = log bmi 
                                        mbv = log bma
                                    in
                                    map (\t -> (fromIntegral t)/(fromIntegral 10)*(mbv-mav) + mav) ([0..10] :: [Int])
                                plotStyle =  (defaultPlotStyle { title = theTitle
                                                               , verticalLabel = Just "Log density"
                                                               , horizontalLabel = Just $ "Log amplitude (" ++ label ++ ")"
                                                               , horizontalTickValues = tickh
                                                               , verticalTickValues = tickv
                                                               , epilog = targetLine
                                                               , axis = False
                                                               , horizontalBounds = Just (log bmi, log bma)
                                                               })  
                            if null l
                                then do 
                                    (\_ -> \_ -> return (R Nothing Nothing), \_ -> \_ -> \_ -> return ())
                                else do
                                    let (t,p) = kde_ 32 (log bmi) (log bma) (U.fromList (map log l))
                                        lt' = U.toList t 
                                        lp' = let templ = U.toList p
                                                  s = sum templ 
                                              in 
                                              map (/ s) templ
                                        (lt,lp) = removeZeroAndLog lt' lp'
                                    let r = (log bma - log bmi) / fromIntegral (length lp)
                                    drawing $ discreteSignalsWithStyle (length lt) plotStyle (AS $ fromListS 0 lt)  [ AS (fromListS 0 lp)]
            let (wi,he) = (\(x,y) -> (fromIntegral x, fromIntegral y)) $ viewerSize 
            if (signedFormat h) 
                then do
                    let posD = drawHist sm ma positives (he / 2 - 10) "pos" (Just "Log density of log amplitude")
                        negD = drawHist sm (-mi) (map negate negatives) (he / 2 - 10) "neg" Nothing
                        pict = Pair posD negD $ \pos -> \neg -> do
                            withNewContext $ do
                                applyMatrix $ translate (0 :+ (he/2 + 10))
                                pos
                            withNewContext $ do
                                neg
                            drawStringLabel statStyle ((printf "negative max: %2.3f %%" negativeSat) :: String)
                                             (leftMargin defaultPlotStyle + (wi / 4.0))  (he / 2 - 10) 100 20
                            drawStringLabel statStyle ((printf "nulls: %2.3f %%" nullValues) :: String)
                                             (leftMargin defaultPlotStyle + (2.0*wi/4.0)) (he/2 - 10) 100 20
                            drawStringLabel statStyle ((printf "positive max: %2.3f %%" positiveSat) :: String)
                                             (leftMargin defaultPlotStyle + (3.0*wi/4.0)) (he/2 - 10) 100 20
                    display pict
                else do
                        let dD = drawHist sm ma positives (he - 40) "unsigned" (Just "Log density of log amplitude")
                            pict = Mono dD $ \d -> do
                                withNewContext $ do
                                    applyMatrix $ translate (0 :+ 40)
                                    d
                                drawStringLabel statStyle ((printf "nulls: %2.3f %%" nullValues) :: String)
                                                    (leftMargin defaultPlotStyle + wi / 3.0) 10 100 20
                                drawStringLabel statStyle ((printf "positive max: %2.3f %%" positiveSat) :: String)
                                                    (leftMargin defaultPlotStyle + (2*wi/3.0)) 10 100 20
                        display pict

{-# NOINLINE traceSample #-}
traceSample :: (HasDoubleRepresentation a, Resolution a) => String -> a -> a
traceSample s a = unsafePerformIO $ do 
    nv <- M.insertWith' (++) s [CBT a] <$> (readIORef globalTrace)
    writeIORef globalTrace nv
    return a

{-# NOINLINE trace #-}
trace :: (HasDoubleRepresentation a, Resolution a)
      => String 
      -> Signal a 
      -> Signal a 
trace s = mapS (traceSample s)

{-# NOINLINE myTake #-}
myTake :: NFData a => Int -> [a] -> [a]
myTake 0 l = []
myTake i (h:l) = h:myTake (i-1) l
myTake _ [] = []

{-# NOINLINE forceSignal #-}
forceSignal :: (NFData a, Show a) 
            => Int 
            -> Signal a 
            -> IO ()
forceSignal i (Signal s) = do
    let l = myTake i s
    l `deepseq` return ()