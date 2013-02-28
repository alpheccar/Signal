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
    , forceSignal
    ) where 

import Common(HasDoubleRepresentation(..))
import System.IO.Unsafe
import qualified Data.Map as M
import Data.IORef
import Control.Applicative((<$>))
import Signal(Signal,mapS,fromListS,takeS)
import Fixed(Resolution(..))
import Statistics.Sample.Histogram 
import qualified Data.Vector.Unboxed as U 
import Viewer
import Plot
import Graphics.PDF
import Displayable
import Internal
import Control.DeepSeq

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
                positiveSat | null positives = 0 
                            | otherwise = fromIntegral (length (filter (== ma) positives)) / fromIntegral (length positives)
                negativeSat | null negatives = 0 
                            | otherwise = fromIntegral (length (filter (== mi) negatives)) / fromIntegral (length negatives)
                nullValues | null nulls = 0 
                           | otherwise = fromIntegral (length nulls) / fromIntegral (length l)
                drawHist bmi bma l h label theTitle = do
                            let targetLine ptF = do 
                                    withNewContext $ do
                                        strokeColor $ Rgb 1.0 0.8 0.8
                                        let start = (log bma) - (log bmi)
                                            (xa :+ ya) = ptF ((log bmi),(-start))
                                            (xb :+ yb) = ptF ((log bma),0)
                                        stroke $ Line  xa ya xb yb
                                (t,p) = histogram 16  (U.fromList (map log l))
                                lt' = U.toList t 
                                lp' = let templ = U.toList p
                                          s = sum templ 
                                      in 
                                      map (/ s) templ
                                (lt,lp) = removeZeroAndLog lt' lp'
                                tickv _ _ = 
                                    let start = (log bma) - (log bmi)
                                        mav = -start
                                        mbv = 0.0
                                    in
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
                                                               , defaultHeight = h
                                                               , epilog = targetLine
                                                               , axis = False
                                                               })  
                            drawing $ discreteSignalsWithStyle lt plotStyle [ AS (fromListS lp)]
            if (signedFormat h) 
                then do
                    let pos = drawHist sm ma positives 190 "pos" (Just "Log density of log amplitude")
                        neg = drawHist sm (-mi) (map negate negatives) 190 "neg" Nothing
                        pict = do
                            withNewContext $ do
                                applyMatrix $ translate (0 :+ 210)
                                pos 
                            withNewContext $ do
                                neg 
                    display pict
                else do
                    display $ drawHist sm ma positives 400 "unsigned" (Just "Log density of log amplitude")

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

forceSignal :: NFData a 
            => Int 
            -> Signal a 
            -> IO (Int)
forceSignal i (Signal s) = 
    let l = take i s 
    in 
    return $ l `deepseq` (length l)