{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Signal(
        Signal,
    BSignal, 
    getSamples,
    duration,
    nbSamples,
    Sample(..),
    DualVal(..),
    samplingRate,
           -- * Basic interface
    headS,                   
    tailS, 
    consS,  
    appendBS,                

    -- * Signal transformations
    mapS, 
    mapBS, 
    imapBS,
    concatMapS,
    concatS,   
    intersperseS,  
    appendListS, 
    onSamples,         

    -- * Building signals
    fromListS,
    fromListBS,
    toListBS,
    -- ** Scans
    scanlS,                  
    scanl1S,                 
    scanrS,                  
    scanr1S,                 

    -- ** Accumulating maps
    mapAccumLS,              
    mapAccumRS,              

    -- ** Infinite signals
    iterateS,                
    repeatS,                 
    replicateS,              
    cycleS,                  

    -- ** Unfolding
    unfoldrS,                

    -- * Sublists
    -- ** Extracting sublists
    takeS,                   
    dropS,                   
    splitAtS,   
    splitAtVectorS,                             
    takeWhileS,              
    dropWhileS,              
    spanS,                   
    breakS,  
    takeVectorS,   

    -- * Predicates
    isPrefixOfS,             
    isInfixOfS,              

   
    -- ** Searching with a predicate
    filterS,                 


    -- * Zipping and unzipping signals
    zipS,                    

    -- | The zipWith family generalises the zip family by zipping with the
    -- function given as the first argument, instead of a tupling function.
    zipWithS,                

    unzipS,                  
 
    -- | Generic functions
    genericTakeS,
    genericDropS,
    genericTakeVectorS,
    genericDropVectorS,
    genericSplitAtS,
    genericReplicateS,

    -- | Vector functions
    fromVectorS,
    fromVectorBS,
    toVectorBS,

    -- | Utility
    playS,

    -- | Conversion
    fromBS,
    appendBSToS,
    cs,
    constSignal,

    Sampled(..),
    HasPeriod(..),
    HasSamples(..)
        ) where

import qualified Prelude as P
import Prelude(Int(..),Maybe(..),Bool(..),Eq(..),Integral(..),($),(.),Num(..),Ord(..),Show
              ,otherwise,Floating,Fractional,Real,Read,RealFloat,RealFrac)

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed((!),Unbox(..))

import Data.Stream(stream,unstream,Stream(..),Step(..),L(..))
import Data.List.Stream

import HaskellViewer.Playable
import HaskellViewer.Viewer(play)

import Signal.Common
import Signal.Common(HasDoubleRepresentation(..))
import Signal.Internal

type Sample a = (RealFrac a, RealFloat a, HasDoubleRepresentation a, Unbox a)

instance Sample a => Playable (Time,Frequency, Signal a) where 
    sound (d,f,s) = (getF f, take (P.floor (getT d * getF f) ) . getSamples . mapS toDouble $ s)

playS :: Sample a 
      => Time 
      -> Sampled Time a
      -> P.IO ()
playS t s = play (sound (t,rate s,getSignal s))

headS :: Signal a -> a
headS (Signal s) = head s
{-# INLINE [0] headS #-}

tailS :: Signal a -> Signal a
tailS (Signal s) = Signal $ tail s
{-# INLINE [0] tailS #-}

consS :: a -> Signal a -> Signal a 
consS a (Signal s) = Signal $ (a:) s
{-# INLINE [0] consS #-}

mapS :: (a -> b) -> Signal a -> Signal b
mapS f (Signal s) = Signal $ map f s
{-# INLINE [0] mapS #-}

appendListS :: [a] -> Signal a -> Signal a
appendListS l (Signal s) = Signal (l ++ s)

concatMapS :: (a -> [b]) -> Signal a -> Signal b 
concatMapS f (Signal s) = Signal $ concatMap f s
{-# INLINE [0] concatMapS #-}

concatS :: Signal [a] -> Signal a 
concatS (Signal s) = Signal $  concat s
{-# INLINE [0] concatS #-}


mapBS :: (Unbox a, Unbox b) => (a -> b) -> BSignal a -> BSignal b
mapBS f (BSignal s) = BSignal $ U.map f s
{-# INLINE [0] mapBS #-}

imapBS :: (Unbox a, Unbox b) => (Int -> a -> b) -> BSignal a -> BSignal b
imapBS f (BSignal s) = BSignal $ U.imap f s
{-# INLINE [0] imapBS #-}

onSamples :: ([a] -> [b]) -> Signal a -> Signal b 
onSamples f (Signal s) = Signal (f s) 
{-# INLINE [0] onSamples #-}

intersperseS :: a -> Signal a -> Signal a
intersperseS c (Signal s) = Signal $ intersperse c s
{-# INLINE [0] intersperseS #-}

scanlS :: (a -> b -> a) -> a -> Signal b -> Signal a
scanlS f q (Signal ls) = Signal $  scanl f q ls
{-# INLINE [0] scanlS #-}

scanl1S :: (a -> a -> a) -> Signal a -> Signal a
scanl1S f (Signal s) = Signal $ scanl1 f s
{-# INLINE [0] scanl1S #-}

scanrS :: (a -> b -> b) -> b -> Signal a -> Signal b
scanrS f q0 (Signal s)    = Signal $ scanr f q0 s
{-# INLINE [0] scanrS #-}

scanr1S :: (a -> a -> a) -> Signal a -> Signal a
scanr1S f (Signal s) = Signal $ scanr1 f s
{-# INLINE [0] scanr1S #-}

mapAccumLS :: (acc -> x -> (acc, y)) -> acc -> Signal x -> (acc, Signal y)
mapAccumLS f s (Signal x) = let (a,s') = mapAccumL f s x in (a,Signal s')
{-# INLINE [0] mapAccumLS #-}

mapAccumRS :: (acc -> x -> (acc, y)) -> acc -> Signal x -> (acc, Signal y)
mapAccumRS f s (Signal x) = let (a,s') = mapAccumR f s x in (a, Signal s')
{-# INLINE [0] mapAccumRS #-}

iterateS :: (a -> a) -> a -> Signal a
iterateS f x =  Signal (iterate f x)
{-# INLINE [0] iterateS #-}

repeatS :: a -> Signal a
repeatS x = Signal (repeat x)
{-# INLINE [0] repeatS #-}

replicateS :: Int -> a -> Signal a
replicateS n0 a = Signal (replicate n0 a)
{-# INLINE [0] replicateS #-}

cycleS :: [a] -> Signal a
cycleS xs0 = Signal (cycle xs0)
{-# INLINE [0] cycleS #-}

unfoldrS :: (b -> Maybe (a, b)) -> b -> Signal a
unfoldrS f b0 = Signal (unfoldr f b0)
{-# INLINE [0] unfoldrS #-}

takeVectorS :: Unbox a => Int -> Signal a -> BSignal a
takeVectorS i (Signal ls) = BSignal $ U.fromList . take i $ ls
{-# INLINE [0] takeVectorS #-}

takeS :: Int -> Signal a -> [a]
takeS nb (Signal l) = take nb l
{-# INLINE [0] takeS #-}

dropS :: Int -> Signal a -> Signal a
dropS n (Signal ls) = Signal $ drop n ls
{-# INLINE [0] dropS #-}

splitAtS :: Int -> Signal a -> ([a], Signal a)
splitAtS n (Signal ls) = let (a,b) = splitAt n ls in (a, Signal b) 

splitAtVectorS :: Unbox a => Int -> Signal a -> (BSignal a, Signal a)
splitAtVectorS n (Signal ls) = let (a,b) = splitAt n ls in (BSignal (U.fromList a), Signal b) 
{-# INLINE [0] splitAtVectorS #-}

takeWhileVectorS :: Unbox a => (a -> Bool) -> Signal a -> BSignal a
takeWhileVectorS p (Signal xs0)   = BSignal $ U.fromList . takeWhile p $  xs0
{-# INLINE [0] takeWhileVectorS #-}

takeWhileS :: (a -> Bool) -> Signal a -> [a]
takeWhileS p = takeWhile p . getSamples 
{-# INLINE [0] takeWhileS #-}


dropWhileVectorS :: Unbox a => (a -> Bool) -> Signal a -> BSignal a
dropWhileVectorS p (Signal xs0)   = BSignal $  U.fromList . dropWhile p $  xs0
{-# INLINE [0] dropWhileVectorS #-}

dropWhileS :: (a -> Bool) -> Signal a -> [a]
dropWhileS p = dropWhile p . getSamples 
{-# INLINE [0] dropWhileS #-}

spanS :: (a -> Bool) -> Signal a -> ([a],Signal a)
spanS p (Signal xs0)        = let (b,s) = span p xs0
                              in (b, Signal s)
{-# INLINE [0] spanS #-}

breakS :: (a -> Bool) -> Signal a -> ([a], Signal a)
breakS p (Signal xs0)     = let (b,s) = break p xs0
                            in (b, Signal s)
{-# INLINE [0] breakS #-}

isPrefixOfS :: (Eq a, Unbox a) => BSignal a -> Signal a -> Bool
isPrefixOfS (BSignal p) (Signal s) = isPrefixOf (U.toList p) s
{-# INLINE [0] isPrefixOfS #-}

isInfixOfS :: (Eq a, Unbox a) => BSignal a -> Signal a -> Bool
isInfixOfS (BSignal needle) (Signal haystack) =  isInfixOf  (U.toList needle) haystack
{-# INLINE [0] isInfixOfS #-}

filterS :: (a -> Bool) -> Signal a -> Signal a
filterS p (Signal x) = Signal $  (filter p) x
{-# INLINE [0] filterS #-}

zipS :: Signal a -> Signal b -> Signal (a, b)
zipS (Signal a) (Signal b) = Signal (zip a b)
{-# INLINE [0] zipS #-}

zipWithS :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipWithS f (Signal a) (Signal b) = Signal (zipWith f a b)
{-# INLINE [0] zipWithS #-}

unzipS :: Signal (a,b) -> (Signal a, Signal b)
unzipS (Signal l) = let (a,b) = unzip l in (Signal a,Signal b)
{-# INLINE [0] unzipS #-}

genericTakeVectorS :: (Unbox a, Integral i) => i -> Signal a -> BSignal a
genericTakeVectorS nb (Signal s) = BSignal $ U.fromList . genericTake nb $ s
{-# INLINE [0] genericTakeVectorS #-}

genericTakeS ::  (Integral i) => i -> Signal a -> [a]
genericTakeS nb = genericTake nb . getSamples 
{-# INLINE [0] genericTakeS #-}

genericDropVectorS :: (Integral i) => i -> Signal a -> Signal a
genericDropVectorS nb (Signal s) = Signal $ genericDrop nb s
{-# INLINE [0] genericDropVectorS #-}

genericDropS ::  (Integral i) => i -> Signal a -> [a]
genericDropS nb = genericDrop nb . getSamples 
{-# INLINE [0] genericDropS #-}

genericSplitAtS :: (Integral i) => i -> Signal a -> ([a], Signal a)
genericSplitAtS i (Signal a) = let (b,s) = genericSplitAt i a
                               in (b, Signal s)
{-# INLINE [0] genericSplitAtS #-}

genericReplicateS :: Integral i => i -> a -> Signal a
genericReplicateS n x = Signal (genericReplicate n x)
{-# INLINE [0] genericReplicateS #-}


fromVectorS :: Unbox a => a -> U.Vector a -> Signal a
fromVectorS d v = Signal $ unstream (repeatVector d)
 where 
        repeatVector d = Stream (nextS d) (L 0)
        nextS d l@(L !i) | i < U.length v = Yield (v!i) (L (i+1))
                         | otherwise = Yield d l
{-# INLINE [0] fromVectorS #-}

fromVectorBS :: Unbox a => U.Vector a -> BSignal a 
fromVectorBS v = BSignal v
{-# INLINE [0] fromVectorBS #-}

fromListS :: a -> [a] -> Signal a 
fromListS d l = Signal ((l ++)  . (repeat d ++) $ [])
{-# INLINE [0] fromListS #-}

fromListBS :: Unbox a => [a] -> BSignal a 
fromListBS = BSignal . U.fromList
{-# INLINE [0] fromListBS #-}

toListBS :: Unbox a => BSignal a -> [a]
toListBS (BSignal l) = U.toList l
{-# INLINE [0] toListBS #-}

fromBS :: Unbox a => a -> BSignal a -> Signal a 
fromBS a (BSignal s) = Signal $  ((\t -> ((U.toList t) ++) . (repeat a ++) $ []) )s
{-# INLINE [0] fromBS #-}

appendBSToS :: Unbox a => BSignal a -> Signal a -> Signal a 
appendBSToS (BSignal b) (Signal s) = Signal $  U.toList b ++ s

toVectorBS :: Unbox a => BSignal a -> U.Vector a 
toVectorBS (BSignal v) = v
{-# INLINE [0] toVectorBS #-}

appendBS :: Unbox a => BSignal a -> BSignal a -> BSignal a 
appendBS (BSignal a) (BSignal b) = BSignal $  (a U.++ b)
{-# INLINE [0] appendBS #-}
