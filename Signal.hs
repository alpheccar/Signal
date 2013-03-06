{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Signal(
	Signal,
    BSignal, 
    IsSignal(..),
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
    takeWhileS,              
    dropWhileS,              
    spanS,                   
    breakS,                  

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

	) where 

import qualified Prelude as P
import Prelude(Int(..),Maybe(..),Bool(..),Eq(..),Integral(..),($),(.),Num(..),Ord(..),Show
              ,otherwise,Floating,Fractional,Real,Read,RealFloat,RealFrac)
import Internal
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed((!),Unbox(..))
import Data.Stream(stream,unstream,Stream(..),Step(..),L(..))
import Data.List.Stream
import Common(HasDoubleRepresentation(..))
import Playable
import Common 
import Viewer(play)

type Sample a = (RealFrac a, RealFloat a, HasDoubleRepresentation a, Unbox a)

instance Sample a => Playable (Time,Frequency, Signal Time a) where 
    sound (d,f,s) = (getF f, toListBS . takeS (P.floor (getT d * getF f) ). mapS toDouble $ s)

playS :: Sample a 
      => Time 
      -> Signal Time a
      -> P.IO ()
playS t s = play (sound (t,samplingRate s,s))

headS :: Signal t a -> a
headS s = so head s
{-# INLINE [0] headS #-}

tailS :: Signal t a -> Signal t a
tailS s = ss tail s
{-# INLINE [0] tailS #-}

consS :: a -> Signal t a -> Signal t a 
consS a s = ss (a:) s
{-# INLINE [0] consS #-}

mapS :: (a -> b) -> Signal t a -> Signal t b
mapS f s = ss (map f) s
{-# INLINE [0] mapS #-}

appendListS :: [a] -> Signal t a -> Signal t a
appendListS l s = ss (l ++) s 
{-# INLINE [0] appendListS #-}

concatMapS :: (a -> [b]) -> Signal t a -> Signal t b 
concatMapS f s = ss (concatMap f) s
{-# INLINE [0] concatMapS #-}

concatS :: Signal t [a] -> Signal t a 
concatS s = ss concat s
{-# INLINE [0] concatS #-}


mapBS :: (Unbox a, Unbox b) => (a -> b) -> BSignal t a -> BSignal t b
mapBS f s = bb (U.map f) s
{-# INLINE [0] mapBS #-}

imapBS :: (Unbox a, Unbox b) => (Int -> a -> b) -> BSignal t a -> BSignal t b
imapBS f s = bb (U.imap f) s
{-# INLINE [0] imapBS #-}

onSamples :: ([a] -> [b]) -> Signal t a -> Signal t b 
onSamples f = ss f 
{-# INLINE [0] onSamples #-}

intersperseS :: a -> Signal t a -> Signal t a
intersperseS c s = ss (intersperse c) s
{-# INLINE [0] intersperseS #-}

scanlS :: (a -> b -> a) -> a -> Signal t b -> Signal t a
scanlS f q ls = ss (scanl f q) ls
{-# INLINE [0] scanlS #-}

scanl1S :: (a -> a -> a) -> Signal t a -> Signal t a
scanl1S f s = ss (scanl1 f) s
{-# INLINE [0] scanl1S #-}

scanrS :: (a -> b -> b) -> b -> Signal t a -> Signal t b
scanrS f q0 s    = ss (scanr f q0) s
{-# INLINE [0] scanrS #-}

scanr1S :: (a -> a -> a) -> Signal t a -> Signal t a
scanr1S f s = ss (scanr1 f) s
{-# INLINE [0] scanr1S #-}

mapAccumLS :: (acc -> x -> (acc, y)) -> acc -> Signal t x -> (acc, Signal t y)
mapAccumLS f s (Signal t x) = let (a,s') = mapAccumL f s x in (a,Signal t s')
{-# INLINE [0] mapAccumLS #-}

mapAccumRS :: (acc -> x -> (acc, y)) -> acc -> Signal t x -> (acc, Signal t y)
mapAccumRS f s (Signal t x) = let (a,s') = mapAccumR f s x in (a, Signal t s')
{-# INLINE [0] mapAccumRS #-}

iterateS :: t -> (a -> a) -> a -> Signal t a
iterateS t f x = os t (iterate f x)
{-# INLINE [0] iterateS #-}

repeatS :: t -> a -> Signal t a
repeatS r x = os r (repeat x)
{-# INLINE [0] repeatS #-}

replicateS :: t -> Int -> a -> Signal t a
replicateS r n0 a = os r (replicate n0 a)
{-# INLINE [0] replicateS #-}

cycleS :: t -> [a] -> Signal t a
cycleS r xs0 = os r (cycle xs0)
{-# INLINE [0] cycleS #-}

unfoldrS :: t -> (b -> Maybe (a, b)) -> b -> Signal t a
unfoldrS r f b0 = os r (unfoldr f b0)
{-# INLINE [0] unfoldrS #-}

takeS :: Unbox a => Int -> Signal t a -> BSignal t a
takeS i ls = sb (U.fromList . take i) ls
{-# INLINE [0] takeS #-}

dropS :: Int -> Signal t a -> Signal t a
dropS n ls = ss (drop n) ls
{-# INLINE [0] dropS #-}

splitAtS :: Int -> Signal t a -> ([a], Signal t a)
splitAtS n (Signal r ls) = let (a,b) = splitAt n ls in (a, Signal r b) 
{-# INLINE [0] splitAtS #-}

takeWhileS :: Unbox a => (a -> Bool) -> Signal t a -> BSignal t a
takeWhileS p xs0   = sb (U.fromList . takeWhile p) xs0
{-# INLINE [0] takeWhileS #-}

dropWhileS :: Unbox a => (a -> Bool) -> Signal t a -> BSignal t a
dropWhileS p xs0   = sb (U.fromList . dropWhile p) xs0
{-# INLINE [0] dropWhileS #-}

spanS :: (a -> Bool) -> Signal t a -> ([a],Signal t a)
spanS p (Signal r xs0)        = let (b,s) = span p xs0
                                in (b, Signal r s)
{-# INLINE [0] spanS #-}

breakS :: (a -> Bool) -> Signal t a -> ([a], Signal t a)
breakS p (Signal r xs0)     = let (b,s) = break p xs0
                              in (b, Signal r s)
{-# INLINE [0] breakS #-}

isPrefixOfS :: (Eq a, Unbox a) => BSignal t a -> Signal t a -> Bool
isPrefixOfS p s = so (isPrefixOf (toListBS p))  s
{-# INLINE [0] isPrefixOfS #-}

isInfixOfS :: (Eq a, Unbox a) => BSignal t a -> Signal t a -> Bool
isInfixOfS needle haystack = so (isInfixOf (toListBS needle)) haystack
{-# INLINE [0] isInfixOfS #-}

filterS :: (a -> Bool) -> Signal t a -> Signal t a
filterS p x = ss (filter p) x
{-# INLINE [0] filterS #-}

zipS :: Signal t a -> Signal t b -> Signal t (a, b)
zipS (Signal r a) (Signal _ b) = Signal r (zip a b)
{-# INLINE [0] zipS #-}

zipWithS :: (a -> b -> c) -> Signal t a -> Signal t b -> Signal t c
zipWithS f (Signal r a) (Signal _ b) = Signal r (zipWith f a b)
{-# INLINE [0] zipWithS #-}

unzipS :: Signal t (a,b) -> (Signal t a, Signal t b)
unzipS (Signal r l) = let (a,b) = unzip l in (Signal r a,Signal r b)
{-# INLINE [0] unzipS #-}

genericTakeS :: (Unbox a, Integral i) => i -> Signal t a -> BSignal t a
genericTakeS nb s = sb (U.fromList . genericTake nb) s
{-# INLINE [0] genericTakeS #-}

genericDropS :: (Integral i) => i -> Signal t a -> Signal t a
genericDropS nb s = ss (genericDrop nb) s
{-# INLINE [0] genericDropS #-}

genericSplitAtS :: (Integral i) => i -> Signal t a -> ([a], Signal t a)
genericSplitAtS i (Signal r a) = let (b,s) = genericSplitAt i a
                                 in (b, Signal r s)
{-# INLINE [0] genericSplitAtS #-}

genericReplicateS :: Integral i => t -> i -> a -> Signal t a
genericReplicateS r n x = os r (genericReplicate n x)
{-# INLINE [0] genericReplicateS #-}


fromVectorS :: Unbox a => t -> U.Vector a -> Signal t a
fromVectorS r v = Signal r $ unstream repeatVector
 where 
 	repeatVector = Stream nextS (L 0)
 	nextS (L !i) | i < U.length v = Yield (v!i) (L (i+1))
 	             | otherwise = Skip (L 0)
{-# INLINE [0] fromVectorS #-}

fromVectorBS :: Unbox a => t -> U.Vector a -> BSignal t a 
fromVectorBS r v = BSignal r v
{-# INLINE [0] fromVectorBS #-}

fromListS :: t -> [a] -> Signal t a 
fromListS r = Signal r . cycle
{-# INLINE [0] fromListS #-}

fromListBS :: Unbox a => t -> [a] -> BSignal t a 
fromListBS r = BSignal r . U.fromList
{-# INLINE [0] fromListBS #-}

toListBS :: Unbox a => BSignal t a -> [a]
toListBS (BSignal _ l) = U.toList l
{-# INLINE [0] toListBS #-}

fromBS :: Unbox a => BSignal t a -> Signal t a 
fromBS s = bs (cycle . U.toList) s
{-# INLINE [0] fromBS #-}

appendBSToS :: Unbox a => BSignal t a -> Signal t a -> Signal t a 
appendBSToS b s = ss (\t -> toListBS b ++ t) s

toVectorBS :: Unbox a => BSignal t a -> U.Vector a 
toVectorBS (BSignal _ v) = v
{-# INLINE [0] toVectorBS #-}

appendBS :: Unbox a => BSignal t a -> BSignal t a -> BSignal t a 
appendBS a b = bb ((toVectorBS a) U.++) b
{-# INLINE [0] appendBS #-}
