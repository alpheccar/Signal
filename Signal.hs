{-# LANGUAGE BangPatterns #-}
module Signal(
	Signal,
	   -- * Basic interface
    headS,                   
    tailS, 
    consS,                  

    -- * Signal transformations
    mapS,                    
    intersperseS,            

    -- * Building signals
    fromListS,
    toListS,
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
    partitionS,              


    -- * Zipping and unzipping signals
    zipS,                    
    zip3S,                   
    zip4S,
    zip5S,
    zip6S,
    zip7S,

    -- | The zipWith family generalises the zip family by zipping with the
    -- function given as the first argument, instead of a tupling function.
    zipWithS,                
    zipWith3S,               
    zipWith4S,
    

    unzipS,                  
    unzip3S,                 
    unzip4S,
    unzip5S,
    unzip6S,
    unzip7S,
 
    -- | Generic functions
    genericTakeS,
    genericDropS,
    genericSplitAtS,
    genericReplicateS,

    -- | Vector functions
    takeVectorS,
    fromVectorS


	) where 

import qualified Prelude as P
import Prelude(Int(..),Maybe(..),Bool(..),Eq(..),Integral(..),($),(.),Num(..),Ord(..),Show
              ,otherwise,Floating,Fractional,Real,Read,RealFloat,RealFrac)
import Internal
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed((!),Unbox(..))
import Data.Stream(stream,unstream,Stream(..),Step(..),L(..))
import Data.List.Stream


headS :: Signal a -> a
headS (Signal a) = head a

tailS :: Signal a -> Signal a
tailS (Signal a) = Signal (tail a)

consS :: a -> Signal a -> Signal a 
consS a (Signal l) = Signal (a:l)

mapS :: (a -> b) -> Signal a -> Signal b
mapS f (Signal a) = Signal (map f a)

intersperseS :: a -> Signal a -> Signal a
intersperseS c (Signal a) = Signal (intersperse c a)

scanlS :: (a -> b -> a) -> a -> Signal b -> Signal a
scanlS f q (Signal ls) = Signal (scanl f q ls)

scanl1S :: (a -> a -> a) -> Signal a -> Signal a
scanl1S f (Signal s) = Signal (scanl1 f s)

scanrS :: (a -> b -> b) -> b -> Signal a -> Signal b
scanrS f q0 (Signal s)    = Signal (scanr f q0 s)

scanr1S :: (a -> a -> a) -> Signal a -> Signal a
scanr1S f (Signal s) = Signal (scanr1 f s)

mapAccumLS :: (acc -> x -> (acc, y)) -> acc -> Signal x -> (acc, Signal y)
mapAccumLS f s (Signal x) = let (a,s') = mapAccumL f s x in (a,Signal s')

mapAccumRS :: (acc -> x -> (acc, y)) -> acc -> Signal x -> (acc, Signal y)
mapAccumRS f s (Signal x) = let (a,s') = mapAccumR f s x in (a, Signal s')

iterateS :: (a -> a) -> a -> Signal a
iterateS f x = Signal (iterate f x)

repeatS :: a -> Signal a
repeatS x = Signal (repeat x)

replicateS :: Int -> a -> Signal a
replicateS n0 a = Signal (replicate n0 a)

cycleS :: [a] -> Signal a
cycleS xs0 = Signal (cycle xs0)

unfoldrS :: (b -> Maybe (a, b)) -> b -> Signal a
unfoldrS f b0 = Signal (unfoldr f b0)

takeS :: Int -> Signal a -> [a]
takeS i (Signal ls) = take i ls 

dropS :: Int -> Signal a -> Signal a
dropS n (Signal ls) = Signal (drop n ls)

splitAtS :: Int -> Signal a -> ([a], Signal a)
splitAtS n (Signal ls) = let (a,b) = splitAt n ls in (a, Signal b) 
 
takeWhileS :: (a -> Bool) -> Signal a -> [a]
takeWhileS p (Signal xs0)   = takeWhile p xs0
  
dropWhileS :: (a -> Bool) -> Signal a -> [a]
dropWhileS p (Signal xs0)   = dropWhile p xs0

spanS :: (a -> Bool) -> Signal a -> ([a],[a])
spanS p (Signal xs0)        = span p xs0
  
breakS :: (a -> Bool) -> Signal a -> ([a], [a])
breakS p (Signal xs0)       = break p xs0

isPrefixOfS :: Eq a => [a] -> Signal a -> Bool
isPrefixOfS p (Signal s) = isPrefixOf p s

isInfixOfS :: Eq a => [a] -> Signal a -> Bool
isInfixOfS needle (Signal  haystack) = isInfixOf needle haystack

filterS :: (a -> Bool) -> Signal a -> Signal a
filterS p (Signal x) = Signal (filter p x)

partitionS :: (a -> Bool) -> Signal a -> (Signal a, Signal a)
partitionS p (Signal xs) = let (a,b) = partition p xs in (Signal a, Signal b)

zipS :: Signal a -> Signal b -> Signal (a, b)
zipS (Signal a) (Signal b) = Signal (zip a b)

zip3S :: Signal a -> Signal b -> Signal c -> Signal (a, b, c)
zip3S (Signal a) (Signal b) (Signal c) = Signal (zip3 a b c)

zip4S :: Signal a -> Signal b -> Signal c -> Signal d -> Signal (a, b, c, d)
zip4S (Signal a) (Signal b) (Signal c) (Signal d) = Signal (zip4 a b c d)

zip5S :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal (a, b, c, d, e)
zip5S (Signal a) (Signal b) (Signal c) (Signal d) (Signal e) = Signal (zip5 a b c d e)

zip6S :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e-> Signal f -> Signal (a, b, c, d, e, f)
zip6S (Signal a) (Signal b) (Signal c) (Signal d) (Signal e) (Signal f) = Signal (zip6 a b c d e f)

zip7S :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g -> Signal (a, b, c, d, e, f, g)
zip7S (Signal a) (Signal b) (Signal c) (Signal d) (Signal e) (Signal f) (Signal g) = Signal (zip7 a b c d e f g)

zipWithS :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipWithS f (Signal a) (Signal b) = Signal (zipWith f a b)

zipWith3S :: (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
zipWith3S z (Signal a) (Signal b) (Signal c) = Signal (zipWith3 z a b c)

zipWith4S :: (a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
zipWith4S z (Signal a) (Signal b) (Signal c) (Signal d) = Signal (zipWith4 z a b c d)

unzipS :: Signal (a,b) -> (Signal a, Signal b)
unzipS (Signal l) = let (a,b) = unzip l in (Signal a,Signal b)

unzip3S :: Signal (a, b, c) -> (Signal a, Signal b, Signal c)
unzip3S (Signal l) = let (a,b,c) = unzip3 l in (Signal a, Signal b, Signal c)

unzip4S :: Signal (a, b, c, d) -> (Signal a, Signal b, Signal c, Signal d)
unzip4S (Signal l) = let (a,b,c,d) = unzip4 l in (Signal a,Signal b,Signal c, Signal d)

unzip5S :: Signal (a, b, c, d,e) -> (Signal a, Signal b, Signal c, Signal d, Signal e)
unzip5S (Signal l) = let (a,b,c,d,e) = unzip5 l in (Signal a,Signal b,Signal c, Signal d, Signal e)

unzip6S :: Signal (a, b, c, d,e,f) -> (Signal a, Signal b, Signal c, Signal d,Signal e, Signal f)
unzip6S (Signal l) = let (a,b,c,d,e,f) = unzip6 l in (Signal a,Signal b,Signal c, Signal d,Signal e,Signal f)

unzip7S :: Signal (a, b, c, d,e,f,g) -> (Signal a, Signal b, Signal c, Signal d,Signal e, Signal f, Signal g)
unzip7S (Signal l) = let (a,b,c,d,e,f,g) = unzip7 l in (Signal a,Signal b,Signal c, Signal d, Signal e, Signal f, Signal g)

genericTakeS :: Integral i => i -> Signal a -> [a]
genericTakeS nb (Signal a) = genericTake nb a

genericDropS :: Integral i => i -> Signal a -> Signal a
genericDropS nb (Signal a) = Signal (genericDrop nb a)

genericSplitAtS :: Integral i => i -> Signal a -> ([a], [a])
genericSplitAtS i (Signal a) = genericSplitAt i a

genericReplicateS :: Integral i => i -> a -> Signal a
genericReplicateS n x = Signal (genericReplicate n x)


takeVectorS :: Unbox a => Int -> Signal a -> U.Vector a
takeVectorS nb = U.fromList . takeS nb

fromVectorS :: Unbox a => U.Vector a -> Signal a
fromVectorS v = Signal $ unstream repeatVector
 where 
 	repeatVector = Stream nextS (L 0)
 	nextS (L !i) | i < U.length v = Yield (v!i) (L (i+1))
 	             | otherwise = Skip (L 0)

fromListS :: [a] -> Signal a 
fromListS = Signal . cycle

toListS :: Signal a -> [a]
toListS (Signal a) = a
