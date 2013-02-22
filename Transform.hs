{-# LANGUAGE BangPatterns #-}
module Transform(
      spectrum 
    , fft 
    , testFFT
    , testBitReverse
    ) where 

import Data.Complex
import qualified Numeric.GSL.Fourier as F
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed((!))
import qualified Data.Vector.Unboxed.Mutable as M 
import Control.Monad.ST 
import Data.Bits(shiftL,shiftR)
import Data.Int
import Data.Bits
import Control.Monad.Primitive 
import Control.Monad(forM_,foldM_)
import Data.Word
import Data.Int
import Data.Bits
import Fixed

import Debug.Trace

debug a = trace (show a) a

spectrum :: Double -- ^ Sampling rate
      -> U.Vector Double 
      -> U.Vector Double
spectrum t d = 
    let l =  fromIntegral (U.length d)
        complexd = U.map (:+ 0.0) d
        m (x :+ y) = t*(x*x + y*y) / l
    in 
    U.map m . U.convert . F.fft  . U.convert $ complexd

w :: Floating a 
  => Int 
  -> Int 
  -> Complex a 
w k n = 
    let arg = 2.0 * pi * fromIntegral k / fromIntegral n 
    in 
    cos arg :+ sin arg 

type MArray s a = ST s (M.MVector s a)  

bitReverse :: Int -> Int -> Int
bitReverse bitSize a = fromIntegral $ br (bitSize - 1) 0 (fromIntegral a)
 where
  br :: Int -> Word64 -> Word64 -> Word64 
  br i !r !a | i < 0 = r
             | a .&. 1 == 1 = br (i-1) (r .|. (1 `shiftL` i)) (a `shiftR` 1)
             | otherwise = br (i-1) (r) (a `shiftR` 1)

bitReverseA :: (M.Unbox a) => Int -> M.MVector s a -> ST s () 
bitReverseA nb m = do 
    let l = 1 `shiftL` (nb-1)
    forM_ [0..l-1] $ \i -> do 
      let j = fromIntegral $ bitReverse nb (fromIntegral i) 
      M.swap m i j 

testBitReverse :: Int -> U.Vector Int 
testBitReverse n = 
    let v = U.fromList ([0..((1 `shiftL` n)-1)] :: [Int]) 
        r = do 
               vect <- U.thaw v 
               bitReverseA n vect 
               U.freeze vect
    in
    runST r

butterfly :: RealFloat a 
          => Complex a
          -> Complex a 
          -> Complex a 
          -> (Complex a, Complex a)
butterfly w x y = 
    let x' = x + w*y 
        y' = x - w*y 
    in
    (x',y')

instance Functor Complex where 
    fmap f (a :+ b) = f a :+ f b

_fft :: (M.Unbox a, RealFloat a, HasDoubleRepresentation a) 
     => Int -- ^ Power of 2
     -> M.MVector s (Complex a) 
     -> ST s ()
_fft n vect = do 
    bitReverseA n vect 
    let l = 1 `shiftL` n
    -- For all stages
    forM_ [0..n-1] $ \s -> do 
        let step = 1 `shiftL` s -- Step for the stage
            w1 = fmap fromDouble . cis $ -pi / fromIntegral step 
            forAllBlocks w b = do 
                forM_ (filter (<l) [b,(b + 2*step)..l]) $ \d -> do 
                    let u = d + step 
                    x <- M.read vect d 
                    y <- M.read vect u 
                    let x' = x + w * y 
                        y' = x - w * y 
                    M.write vect d x' 
                    M.write vect u y' 
                return (w*w1)
        -- Iterate butterflies. Compute butterfly nb for all blocks
        -- For instance : butterfly 1 for allb blocks
        -- Then butterfly 2 for all blocks
        foldM_ forAllBlocks (1 :+ 0) (filter (< step) [0,1..step])


fft :: (U.Unbox a, RealFloat a, HasDoubleRepresentation a)
    => Int -- ^ Power of 2
    -> U.Vector (Complex a) 
    -> U.Vector (Complex a) 
fft n v' =
    let log2 x = log x / log 2 
        n = floor (log2 (fromIntegral $ U.length v'))
        v = U.generate (1 `shiftL` n) (v' !)
        r = do 
              vect <- U.thaw v 
              _fft n vect  
              U.freeze vect
    in 
    runST r

testFFT :: Int -> U.Vector (Complex Double)
testFFT n = fft n (U.fromList . map ((:+ 0) . fromIntegral) $ ([1..(1 `shiftL` n)] :: [Int]))

testFFT1 :: Int -> U.Vector (Complex Double)
testFFT1 n = U.convert . F.fft . U.convert $  (U.fromList . map ((:+ 0) . fromIntegral) $ ([1..(1 `shiftL` n)] :: [Int]))