{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Transform(
      --spectrum 
      fft 
    , ifft 
    , spectrum
    , testFFT
    , testFFT1
    --, testBitReverse
    -- , bitReverse
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
import Control.Monad(forM_,foldM_,when)
import Data.Word
import Data.Int
import Data.Bits
import Fixed
import Common(Time(..),Frequency(..))
import Windows 
import Signal 
import GHC.TypeLits
import Foreign.C.Types

import Debug.Trace

isPowerOfTwo :: Word16 -> Bool
isPowerOfTwo a = (a /= 0) && ((a .&. (a-1)) == 0)

debug a = trace (show a) a

restrictVectorToPOT :: U.Unbox a => U.Vector a -> (Int,U.Vector a) 
restrictVectorToPOT v' | isPowerOfTwo (fromIntegral $ U.length v') = (n,v') 
                       | otherwise = (n,U.generate (1 `shiftL` n) (v' !))
  where log2 x = log x / log 2 
        n = floor (log2 (fromIntegral $ U.length v'))

_spectrum :: (FFT a, U.Unbox a, RealFloat a, HasDoubleRepresentation a, Num a, Show a) 
          => Time -- ^ Sampling rate
          -> (Int -> Int -> a -> a)
          -> U.Vector a 
          -> U.Vector Double
_spectrum (Time t) window d' = 
    let (n,d) = restrictVectorToPOT d'
        l = 1 `shiftL` n
        complexd = U.map (:+ 0) . U.imap (window l) $ d
        m (x :+ y) = 
            let x' = toDouble x 
                y' = toDouble y 
            in
            t*(x'*x' + y'*y') / fromIntegral l
    in 
    U.map m . fft  $ complexd

spectrum :: (FFT a, U.Unbox a, HasDoubleRepresentation a, RealFloat a, Num a,Show a) 
         => Frequency
         -> Time
         -> (Int -> Int -> a -> a) 
         -> Signal a  
         -> (Frequency, Signal Double)
spectrum f duration window signal = 
    let n = (getT duration) * (getF f) 
        s = _spectrum (Time $ 1.0 / getF f) window . takeVectorS (floor n) $ signal 
        nbSamples = U.length s 
        freqResolution = f / fromIntegral nbSamples
        freqSignal = fromVectorS s 
    in 
    (freqResolution,freqSignal)

bitReverse :: Int -> Int -> Int
bitReverse bitSize a = fromIntegral $ br (bitSize - 1) 0 (fromIntegral a)
 where
  br :: Int -> Word64 -> Word64 -> Word64 
  br i !r !a | i < 0 = r
             | a .&. 1 == 1 = br (i-1) (r .|. (1 `shiftL` i)) (a `shiftR` 1)
             | otherwise = br (i-1) (r) (a `shiftR` 1)

bitReverseA :: (M.Unbox a) => Int -> M.MVector s a -> ST s () 
bitReverseA nb m = do 
    let l = 1 `shiftL` nb
    forM_ [0..l-1] $ \i -> do 
      let j = fromIntegral $ bitReverse nb (fromIntegral i) 
      when (j > i) $ M.swap m i j 

--testBitReverse :: Int -> U.Vector Int 
--testBitReverse n = 
--    let v = U.fromList ([0..((1 `shiftL` n)-1)] :: [Int]) 
--        r = do 
--               vect <- U.thaw v 
--               bitReverseA n vect 
--               U.freeze vect
--    in
--    runST r

instance Functor Complex where 
    fmap f (a :+ b) = f a :+ f b

type FFTCore a = forall s . Int -> Int -> M.MVector s (Complex a) -> ST s ()

_fft :: (M.Unbox a, RealFloat a, HasDoubleRepresentation a) 
     => Int -- ^ Power of 2
     -> Int
     -> M.MVector s (Complex a) 
     -> ST s ()
_fft n sign vect = do 
    bitReverseA n vect 
    let l = 1 `shiftL` n
    -- For all stages
    forM_ [0..n-1] $ \s -> do 
        let step = 1 `shiftL` s -- Step for the stage
            w1 = fmap fromDouble . cis $ -pi*fromIntegral sign / fromIntegral step 
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

mac ::  (SingI n, SingI (15 + n)) 
    => Complex (Fixed Int16 15 Saturated)
    -> Complex (Fixed Int16 n Saturated) 
    -> Complex (Fixed Int16 n Saturated) 
    -> Complex (Fixed Int32 (15 + n) Saturated)
mac w y x = fmap convert x + amulc w y

msb ::  (SingI n, SingI (15 + n)) 
    => Complex (Fixed Int16 15 Saturated)
    -> Complex (Fixed Int16 n Saturated) 
    -> Complex (Fixed Int16 n Saturated) 
    -> Complex (Fixed Int32 (15 + n) Saturated)
msb w y x = fmap convert x - amulc w y


_fftFixed :: (SingI n, SingI (15 + n)) 
          => Int -- ^ Power of 2
          -> Int
          -> M.MVector s (Complex (Fixed Int16 n Saturated)) 
          -> ST s ()
_fftFixed n sign vect = do 
    bitReverseA n vect 
    let l = 1 `shiftL` n
    -- For all stages
    forM_ [0..n-1] $ \s -> do 
        let step = 1 `shiftL` s -- Step for the stage
            w1 = fmap fromDouble . cis $ -pi*fromIntegral sign / fromIntegral step :: Complex (Fixed Int16 15 Saturated)
            forAllBlocks (w :: Complex (Fixed Int16 15 Saturated) ) b = do 
                forM_ (filter (<l) [b,(b + 2*step)..l]) $ \d -> do 
                    let u = d + step 
                    x <- M.read vect d 
                    y <- M.read vect u 
                    let x' = mac w y x
                        y' = msb w y x
                    M.write vect d (fmap convert x')
                    M.write vect u (fmap convert y') 
                return (w*w1)
        -- Iterate butterflies. Compute butterfly nb for all blocks
        -- For instance : butterfly 1 for allb blocks
        -- Then butterfly 2 for all blocks
        foldM_ forAllBlocks (1 :+ 0) (filter (< step) [0,1..step])


genericfft :: (U.Unbox a, RealFloat a, HasDoubleRepresentation a)
    => Bool 
    -> FFTCore a
    -> U.Vector (Complex a) 
    -> U.Vector (Complex a) 
genericfft inverse fftCore v' =
    let (n,v) = restrictVectorToPOT v' 
        sign | inverse = -1
             | otherwise = 1
        r = do 
              vect <- U.thaw v 
              fftCore n sign vect  
              when inverse $ do 
                let l = 1 `shiftL` n 
                    scale = (fromDouble $ (1.0 / fromIntegral l)) :+ 0
                forM_ [0..l-1] $ \i -> do 
                    x <- M.read vect i 
                    M.write vect i (x*scale)
              U.freeze vect
    in 
    runST r

class FFT a where 
    fft :: (U.Unbox a, RealFloat a, HasDoubleRepresentation a)
        => U.Vector (Complex a) 
        -> U.Vector (Complex a) 
    fft = genericfft False _fft
    
    ifft :: (U.Unbox a, RealFloat a, HasDoubleRepresentation a)
        => U.Vector (Complex a) 
        -> U.Vector (Complex a) 
    ifft = genericfft True _fft

instance FFT Double 

instance (SingI n, SingI (15 + n)) => FFT (Fixed Int16 n Saturated) where
    fft = genericfft False _fftFixed
    ifft = genericfft True _fftFixed

testFFT :: Int -> Signal Double -> U.Vector (Complex Double)
testFFT n s = fft . U.map (:+ 0) . takeVectorS n $ s

testFFT1 :: Int -> Signal Double -> U.Vector (Complex Double)
testFFT1 n s = U.convert . F.fft . U.convert . U.map (:+ 0) . takeVectorS n $ s