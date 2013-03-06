{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Transform(
      --spectrum 
      spectrum
    , testFFT
    , testFFT1
    , FFT(..)
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
import TypeAddition

--import Debug.Trace

isPowerOfTwo :: Word16 -> Bool
isPowerOfTwo a = (a /= 0) && ((a .&. (a-1)) == 0)

--debug a = trace (show a) a

restrictVectorToPOT :: U.Unbox a => U.Vector a -> (Int,U.Vector a) 
restrictVectorToPOT v' | isPowerOfTwo (fromIntegral $ U.length v') = (n,v') 
                       | otherwise = (n,U.generate (1 `shiftL` n) (v' !))
  where log2 x = log x / log 2 
        n = floor (log2 (fromIntegral $ U.length v'))

_spectrum :: (FFT a, Sample a) 
          => (Int -> Int -> a -> a)
          -> BSignal Time a 
          -> U.Vector Double
_spectrum window d' = 
    let Time t = samplingPeriod d'
        vd' = toVectorBS d'
        (n,d) = restrictVectorToPOT vd'
        l = 1 `shiftL` n
        complexd = U.map (:+ 0) . U.imap (window l) $ d
        m (x :+ y) = 
            let x' = toDouble x 
                y' = toDouble y 
            in
            t*(x'*x' + y'*y') / fromIntegral l
    in 
    U.map m . fft  $ complexd

spectrum :: (FFT a, Sample a) 
         => Time
         -> (Int -> Int -> a -> a) 
         -> Signal Time a  
         -> Signal Frequency Double
spectrum duration window signal = 
    let f = (samplingRate signal)
        n = (getT duration) * (getF f) 
        s = _spectrum window . takeS (floor n) $ signal 
        nbSamples = U.length s 
        freqResolution = f / fromIntegral nbSamples
        freqSignal = fromVectorS freqResolution s 
    in 
    freqSignal

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

_fft :: Sample a 
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

mac ::  (SingI n, SingI r, SingI (15 + n)) 
    => Complex (Fixed Int16 15 Sat r)
    -> Complex (Fixed Int16 n Sat r) 
    -> Complex (Fixed Int16 n Sat r) 
    -> Complex (Fixed Int32 (15 + n) Sat r)
mac w y x = fmap convert x + amulc w y

msb ::  (SingI n, SingI r, SingI (15 + n)) 
    => Complex (Fixed Int16 15 Sat r)
    -> Complex (Fixed Int16 n Sat r) 
    -> Complex (Fixed Int16 n Sat r) 
    -> Complex (Fixed Int32 (15 + n) Sat r)
msb w y x = fmap convert x - amulc w y


_fftFixed :: (SingI n, SingI r, SingI (15 + n)) 
          => Int -- ^ Power of 2
          -> Int
          -> M.MVector s (Complex (Fixed Int16 n Sat r)) 
          -> ST s ()
_fftFixed n sign vect = do 
    bitReverseA n vect 
    let l = 1 `shiftL` n
    -- For all stages
    forM_ [0..n-1] $ \s -> do 
        let step = 1 `shiftL` s -- Step for the stage
            w1 = fmap fromDouble . cis $ -pi*fromIntegral sign / fromIntegral step :: Complex (Fixed Int16 15 Sat r)
            forAllBlocks (w :: Complex (Fixed Int16 15 Sat r) ) b = do 
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


genericfft :: Sample a
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
    fft :: Sample a
        => U.Vector (Complex a) 
        -> U.Vector (Complex a) 
    fft = genericfft False _fft
    
    ifft :: Sample a
         => U.Vector (Complex a) 
         -> U.Vector (Complex a) 
    ifft = genericfft True _fft

instance FFT Double 

instance (SingI n, SingI r, SingI (15 + n)) => FFT (Fixed Int16 n Sat r) where
    fft = genericfft False _fftFixed
    ifft = genericfft True _fftFixed

testFFT :: (FFT a,Sample a) => Int -> Signal Time a -> U.Vector (Complex Double)
testFFT n s = U.map (fmap toDouble) . fft . toVectorBS . mapBS (:+ 0) . takeS n $ s

testFFT1 :: Sample a => Int -> Signal Time a -> U.Vector (Complex Double)
testFFT1 n s = U.convert . F.fft . U.convert . toVectorBS . mapBS ((:+ 0) . toDouble) . takeS n $ s