{-# LANGUAGE ConstraintKinds #-}
module Windows(
      hann
    , noWindow
    , hamming
    , tukey
    , cosine
    , lanczos
    , triangular
    , cossq
    , frameWithWinAndOverlap
    , flattenWithOverlapS
    ) where 

import Prelude hiding(splitAt,(++),concat,zipWith,concatMap,null,head,take)
import Common
import Signal
import Internal
import Data.List.Stream
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed((!),Unbox(..))

-- This version is not lazy enough and we get an infinite loop
-- (I don't yet understand why)
--frameWithWinAndOverlap :: (Unbox a) 
--                       => Int
--                       -> (Int -> Int -> a -> a) 
--                       -> Int 
--                       -> Signal t a 
--                       -> Signal t (U.Vector a) 
--frameWithWinAndOverlap winSize winF o s = 
--    let frame z = 
--          let (ws,r1) = splitAtS (winSize - o) z 
--              (os,r2) = splitAtS o r1 
--              h = toVectorBS $ imapBS (winF winSize) (appendBS ws os)
--          in 
--          consS h (frame r1)
--    in 
--    frame s

frameWithWinAndOverlap :: (Unbox a) 
                       => Int
                       -> (Int -> Int -> a -> a) 
                       -> Int 
                       -> Signal t a 
                       -> Signal t (U.Vector a) 
frameWithWinAndOverlap winSize winF o si@(Signal r s) = 
    let frame z = 
          let (ws,r1) = splitAtS (winSize - o) z 
              (os,r2) = splitAtS o r1 
              h = toVectorBS . imapBS (winF winSize) $ (appendBS ws  os)
          in 
          h:(frame r1)
    in 
    Signal r (frame si)


flattenWithOverlapS :: (Unbox a,Num a) 
                    => Int 
                    -> Signal t (U.Vector a)
                    -> Signal t a 
flattenWithOverlapS o s@(Signal r l) | null l = error "A signal can't be empty : in flattenWithOverlapS"
                                     | o == 0 = concatMapS U.toList s
                                     | otherwise = 
                                          let h = headS s 
                                              n = U.length h
                                              index = U.fromList [0..o-1]
                                              _flatten [] = []
                                              _flatten (a:b:l) = U.toList (U.slice o (n-o) v) : _flatten (b:l)
                                                where 
                                                  combine b i x | i >= o = x + (b!(i-o))
                                                                | otherwise = x
                                                  v = U.imap (combine b) a
                                          in 
                                          appendListS (take o . U.toList $ h) (concatS . onSamples _flatten $ s)


cossq m i x = let sq z = z * z 
              in
              x* sq(sin(pi*fromIntegral i / fromIntegral (m-1)))

hann :: (Num a, HasDoubleRepresentation a) 
     => Int 
     -> Int 
     -> a 
     -> a
hann m n x = x * (fromDouble $ 0.5 * (1 - cos (2 * pi * fromIntegral n / fromIntegral (m - 1))))

noWindow :: Int -> Int -> a -> a
noWindow _ _ x = x

hamming :: (Num a, HasDoubleRepresentation a) 
        => Double -- ^ Alpha 
        -> Int 
        -> Int 
        -> a 
        -> a 
hamming alpha m n x = x * (fromDouble $ alpha - (1 - alpha) * cos(2*pi*fromIntegral n / fromIntegral (m-1)))

tukey :: (Num a, HasDoubleRepresentation a) 
      => Double -- ^ Alpha 
      -> Int 
      -> Int 
      -> a 
      -> a 
tukey alpha m n x | n <= floor (alpha * fromIntegral (m-1) / 2.0) = 
                        x * (fromDouble $ 0.5 * (1 + cos(pi * (2*fromIntegral n/alpha/ fromIntegral (m-1) - 1))))
                  | (floor $ alpha * fromIntegral (m-1) / 2.0) <= n  
                      && n <= floor (fromIntegral (m-1)*(1 - alpha / 2.0)) = x
                  | otherwise = x * (fromDouble $ 0.5 * 
                    (1 + cos(pi * (2*fromIntegral n/alpha/ fromIntegral (m-1) - 2 / alpha + 1))))

cosine :: (Num a, HasDoubleRepresentation a) 
       => Int 
       -> Int 
       -> a 
       -> a
cosine m n x = x * (fromDouble $ sin (pi * fromIntegral n / fromIntegral (m-1)))

lanczos :: (Num a, HasDoubleRepresentation a) 
       => Int 
       -> Int 
       -> a 
       -> a
lanczos m n x = x * (fromDouble $ sinc (2*fromIntegral n / fromIntegral (m-1) - 1))
 where 
    sinc x | x == 0 = 1 
           | otherwise = sin(pi*x) / (pi*x)

triangular :: (Num a, HasDoubleRepresentation a) 
           => Int 
           -> Int 
           -> a 
           -> a
triangular m n x = x * (fromDouble $ 2.0 / fromIntegral (m+1) * (fromIntegral (m+1)/2.0 - abs (fromIntegral n - fromIntegral(m-1)/2.0)))
