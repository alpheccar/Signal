module Windows(
      hann
    , noWindow
    , hamming
    , tukey
    , cosine
    , lanczos
    , triangular
    ) where 

import Common

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
