module Transform(
	   myFFT 
	) where 

import Data.Complex
import Numeric.GSL.Fourier
import qualified Data.Vector.Unboxed as U

myFFT :: Double -> U.Vector Double -> U.Vector Double
myFFT t d = 
	let l =  fromIntegral (U.length d)
	    complexd = U.map (:+ 0.0) d
	    m (x :+ y) = t*(x*x + y*y) / l
	in 
	U.map m . U.convert . fft  . U.convert $ complexd