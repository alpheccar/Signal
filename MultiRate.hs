module MultiRate(
	  decimateS
	, interpolateS
	) where 

import Signal 
import Internal 
import Data.List.Stream
import Prelude hiding(drop, replicate, (++),(:))

decimateS :: Int -> Signal t a -> Signal t a 
decimateS i s = ss (decimateL i) s
 
decimateL i [] = []
decimateL i (h:l) = h:decimateL i (drop i l)

interpolateS :: Num a => Int -> Signal t a -> Signal t a 
interpolateS i s = ss (interpolateL i) s
  
interpolateL i [] = [] 
interpolateL i (h:l) = h:replicate i 0 ++ interpolateL i l
