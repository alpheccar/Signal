module Internal(
	  Signal(..)
	) where 

import Data.List.Stream

newtype Signal a = Signal {getSamples :: [a]}

