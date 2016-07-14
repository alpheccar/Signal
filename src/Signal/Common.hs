module Signal.Common(
          Time(..),
          Frequency(..),
          HasDoubleRepresentation(..)
        ) where


import Signal.Internal(Time(..), Frequency(..), Dual(..))

-- | Signals can use values which are not directly doubles (like voltage etc ...)
-- Those values must be convertible into Double for display on a graphic
class HasDoubleRepresentation a where
    toDouble :: a -> Double 
    fromDouble :: Double -> a

instance HasDoubleRepresentation Time where 
        toDouble = getT
        fromDouble = Time

instance HasDoubleRepresentation Frequency where 
        toDouble = getF
        fromDouble = Frequency
