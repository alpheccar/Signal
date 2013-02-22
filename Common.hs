{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Common(
	  Time(..),
	  Frequency(..),
	  HasDoubleRepresentation(..)
	) where 


newtype Time = Time {getT :: Double} deriving(Eq,Ord,Show,Floating,Fractional,Num,Real,RealFloat,RealFrac,Read)
newtype Frequency = Frequency {getF :: Double} deriving(Eq,Ord,Show,Floating,Fractional,Num,Real,RealFloat,RealFrac,Read)


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