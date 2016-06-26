{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Signal.TypeAddition where

import GHC.TypeLits

-- type instance (15 :: Nat) + (1 :: Nat)  = (16 :: Nat)
-- type instance (15 :: Nat) + (2 :: Nat)  = (17 :: Nat)
-- type instance (15 :: Nat) + (3 :: Nat)  = (18 :: Nat)
-- type instance (15 :: Nat) + (4 :: Nat)  = (19 :: Nat)
-- type instance (15 :: Nat) + (5 :: Nat)  = (20 :: Nat)
-- type instance (15 :: Nat) + (6 :: Nat)  = (21 :: Nat)
-- type instance (15 :: Nat) + (7 :: Nat)  = (22 :: Nat)
-- type instance (15 :: Nat) + (8 :: Nat)  = (23 :: Nat)
-- type instance (15 :: Nat) + (9 :: Nat)  = (24 :: Nat)
-- type instance (15 :: Nat) + (10 :: Nat)  = (25 :: Nat)
-- type instance (15 :: Nat) + (11 :: Nat)  = (26 :: Nat)
-- type instance (15 :: Nat) + (12 :: Nat)  = (27 :: Nat)
-- type instance (15 :: Nat) + (13 :: Nat)  = (28 :: Nat)
-- type instance (15 :: Nat) + (14 :: Nat)  = (29 :: Nat)
-- type instance (15 :: Nat) + (15 :: Nat)  = (30 :: Nat)
-- type instance (15 :: Nat) + (16 :: Nat)  = (31 :: Nat)
--
-- type instance (1 :: Nat) + (1 :: Nat) = (2 :: Nat)
-- type instance (2 :: Nat) + (2 :: Nat) = (4 :: Nat)
-- type instance (3 :: Nat) + (3 :: Nat) = (6 :: Nat)
-- type instance (4 :: Nat) + (4 :: Nat) = (8 :: Nat)
-- type instance (5 :: Nat) + (5 :: Nat) = (10 :: Nat)
-- type instance (6 :: Nat) + (6 :: Nat) = (12 :: Nat)
-- type instance (7 :: Nat) + (7 :: Nat) = (14 :: Nat)
-- type instance (8 :: Nat) + (8 :: Nat) = (16 :: Nat)
-- type instance (9 :: Nat) + (9 :: Nat) = (18 :: Nat)
-- type instance (10 :: Nat) + (10 :: Nat) = (20 :: Nat)
-- type instance (11 :: Nat) + (11 :: Nat) = (22 :: Nat)
-- type instance (12 :: Nat) + (12 :: Nat) = (24 :: Nat)
-- type instance (13 :: Nat) + (13 :: Nat) = (26 :: Nat)
-- type instance (14 :: Nat) + (14 :: Nat) = (28 :: Nat)
-- type instance (15 :: Nat) + (15 :: Nat) = (30 :: Nat)
--
