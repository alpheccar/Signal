{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module TypeAddition where

import GHC.TypeLits

type instance (15 :: Nat) + (1 :: Nat)  = 16
type instance (15 :: Nat) + (2 :: Nat)  = 17
type instance (15 :: Nat) + (3 :: Nat)  = 18
type instance (15 :: Nat) + (4 :: Nat)  = 19
type instance (15 :: Nat) + (5 :: Nat)  = 20
type instance (15 :: Nat) + (6 :: Nat)  = 21
type instance (15 :: Nat) + (7 :: Nat)  = 22
type instance (15 :: Nat) + (8 :: Nat)  = 23
type instance (15 :: Nat) + (9 :: Nat)  = 24
type instance (15 :: Nat) + (10 :: Nat)  = 25
type instance (15 :: Nat) + (11 :: Nat)  = 26
type instance (15 :: Nat) + (12 :: Nat)  = 27
type instance (15 :: Nat) + (13 :: Nat)  = 28
type instance (15 :: Nat) + (14 :: Nat)  = 29
type instance (15 :: Nat) + (15 :: Nat)  = 30
type instance (15 :: Nat) + (16 :: Nat)  = 31
