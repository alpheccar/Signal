{-# LANGUAGE DataKinds #-}
module Signal.TestCases(
        runTests
        ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import Signal.SpecialInt

import Data.Bits
import Data.Word
import Data.Int
import Text.Printf

h x = printf "0x%x\n" x

tests = [
        specialIntTests
    ]

runTests = defaultMain tests


{-

Tests

-}

specialIntTests = testGroup "SpecialInt" [
            testGroup "Bits" [
                      testCase "complement int40" test_complement_int40
                    , testCase "complement word40" test_complement_word40
                    , testCase "shift right int40" test_shift_right_int40
                    , testCase "shift right word40" test_shift_right_word40
                    , testCase "shift left int40" test_shift_left_int40
                    , testCase "shift left word40" test_shift_left_word40
                    , testCase "rotate left int40" test_rotate_left_int40
                    , testCase "rotate left word40" test_rotate_left_word40
                    , testCase "rotate right positive int40" test_rotate_right_pos_int40
                    , testCase "rotate right negative int40" test_rotate_right_neg_int40
                    , testCase "rotate right word40" test_rotate_right_word40
                    ]
            , testGroup "Num" [
                      testCase "fromInteger int40 max" test_from_integer_int40_max
                    , testCase "fromInteger word40 max" test_from_integer_word40_max
                    , testCase "fromInteger int40 min" test_from_integer_int40_min
                    ]
            ]

test_complement_int40 = baseValue (complement (1 :: Int40)) @?= 0x0FFFFFFFFFE
test_complement_word40 = baseValue (complement (1 :: Word40)) @?= 0x0FFFFFFFFFE

test_shift_right_int40 = baseValue (((fromBaseValue 0x0FFFFFFFFFF) :: Int40) `shiftR` 1) @?= 0x0FFFFFFFFFF
test_shift_right_word40 = baseValue (((fromBaseValue 0x0FFFFFFFFFF) :: Word40) `shiftR` 1) @?= 0x07FFFFFFFFF
test_shift_left_int40 = baseValue (((fromBaseValue 0x0FFFFFFFFFF) :: Int40) `shiftL` 1) @?= 0x0FFFFFFFFFE
test_shift_left_word40 = baseValue (((fromBaseValue 0x0FFFFFFFFFF) :: Word40) `shiftL` 1) @?= 0x0FFFFFFFFFE

test_rotate_left_int40  = baseValue (((fromBaseValue 0x0FFFFFFFFFC) :: Int40) `rotate` 1)  @?= 0x0FFFFFFFFF9
test_rotate_left_word40 = baseValue (((fromBaseValue 0x0FFFFFFFFFC) :: Word40) `rotate` 1) @?= 0x0FFFFFFFFF9

test_rotate_right_neg_int40  = baseValue (((fromBaseValue 0x0FFFFFFFFFC) :: Int40) `rotate` (-1))  @?= 0x07FFFFFFFFE
test_rotate_right_pos_int40  = baseValue (((fromBaseValue 0x1) :: Int40) `rotate` (-1))  @?= 0x08000000000
test_rotate_right_word40 = baseValue (((fromBaseValue 0x0FFFFFFFFFC) :: Word40) `rotate` (-1)) @?= 0x07FFFFFFFFE

test_from_integer_int40_max = baseValue ((fromInteger 0x018000000001) :: Int40) @?= 0x01
test_from_integer_word40_max = baseValue ((fromInteger 0x018000000001) :: Word40) @?= 0x08000000001 
test_from_integer_int40_min = baseValue ((fromInteger (- 0x10000000000)) :: Int40) @?= 0x00000000000
