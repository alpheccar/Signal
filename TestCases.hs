{-# LANGUAGE DataKinds #-}
module TestCases(
	runTests
	) where 

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import Fixed 

tests = [
        testGroup "Saturation" [
                testCase "Saturation Int16" test_sat16
            ]
    ]

test_sat16 = (fromRawValue 0x7FFE :: Fixed Int16 0 Saturated) + 2 @?= fromRawValue 0x7FFF

runTests = defaultMain tests