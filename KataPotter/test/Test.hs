
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.HUnit

import KattaPotter

tests = [
    testGroup "cases" $ zipWith (testCase . show) [1::Int ..] $
    [ 
        assertEqual "Testing basics 1" 0 (price [])
        , assertEqual "Testing basics 2" 8 (price [0])
        , assertEqual "Testing basics 3" 8 (price [1])
        , assertEqual "Testing basics 4" 8 (price [2])
        , assertEqual "Testing basics 5" 8 (price [3])
        , assertEqual "Testing basics 6" 8 (price [4])
        , assertEqual "Testing basics 7" (8 * 2) (price [0, 0])
        , assertEqual "Testing basics 8" (8 * 3) (price [1, 1, 1])
        , assertEqual "Discount 1" (8 * 2 * 0.95) (price [0, 1])
        , assertEqual "Discount 2" (8 * 3 * 0.9) (price [0, 2, 4])
        , assertEqual "Discount 3" (8 * 4 * 0.8) (price [0, 1, 2, 4])
        , assertEqual "Discount 4" (8 * 5 * 0.75) (price [0, 1, 2, 3, 4])
        , assertEqual "Several discounts 1" (8 + (8 * 2 * 0.95)) (price [0, 0, 1])
        , assertEqual "Several discounts 2" (2 * (8 * 2 * 0.95)) (price [0, 0, 1, 1])
        , assertEqual "Several discounts 3" ((8 * 4 * 0.8) + (8 * 2 * 0.95)) (price [0, 0, 1, 2, 2, 3])
        , assertEqual "Several discounts 4" (8 + (8 * 5 * 0.75)) (price [0, 1, 1, 2, 3, 4])
        , assertEqual "Edge case 1" (2 * (8 * 4 * 0.8)) (price [0, 0, 1, 1, 2, 2, 3, 4])
        , assertEqual "Edge case 2" (3 * (8 * 5 * 0.75) + 2 * (8 * 4 * 0.8)) (price [0, 0, 0, 0, 0,
                                                                                             1, 1, 1, 1, 1,
                                                                                             2, 2, 2, 2, 
                                                                                             3, 3, 3, 3, 3,
                                                                                             4, 4, 4, 4])
    ]
    ]

main = defaultMain tests
