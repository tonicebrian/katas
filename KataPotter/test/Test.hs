
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.HUnit

import KattaPotter

tests = [
    testGroup "cases" $ zipWith (testCase . show) [1::Int ..] $
    [ case_basics
    ]
    , testGroup "properties" $ zipWith (testProperty . show) [1::Int ..] $
        [ property prop_theProperty]
    ]

--------------------------------------------------    
case_basics = assertEqual "message" expected obtained
    where
        expected = True
        obtained = False

--------------------------------------------------    
prop_theProperty = True

main = defaultMain tests
