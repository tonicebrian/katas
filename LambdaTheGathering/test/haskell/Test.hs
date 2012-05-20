import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.HUnit


tests = [
    testGroup "cases" $ zipWith (testCase . show) [1::Int ..] $
    [ case_theTest
    ]
    , testGroup "properties" $ zipWith (testProperty . show) [1::Int ..] $
        [ property prop_theProperty]
    ]

--------------------------------------------------    
case_theTest = assertEqual "message" expected obtained
    where
        expected = True
        obtained = False

--------------------------------------------------    
prop_theProperty = True

-- Main program
main = defaultMain tests 

