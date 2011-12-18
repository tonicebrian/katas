import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.HUnit

import LoveTriangle

tests = [
    testGroup "cases" $ zipWith (testCase . show) [1::Int ..] $
    [ case_Test1
    , case_Test2
    , case_Test3
    , case_Test4
    , case_Test5
    , case_Test6
    , case_Test7
    , case_Test8
    ]
    ]

--------------------------------------------------    
case_Test1 = assertEqual "message" expected obtained
    where
        expected = 10
        obtained = lovetriangle [15,15,15,15,15]

--------------------------------------------------    
case_Test2 = assertEqual "message" expected obtained
    where
        expected = 10
        obtained = lovetriangle [15,15,15,15,15]

--------------------------------------------------    
case_Test3 = assertEqual "message" expected obtained
    where
        expected = 10
        obtained = lovetriangle [15,15,15,15,15]

--------------------------------------------------    
case_Test4 = assertEqual "message" expected obtained
    where
        expected = 10
        obtained = lovetriangle [15,15,15,15,15]

--------------------------------------------------    
case_Test5 = assertEqual "message" expected obtained
    where
        expected = 10
        obtained = lovetriangle [15,15,15,15,15]

--------------------------------------------------    
case_Test6 = assertEqual "message" expected obtained
    where
        expected = 10
        obtained = lovetriangle [15,15,15,15,15]

--------------------------------------------------    
case_Test7 = assertEqual "message" expected obtained
    where
        expected = 10
        obtained = lovetriangle [15,15,15,15,15]

--------------------------------------------------    
case_Test8 = assertEqual "message" expected obtained
    where
        expected = 10
        obtained = lovetriangle [15,15,15,15,15]

-- Main program
main = defaultMain tests 



