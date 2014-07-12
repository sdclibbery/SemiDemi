import Test.HUnit
import qualified MatcherTests

{- TODO:
* Use lev over all prefixes; pick where editdistance is lowest
-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        ]