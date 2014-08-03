import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
x Remove 'dubious' DemiMaker matches
* Remove 'empty' check in parser
* In BestMatch, if multiple matches occur, do a single editDistance to find the best
-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        , ParserTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]