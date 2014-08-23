import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:

* show instance for Matcher to build back into the markup

* Reprofile with many 'version' tags in DemiMaker

* Try regression again, work through each failure in turn
 * Remove generic smarttv matcher from .demi file, but use it as the default result when nothing else matches

-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        , ParserTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]