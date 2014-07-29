import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
* Refine Demi file generation to pass all regression tests...
 * NEXT: line 74...
? Do we really need better regression testing? EG auto generate version numbers QuickCheck style?
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]