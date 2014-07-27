import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
* Refine Demi file generation to pass all regression tests...
 * Try using Version for any numeric bits...
 * Finding Exact strings and then marking up version numbers should help...
? Do we really need better regression testing? EG auto generate version numbers QuickCheck style?
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]