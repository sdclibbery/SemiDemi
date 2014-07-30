import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
* Try having BestMatch return error if more than one match
 * Then disable all fuzzy/version matching since its now irrelevant
 * Right at end, can then tidy all this up and simplify the whole thing massively if this works
* Refine Demi file generation to pass all regression tests...
 * NEXT: line 78...
? Do we really need better regression testing? EG auto generate version numbers QuickCheck style?
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]