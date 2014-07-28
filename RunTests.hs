import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
* Refine Demi file generation to pass all regression tests...
 x DemiMaker should apply specific rules for generating the matcher markup
  x Iterate through finding these rules until entire .demi file is marked up well
  x Have a long list of very specific things to markup as Exact
  x And things to markup as version: "Mozilla/xxx" becomes "Mozilla/[v]" etc
 * NEXT: line 15
? Do we really need better regression testing? EG auto generate version numbers QuickCheck style?
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]