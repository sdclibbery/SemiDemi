import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
x Changes to basic matching strategy
 x Version allows underscores as well as .'s
* Refine Demi file generation to pass all regression tests...
 * DemiMaker should apply specific rules for generating the matcher markup
  * Iterate through finding these rules until entire .demi file is marked up well
  * Have a long list of very specific things to markup as Exact
  * Eg "Mozilla/xxx" becomes "Mozilla/[v]" etc
? Do we really need better regression testing? EG auto generate version numbers QuickCheck style?
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]