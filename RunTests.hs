import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
x Try having BestMatch return error if more than one match
x Then disable all fuzzy/version matching since its now irrelevant
* When parsing, ignore Fuzzy, FullFuzzy and Version altogether
* Its now a parse time error to have a matcher with no Exact or Disallowed in it...
* Refine Demi file generation to pass all regression tests...
 * NEXT: line 78...
* Tidy Matcher up: remove unused stuff...
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]