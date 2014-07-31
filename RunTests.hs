import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
x Try having BestMatch return error if more than one match
x Then disable all fuzzy/version matching since its now irrelevant
x When parsing, ignore Fuzzy, FullFuzzy and Version altogether
x Tidy Matcher up: remove unused stuff...
* Make it a parse time error to have a matcher with no Exact or Disallowed in it
 * And also a parse time error to have two identical matchers with different outputs
* Refine Demi file generation to pass all regression tests...
 * NEXT: line 81...
 * If multiple results are unavoidable, do a single editDistance on the whole ua string for just the matched results...
-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        , ParserTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]