import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:

* Optimise matching by building a tree of Exact/Disallowed
 * Split matching and scoring into separate files
 * Define and build a match tree
 * Then match by taking the UA string through the match tree to return a list of possible matches

* Sort the 'head' in Regression.hs
 * Empty list means that there are NO matchers setup for this test yet (think TDD red)
 * More than one entry means multiple could have matched, and they should ALL be shown as possible expected matchers in the results...

* ?How could we apply a similar global optimisation to scoring??

* Work through and get all regression tests passing...           

* Notes for writing matchers:
 * Use [+...] with something that uniquely identifies the UA ideally
 * Failing that, use several [+...]'s, along with fuzzy matching
 * Use [-...] only if it will cleanly disambiguate
 * Use [v...] only if required to help the fuzzy matcher
 * Use multiple matchers for the same UA if there are several distinct UA 'patterns'

-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        , ParserTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]