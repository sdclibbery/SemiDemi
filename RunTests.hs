import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:

* profile/optimise; esp. scoring/normalisation
 * Undo current changes and switch MatchString fully over to be ByteString instead.
 * If still slow, run with -O2 (can setup sublime to do this?)

* Try regression again, work through each failure in turn
 * Remove generic smarttv matcher from .demi file, but use it as the default result when nothing else matches

-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        , ParserTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]