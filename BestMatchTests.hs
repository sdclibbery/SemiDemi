module BestMatchTests (
        tests
    ) where
import Test.HUnit
import qualified Matcher as M
import BestMatch

tests = TestLabel "BestMatch" $ TestList
    [ testSimple
    , testTricky
    ]

testSimple = TestLabel "Simple" $ TestList
    [ test   (Right $ ex "abc")   [ex "abc"]                             "abc"
    , test   (Right $ ex "abc")   [ex "abc", ex "def"]                   "abc"
    , test   (Right $ ex "abc")   [ex "def", ex "abc"]                   "abc"
    ] where
        test e ms s = (show s ++ show ms) ~: e ~=? match s ms
        ex s = (M.Desc [M.Exact s] [], s)

testTricky = TestLabel "Tricky" $ TestList
    [ -- Test: 
    ] where
        test e ms s = (show s ++ show ms) ~: e ~=? match s ms
        m s = (M.Desc [M.Exact "WooHoo"] [], s)
