module BestMatchTests (
        tests
    ) where
import Test.HUnit
import qualified Matcher as M
import BestMatch

tests = TestLabel "BestMatch" $ TestList
    [ testSingleResult
    , testNoResult
    , testMultipleResult
    , testVersion
    , testExample
    ]

testNoResult = TestLabel "NoResult" $ TestList
    [ test   (Left "No matches for: def")   [ex "abc"]                   "def"
    ] where
        test e ms s = (show s ++ show ms) ~: e ~=? match s ms
        ex s = (M.Desc [M.Exact s] [], s)

testSingleResult = TestLabel "SingleResult" $ TestList
    [ test   (Right $ ex "abc")   [ex "abc"]                             "abc"
    , test   (Right $ ex "abc")   [ex "abc", ex "def"]                   "abc"
    , test   (Right $ ex "abc")   [ex "def", ex "abc"]                   "abc"
    ] where
        test e ms s = (show s ++ show ms) ~: e ~=? match s ms
        ex s = (M.Desc [M.Exact s] [], s)

testMultipleResult = TestLabel "MultipleResult" $ TestList
    [ test   (Right $ m "abc")       [m "abc", m "def"]                      "abcWooHoo"
    , test   (Right $ m "def")       [m "abc", m "def"]                      "defWooHoo"
    , test   (Right $ m "defdefdef") [m "a", m "defdefdef"]                  "defdefdefWooHoo"
    ] where
        test e ms s = (show s ++ show ms) ~: e ~=? match s ms
        m s = (M.Desc [M.Fuzzy s, M.Exact "WooHoo"] [], s)

testVersion = TestLabel "Version" $ TestList
    [ test   (Right $ m "abc")       [m "abc", m "ab c"]                      "abc123"
    , test   (Right $ m "abc")       [m "abc", m "ab c"]                      "abc456"
    ] where
        test e ms s = (show s ++ show ms) ~: e ~=? match s ms
        m s = (M.Desc [M.Fuzzy s, M.Version "c"] [], s)

testExample = TestLabel "Example" $ TestList
    [ test   (Right $ m2)       [m1, m2]                      "Mozilla/5.0 (PLAYSTATION 3; 2.00)"
    ] where
        test e ms s = (show s ++ show ms) ~: e ~=? match s ms
        m1 = (M.Desc [M.Version "Mozilla/", M.Fuzzy " (", M.Exact "PLAYSTATION 3", M.Fuzzy "; 1.00)"] [], "m1")
        m2 = (M.Desc [M.Version "Mozilla/", M.Fuzzy " (", M.Exact "PLAYSTATION 3", M.Fuzzy "; 2.00)"] [], "m2")
