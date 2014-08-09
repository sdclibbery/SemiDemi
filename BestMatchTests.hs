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

