module ScorerTests (
        tests
    ) where
import Test.HUnit
import Matcher
import Scorer

tests = TestLabel "Scorer" $ TestList
    [ testScore
    , testVersion
    , testScoreExample
    ]

testScore = TestLabel "Score" $ TestList
    [ test 3    (Desc [] [])                                  "abc"
    , test 0     (Desc [Fuzzy "abc"] [])                       "abc"
    , test 0     (Desc [Fuzzy "abc"] [Disallowed "def"])       "abc"
    , test 0     (Desc [Fuzzy "ab", Fuzzy "c"] [])             "abc"
    , test 0     (Desc [Fuzzy "a", Exact "b", Fuzzy "c"] [])   "abc"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s

testVersion = TestLabel "Version" $ TestList
    [ test 0    (Desc [v, e] [])                                  "abc1.00def"
    , test 0    (Desc [v, e] [])                                  "abc0def"
    , test 0    (Desc [v, e] [])                                  "abc1.0.0def"
    , test 0    (Desc [v, e] [])                                  "abc99.999.999def"
    , test 0    (Desc [v, e] [])                                  "abc0_1def"
    , test 0    (Desc [Version "a*(b)+c", e] [])                  "a*(b)+c1.0def"
    , test 0    (Desc [Version "abc"] [])                         "abc1.00"
    , test 0    (Desc [Version "; "] [])                          "; 1.00"
    , test 0    (Desc [Version "; "] [])                          "; 2.00"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s
        v = Version "abc"
        e = Exact "def"

testScoreExample = TestLabel "ScoreExample" $ TestList
    [ test   1     ps1                      "Mozilla/5.0 (PLAYSTATION 3; 2.00)"
    , test   0     ps2                      "Mozilla/5.0 (PLAYSTATION 3; 2.00)"
    , test   2     ps1                      "Mozilla/2.0 (PLAYSTATION 3; 4.70)"
    , test   1     ps4                      "Mozilla/2.0 (PLAYSTATION 3; 4.70)"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s
        ps1 = Desc [Version "Mozilla/", Fuzzy " (", Exact "PLAYSTATION 3", Fuzzy "; 1.00)"] []
        ps2 = Desc [Version "Mozilla/", Fuzzy " (", Exact "PLAYSTATION 3", Fuzzy "; 2.00)"] []
        ps4 = Desc [Version "Mozilla/", Fuzzy " (", Exact "PLAYSTATION 3", Fuzzy "; 4.77)"] []
