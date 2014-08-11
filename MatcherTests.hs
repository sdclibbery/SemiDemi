module MatcherTests (
        tests
    ) where
import Test.HUnit
import Matcher

tests = TestLabel "Matcher" $ TestList
    [ testEmpty
    , testMatchesExact
    , testMatchesDisallowed
    , testMatchesExamples
    , testScore
    , testVersion
    , testScoreExample
    ]

testEmpty = TestLabel "Empty" $ TestList
    [ test True        (Desc [] [])
    , test True        (Desc [Fuzzy "any"] [])
    , test False       (Desc [Exact "any"] [])
    , test False       (Desc [Exact "any"] [Disallowed "atall"])
    , test False       (Desc [] [Disallowed "atall"])
    ] where
        test e d = (show d) ~: e ~=? empty d

testMatchesExact = TestLabel "MatchesExact" $ TestList
    [ test True     (Desc [Exact "abc"] [])                   "abc"
    , test False    (Desc [Exact "abc"] [])                   "abd"
    , test True     (Desc [Exact "ab"] [])                    "abc"
    , test True     (Desc [Exact "bc"] [])                    "abc"
    , test True     (Desc [Exact "ab", Exact "bc"] [])        "abc"
    , test False    (Desc [Exact "ab", Exact "cd"] [])        "abc"
    , test True     (Desc [Exact "ab", Exact "c"] [])         "abc"
    , test True     (Desc [Exact "a", Exact "c"] [])          "abc"
    , test True     (Desc [Exact "cde"] [])                   "abcabcde"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? matches d s

testMatchesDisallowed = TestLabel "MatchesDisallowed" $ TestList
    [ test True       (Desc [] [Disallowed "d"])                   "abc"
    , test False      (Desc [] [Disallowed "a"])                   "abc"
    , test False      (Desc [] [Disallowed "d", Disallowed "a"])   "abc"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? matches d s

testMatchesExamples = TestLabel "MatchesExamples" $ TestList
    [ test True     desc   "Mozilla/5.0(compatible; U; InfiNet 0.1; Diga) AppleWebKit/420+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014)"
    , test True     desc   "Mozilla/1234(compatible; U; InfiNet 1.2.3.4; Diga) AppleWebKit/5678+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014)"
    , test False    desc   "Mozilla/5.0(compatible; U; InfiNet 0.1; Diga) AppleWebKit/420+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro5r.2014)"
    , test True     desc   "Mozilla/5.0(!compatible; U; InfiNet 0.1; Diga) AppleWebKit/420+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014)"
    , test True     desc   "Mozilla/5.0(compatible; UX; InfiNet 0.1; Diga; woo) AppleWebKit/420++ (KHTML, like Gecko yeah)(avdn/Panasonic.bd.pro4r.2014) blah"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? matches d s
        desc = Desc [Exact "Panasonic.bd.pro4r.2014"] [Disallowed "Opera"]

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
