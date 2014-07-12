module MatcherTests (
        tests
    ) where
import Test.HUnit
import Matcher

tests = TestLabel "Matcher" $ TestList
    [ testExact
    , testDisallowed
    , testVersion
    , testFuzzy
    , testExamples
    ]

testExact = TestLabel "Exact" $ TestList
    [ test (Just 3)     (Desc [Exact "abc"] [])                   "abc"
    , test Nothing      (Desc [Exact "abc"] [])                   "abd"
    , test (Just 1)     (Desc [Exact "ab"] [])                    "abc"
    , test (Just 1)     (Desc [Exact "bc"] [])                    "abc"
    , test Nothing      (Desc [Exact "ab", Exact "bc"] [])        "abc"
    , test Nothing      (Desc [Exact "ab", Exact "cd"] [])        "abc"
    , test (Just 3)     (Desc [Exact "ab", Exact "c"] [])         "abc"
    , test (Just 1)     (Desc [Exact "a", Exact "c"] [])          "abc"
    , test (Just (-2))  (Desc [Exact "cde"] [])                   "abcabcde"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s

testDisallowed = TestLabel "Disallowed" $ TestList
    [ test (Just (-3))  (Desc [] [Disallowed "d"])                   "abc"
    , test Nothing      (Desc [] [Disallowed "a"])                   "abc"
    , test Nothing      (Desc [] [Disallowed "d", Disallowed "a"])   "abc"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s

testVersion = TestLabel "Version" $ TestList
    [ test (Nothing)    (Desc [Version] [])                   "abc"
    , test (Just 1)     (Desc [Version] [])                   "1"
    , test (Just 1)     (Desc [Version] [])                   "1.0"
    , test (Just (-2))  (Desc [Version] [])                   "xxx1.0"
    , test (Just (-2))  (Desc [Version] [])                   "1.0xxx"
    , test (Just 1)     (Desc [Version] [])                   "0.0.0"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s

testFuzzy = TestLabel "Fuzzy" $ TestList
    [ test (Just 4)     (Desc [Fuzzy "abcd"] [])                   "abcd"
    , test (Just 0)     (Desc [Fuzzy "ab"] [])                     "abcd"
    , test (Just (-4))  (Desc [Fuzzy "ab"] [])                     "axxxb"
    , test (Just (-2))  (Desc [Fuzzy "abef"] [])                   "abcd"
    , test (Just 0)     (Desc [Fuzzy "abcd"] [])                   "ab"
    , test (Just 4)     (Desc [Fuzzy "ab", Fuzzy "cd"] [])         "abcd"
    , test (Just (-4))  (Desc [] [])                               "abcd"
    , test (Just (-5))  (Desc [Fuzzy "ab"] [])                     "xyz"
    , test (Just (-2))  (Desc [Fuzzy "ab", Fuzzy "cd"] [])         "cdab"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s

testExamples = TestLabel "Examples" $ TestList
    [ test (Just 105) desc "Mozilla/5.0(compatible; U; InfiNet 0.1; Diga) AppleWebKit/420+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014)"
    , test (Just 105) desc "Mozilla/1234(compatible; U; InfiNet 1.2.3.4; Diga) AppleWebKit/5678+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014)"
    , test Nothing    desc "Mozilla/5.0(compatible; U; InfiNet 0.1; Diga) AppleWebKit/420+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro5r.2014)"
    , test (Just 97) desc "Mozilla/5.0(compatible; X; InfiNet 0.1; Diga; woo) AppleWebKit/420 (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014) blah"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s
        desc = (Desc [ Fuzzy "Mozilla/"
                , Version
                , Fuzzy "(compatible; U; InfiNet "
                , Version
                , Fuzzy "; Diga) AppleWebKit/"
                , Version
                , Fuzzy "+ (KHTML like Gecko)"
                , Exact "(avdn/Panasonic.bd.pro4r.2014)"
            ] [Disallowed"Opera"])

