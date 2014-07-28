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
    , testFullFuzzy
    , testExamples
    ]

testExact = TestLabel "Exact" $ TestList
    [ test (Just 9)     (Desc [Exact "abc"] [])                   "abc"
    , test Nothing      (Desc [Exact "abc"] [])                   "abd"
    , test (Just 5)     (Desc [Exact "ab"] [])                    "abc"
    , test (Just 5)     (Desc [Exact "bc"] [])                    "abc"
    , test Nothing      (Desc [Exact "ab", Exact "bc"] [])        "abc"
    , test Nothing      (Desc [Exact "ab", Exact "cd"] [])        "abc"
    , test (Just 9)     (Desc [Exact "ab", Exact "c"] [])         "abc"
    , test (Just 5)     (Desc [Exact "a", Exact "c"] [])          "abc"
    , test (Just 4)  (Desc [Exact "cde"] [])                   "abcabcde"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s

testDisallowed = TestLabel "Disallowed" $ TestList
    [ test (Just (-3))  (Desc [] [Disallowed "d"])                   "abc"
    , test Nothing      (Desc [] [Disallowed "a"])                   "abc"
    , test Nothing      (Desc [] [Disallowed "d", Disallowed "a"])   "abc"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s

testVersion = TestLabel "Version" $ TestList
    [ test (Just (-2))  (Desc [Version] [])                   "abc"
    , test (Just 1)     (Desc [Version] [])                   ""
    , test (Just 1)     (Desc [Version] [])                   "1"
    , test (Just 1)     (Desc [Version] [])                   "1.0"
    , test (Just (-2))  (Desc [Version] [])                   "xxx1.0"
    , test (Just (-2))  (Desc [Version] [])                   "1.0xxx"
    , test (Just 1)     (Desc [Version] [])                   "0.0.0"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s

testFuzzy = TestLabel "Fuzzy" $ TestList
    [ test (Just (-4))     (Desc [Fuzzy "abcd"] [])                 "abcd"
    , test (Just (-4))     (Desc [Fuzzy ""] [])                     "abcd"
    , test (Just (-4))     (Desc [Fuzzy "12"] [])                   "abcd"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s

testFullFuzzy = TestLabel "FullFuzzy" $ TestList
    [ test (Just 4)     (Desc [FullFuzzy "abcd"] [])                   "abcd"
    , test (Just 0)     (Desc [FullFuzzy "ab"] [])                     "abcd"
    , test (Just (-1))  (Desc [FullFuzzy "ab"] [])                     "axxxb"
    , test (Just (-2))  (Desc [FullFuzzy "abef"] [])                   "abcd"
    , test (Just 0)     (Desc [FullFuzzy "abcd"] [])                   "ab"
    , test (Just (-2))  (Desc [FullFuzzy "abcd"] [])                   "cdab"
    , test (Just 0)     (Desc [FullFuzzy "ab"] [])                     "cdab"
    , test (Just 4)     (Desc [FullFuzzy "ab", FullFuzzy "cd"] [])     "abcd"
    , test (Just (-4))  (Desc [] [])                                   "abcd"
    , test (Just (-5))  (Desc [FullFuzzy "ab"] [])                     "xyz"
    , test (Just (-2))  (Desc [FullFuzzy "ab", FullFuzzy "cd"] [])     "cdab"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s

testExamples = TestLabel "Examples" $ TestList
    [ test (Just 20) desc "Mozilla/5.0(compatible; U; InfiNet 0.1; Diga) AppleWebKit/420+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014)"
    , test (Just 20) desc "Mozilla/1234(compatible; U; InfiNet 1.2.3.4; Diga) AppleWebKit/5678+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014)"
    , test Nothing   desc "Mozilla/5.0(compatible; U; InfiNet 0.1; Diga) AppleWebKit/420+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro5r.2014)"
    , test (Just 19) desc "Mozilla/5.0(!compatible; U; InfiNet 0.1; Diga) AppleWebKit/420+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014)"
    , test (Just 3)  desc "Mozilla/5.0(compatible; UX; InfiNet 0.1; Diga; woo) AppleWebKit/420++ (KHTML, like Gecko yeah)(avdn/Panasonic.bd.pro4r.2014) blah"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s
        desc = (Desc [ Fuzzy "Mozilla/"
                , Version
                , Fuzzy "(compatible; U; InfiNet "
                , Version
                , Fuzzy "; Diga) AppleWebKit/"
                , Version
                , Fuzzy "+ (KHTML, like Gecko)"
                , Exact "(avdn/Panasonic.bd.pro4r.2014)"
            ] [Disallowed"Opera"])

