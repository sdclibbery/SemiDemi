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
    [ -- test (Just 4)     (Desc [Fuzzy "abcd"] [])                   "abcd"
--    , test (Just 0)     (Desc [Fuzzy "ab"] [])                     "abcd"
--    , test (Just 1)     (Desc [Fuzzy "ab"] [])                     "axb"
--    , test (Just 0)     (Desc [Fuzzy "abef"] [])                   "abcd"
--    , test (Just 2)     (Desc [Fuzzy "abcd"] [])                   "ab"
--    , test (Just 4)     (Desc [Fuzzy "ab", Fuzzy "cd"] [])      "abcd"
--    , test (Just 0)     (Desc [] [])                                  "abcd"
--    , test (Just 2)     (Desc [Fuzzy "abcd"] [])                   "ab"
--    , test (Just 0)     (Desc [Fuzzy "ab", Fuzzy "cd"] [])      "cdab"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? score d s

