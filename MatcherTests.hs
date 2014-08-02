module MatcherTests (
        tests
    ) where
import Test.HUnit
import Matcher

tests = TestLabel "Matcher" $ TestList
    [ testEmpty
    , testExact
    , testDisallowed
    , testExamples
    ]

testEmpty = TestLabel "Empty" $ TestList
    [ test True       (Desc [] [])
    , test False       (Desc [Exact "any"] [])
    , test False       (Desc [Exact "any"] [Disallowed "atall"])
    , test False       (Desc [] [Disallowed "atall"])
    ] where
        test e d = (show d) ~: e ~=? empty d

testExact = TestLabel "Exact" $ TestList
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

testDisallowed = TestLabel "Disallowed" $ TestList
    [ test True       (Desc [] [Disallowed "d"])                   "abc"
    , test False      (Desc [] [Disallowed "a"])                   "abc"
    , test False      (Desc [] [Disallowed "d", Disallowed "a"])   "abc"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? matches d s

testExamples = TestLabel "Examples" $ TestList
    [ test True     desc "Mozilla/5.0(compatible; U; InfiNet 0.1; Diga) AppleWebKit/420+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014)"
    , test True     desc "Mozilla/1234(compatible; U; InfiNet 1.2.3.4; Diga) AppleWebKit/5678+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014)"
    , test False    desc "Mozilla/5.0(compatible; U; InfiNet 0.1; Diga) AppleWebKit/420+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro5r.2014)"
    , test True     desc "Mozilla/5.0(!compatible; U; InfiNet 0.1; Diga) AppleWebKit/420+ (KHTML, like Gecko)(avdn/Panasonic.bd.pro4r.2014)"
    , test True     desc "Mozilla/5.0(compatible; UX; InfiNet 0.1; Diga; woo) AppleWebKit/420++ (KHTML, like Gecko yeah)(avdn/Panasonic.bd.pro4r.2014) blah"
    ] where
        test e d s = (show s ++ show d) ~: e ~=? matches d s
        desc = (Desc [Exact "Panasonic.bd.pro4r.2014"] [Disallowed "Opera"])

