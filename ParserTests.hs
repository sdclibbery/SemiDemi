module ParserTests (
        tests
    ) where
import Test.HUnit
import Matcher
import Parser

tests = TestLabel "Parser" $ TestList
    [ testExact
    , testEmpty
    , testDisallowed
    , testVersion
    , testExample
    ]

testExact = TestLabel "Exact" $ TestList
    [ test (Right $ Desc [Exact "abc"] [])                                                                  "[+abc]"
    , test (Right $ Desc [Exact "ab]c"] [])                                                                 "[+ab\\]c]"
    , test (Left "\"matcher\" (line 1, column 6):\nunexpected end of input\nexpecting \"\\\\\" or \"]\"")   "[+abc"
    , test (Right $ Desc [Fuzzy "123", Exact "abc", Fuzzy "456"] [])                                        "123[+abc]456"
    , test (Right $ Desc [Exact "abc", Exact "123"] [])                                                     "[+abc][+123]"
    ] where
        test e s = (show s) ~: e ~=? parse s

testEmpty = TestLabel "Empty" $ TestList
    [ test (Left $ "Empty matcher: abc")                                                              "abc"
    ] where
        test e s = (show s) ~: e ~=? parse s

testDisallowed = TestLabel "Disallowed" $ TestList
    [ test (Right $ Desc [] [Disallowed "abc"])                                                              "[-abc]"
    , test (Right $ Desc [] [Disallowed "ab]c"])                                                             "[-ab\\]c]"
    , test (Left "\"matcher\" (line 1, column 6):\nunexpected end of input\nexpecting \"\\\\\" or \"]\"")    "[-abc"
    , test (Left "\"matcher\" (line 1, column 7):\nunexpected 'd'\nexpecting end of input or \"[-\"")        "[-abc]def"
    , test (Right $ Desc [Fuzzy "123"] [Disallowed "abc", Disallowed "def"])                                 "123[-abc][-def]"
    , test (Right $ Desc [Fuzzy "123", Exact "abc"] [Disallowed "def"])                                      "123[+abc][-def]"
    ] where
        test e s = (show s) ~: e ~=? parse s

testVersion = TestLabel "Version" $ TestList
    [ test (Right $ Desc [Version "abc" ""] [])                                                              "[vabc]"
    , test (Right $ Desc [Version "abc" "1_2.3_09"] [])                                                      "[vabc1_2.3_09]"
    , test (Right $ Desc [Version "abc " "1.0.0"] [])                                                        "[vabc 1.0.0]"
    ] where
        test e s = (show s) ~: e ~=? parse s

testExample = TestLabel "Example" $ TestList
    [ test (Right $ Desc [
            Fuzzy "Mozilla/1.2(compatible; U; InfiNet 2.3; Diga) AppleWebKit/3.4+ (KHTML, like Gecko)",
            Exact "(avdn/Panasonic.bd.pro4r.2014)"
        ] [Disallowed"Opera"])   "Mozilla/1.2(compatible; U; InfiNet 2.3; Diga) AppleWebKit/3.4+ (KHTML, like Gecko)[+(avdn/Panasonic.bd.pro4r.2014)][-Opera]"
    ] where
        test e s = (show s) ~: e ~=? parse s

