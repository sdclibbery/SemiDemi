module ParserTests (
        tests
    ) where
import Test.HUnit
import Matcher
import Parser

tests = TestLabel "Parser" $ TestList
    [ testExact
    , testDisallowed
    , testExample
    ]

testExact = TestLabel "Exact" $ TestList
    [ test (Right $ Desc [Exact "abc"] [])                                                                  "[+abc]"
    , test (Right $ Desc [Exact "ab]c"] [])                                                                 "[+ab\\]c]"
    , test (Left "\"matcher\" (line 1, column 6):\nunexpected end of input\nexpecting \"\\\\\" or \"]\"")   "[+abc"
    , test (Right $ Desc [Exact "abc"] [])                                                                  "123[+abc]456"
    , test (Right $ Desc [Exact "abc", Exact "123"] [])                                                     "[+abc][+123]"
    ] where
        test e s = (show s) ~: e ~=? parse s

testDisallowed = TestLabel "Disallowed" $ TestList
    [ test (Right $ Desc [] [Disallowed "abc"])                                                              "[-abc]"
    , test (Right $ Desc [] [Disallowed "ab]c"])                                                             "[-ab\\]c]"
    , test (Left "\"matcher\" (line 1, column 6):\nunexpected end of input\nexpecting \"\\\\\" or \"]\"")    "[-abc"
    , test (Left "\"matcher\" (line 1, column 7):\nunexpected 'd'\nexpecting end of input or \"[-\"")        "[-abc]def"
    , test (Right $ Desc [] [Disallowed "abc", Disallowed "def"])                                            "123[-abc][-def]"
    , test (Right $ Desc [Exact "abc"] [Disallowed "def"])                                                   "123[+abc][-def]"
    ] where
        test e s = (show s) ~: e ~=? parse s

testExample = TestLabel "Example" $ TestList
    [ test (Right $ Desc [Exact "(avdn/Panasonic.bd.pro4r.2014)"] [Disallowed"Opera"])   "Mozilla/1.2(compatible; U; InfiNet 2.3; Diga) AppleWebKit/3.4+ (KHTML, like Gecko)[+(avdn/Panasonic.bd.pro4r.2014)][-Opera]"
    ] where
        test e s = (show s) ~: e ~=? parse s

