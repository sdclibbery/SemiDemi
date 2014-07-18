module ParserTests (
        tests
    ) where
import Test.HUnit
import Matcher
import Parser

tests = TestLabel "Parser" $ TestList
    [ testFuzzy
    , testFullFuzzy
    , testVersion
    , testExact
    , testDisallowed
    , testExample
    ]

testFuzzy = TestLabel "Fuzzy" $ TestList
    [ test (Right $ Desc [] [])                                       ""
    , test (Right $ Desc [Fuzzy "abc"] [])                            "abc"
    , test (Right $ Desc [Fuzzy "abc (-) +hello v good.!!!"] [])      "abc (-) +hello v good.!!!"
    ] where
        test e s = (show s) ~: e ~=? parse s

testFullFuzzy = TestLabel "FullFuzzy" $ TestList
    [ test (Right $ Desc [FullFuzzy "abc"] [])                                         "|?abc|"
    , test (Right $ Desc [Fuzzy "123", FullFuzzy "abc (-) +hello v good.!!!"] [])      "123|?abc (-) +hello v good.!!!|"
    ] where
        test e s = (show s) ~: e ~=? parse s

testVersion = TestLabel "Version" $ TestList
    [ test (Right $ Desc [Version] [])                            "|v|"
    , test (Left "Failed reading: Parse error")                   "|v"
    , test (Left "Failed reading: Parse error")                   "v|"
    , test (Right $ Desc [Fuzzy "abc", Version] [])               "abc|v|"
    , test (Right $ Desc [Version, Fuzzy "abc"] [])               "|v|abc"
    , test (Right $ Desc [Fuzzy "abc", Version, Fuzzy "abc"] [])  "abc|v|abc"
    , test (Right $ Desc [Fuzzy "abc", Version, Fuzzy "def", Version, Fuzzy "ghi"] [])  "abc|v|def|v|ghi"
    ] where
        test e s = (show s) ~: e ~=? parse s

testExact = TestLabel "Exact" $ TestList
    [ test (Right $ Desc [Exact "abc"] [])                              "|+abc|"
    , test (Left "Failed reading: Parse error")                         "|+abc"
    , test (Right $ Desc [Fuzzy "123", Exact "abc", Fuzzy "456"] [])    "123|+abc|456"
    , test (Right $ Desc [Exact "abc", Exact "123"] [])                 "|+abc||+123|"
    ] where
        test e s = (show s) ~: e ~=? parse s

testDisallowed = TestLabel "Disallowed" $ TestList
    [ test (Right $ Desc [] [Disallowed "abc"])                                     "|-abc|"
    , test (Left "Failed reading: Parse error")                                     "|-abc"
    , test (Left "Failed reading: Parse error")                                     "|-abc|def"
    , test (Right $ Desc [Fuzzy "123"] [Disallowed "abc", Disallowed "def"])        "123|-abc||-def|"
    , test (Right $ Desc [Fuzzy "123", Version, Exact "abc"] [Disallowed "def"])    "123|v||+abc||-def|"
    ] where
        test e s = (show s) ~: e ~=? parse s

testExample = TestLabel "Example" $ TestList
    [ test (Right $ Desc [ Fuzzy "Mozilla/"
                , Version
                , Fuzzy "(compatible; U; InfiNet "
                , Version
                , Fuzzy "; Diga) AppleWebKit/"
                , Version
                , Fuzzy "+ (KHTML, like Gecko)"
                , Exact "(avdn/Panasonic.bd.pro4r.2014)"
            ] [Disallowed"Opera"])   "Mozilla/|v|(compatible; U; InfiNet |v|; Diga) AppleWebKit/|v|+ (KHTML, like Gecko)|+(avdn/Panasonic.bd.pro4r.2014)||-Opera|"
    ] where
        test e s = (show s) ~: e ~=? parse s

