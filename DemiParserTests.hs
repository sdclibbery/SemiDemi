module DemiParserTests (
        tests
    ) where
import Test.HUnit
import Matcher
import DemiParser

tests = TestLabel "DemiParser" $ TestList
    [ testExample
    ]

testExample = TestLabel "Example" $ TestList
    [ test   (Right [(Desc [Fuzzy "abc"] [], "foo")])                                    "abc\tfoo"
    , test   (Right [(Desc [Fuzzy "abc"] [], "foo")])                                    "abc\tfoo\n"
    , test   (Right [(Desc [Fuzzy "abc"] [], "foo")])                                    "abc\tfoo\r\n"
    , test   (Right [(Desc [Fuzzy "abc"] [], "foo"), (Desc [Version] [], "bar")])        "abc\tfoo\n[v]\tbar"
    , test   (Right [(Desc [Fuzzy "abc"] [], "foo"), (Desc [Version] [], "bar")])        "abc\tfoo\r\n[v]\tbar\r\n"
    ] where
        test e s = (show s) ~: e ~=? parse s

