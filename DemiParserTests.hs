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
    [ test   (Right [(Desc [Exact "abc"] [], "foo")])                                        "[+abc]\tfoo"
    , test   (Right [(Desc [Exact "abc"] [], "foo")])                                        "[+abc]\tfoo\n"
    , test   (Right [(Desc [Exact "abc"] [], "foo")])                                        "[+abc]\tfoo\r\n"
    , test   (Right [(Desc [Exact "abc"] [], "foo"), (Desc [Exact "def"] [], "bar")])        "[+abc]\tfoo\n[+def]\tbar"
    , test   (Right [(Desc [Exact "abc"] [], "foo"), (Desc [Exact "def"] [], "bar")])        "[+abc]\tfoo\r\n[+def]\tbar\r\n"
    , test   (Left "\"demi\" (line 2, column 1):\nunexpected end of input\n\"matcher\" (line 1, column 4):\nunexpected \"c\"")     "ab\\c\tfoo\r\n"
    ] where
        test e s = (show s) ~: e ~=? parse s

