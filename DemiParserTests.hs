module DemiParserTests (
        tests
    ) where
import Test.HUnit
import Matcher
import DemiParser

tests = TestLabel "DemiParser" $ TestList
    [ testComments
    , testExample
    ]

testComments = TestLabel "Comments" $ TestList
    [ test   (Right [])                                                                "# abc"
    , test   (Right [])                                                                "# abc\n"
    , test   (Right [])                                                                "# abc\n# 123"
    , test   (Right [(Desc [Invariant "def"] [], "foo")])                                  "# abc\n# 123\n[+def]\tfoo"
    , test   (Right [(Desc [Invariant "def"] [], "foo")])                                  "# abc\n\n# 123\n\n[+def]\tfoo"
    , test   (Right [(Desc [Invariant "def"] [], "foo")])                                  "[+def]\tfoo\n# abc"
    , test   (Right [(Desc [Invariant "def"] [], "foo"), (Desc [Invariant "ghi"] [], "bar")])  "# abc\n[+def]\tfoo\n# abc\n[+ghi]\tbar\n# abc"
    , test   (Right [(Desc [Invariant "def"] [], "foo"), (Desc [Invariant "ghi"] [], "bar")])  "# abc\r\n\r\n[+def]\tfoo\r\n\r\n# abc\r\n\r\n[+ghi]\tbar\r\n\r\n# abc"
    ] where
        test e s = (show s) ~: e ~=? parse s

testExample = TestLabel "Example" $ TestList
    [ test   (Right [(Desc [Invariant "abc"] [], "foo")])                                        "[+abc]\tfoo"
    , test   (Right [(Desc [Invariant "abc"] [], "foo")])                                        "[+abc]\tfoo\n"
    , test   (Right [(Desc [Invariant "abc"] [], "foo")])                                        "[+abc]\tfoo\r\n"
    , test   (Right [(Desc [Invariant "abc"] [], "foo"), (Desc [Invariant "def"] [], "bar")])        "[+abc]\tfoo\n\n\n[+def]\tbar"
    , test   (Right [(Desc [Invariant "abc"] [], "foo"), (Desc [Invariant "def"] [], "bar")])        "[+abc]\tfoo\r\n[+def]\tbar\r\n"
    , test   (Left "\"demi\" (line 2, column 1):\nunexpected end of input\n\"matcher\" (line 1, column 4):\nunexpected \"c\"")     "ab\\c\tfoo\r\n"
    ] where
        test e s = (show s) ~: e ~=? parse s

