module BestMatchTests (
        tests
    ) where
import Test.HUnit
import qualified Matcher as M
import BestMatch

tests = TestLabel "BestMatch" $ TestList
    [ testBlah
    ]

testBlah = TestLabel "Blah" $ TestList
    [ test   (ex "abc")   [ex "abc"]                             "abc"
    , test   (ex "abc")   [ex "abc", ex "def"]                   "abc"
    , test   (ex "abc")   [ex "def", ex "abc"]                   "abc"
    , test   (fz "abc")   [fz "def", fz "abc"]                   "abc"
    , test   (fz "abz")   [fz "azz", fz "abz"]                   "abc"
    , test   (fz "azz")   [ex "abz", fz "azz"]                   "abc"
    ] where
        test e ms s = (show s ++ show ms) ~: e ~=? match s ms
        ex s = (M.Desc [M.Exact s] [], s)
        fz s = (M.Desc [M.Fuzzy s] [], s)
