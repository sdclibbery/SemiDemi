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
    [ test   (Right $ ex "abc")   [ex "abc"]                             "abc"
    , test   (Right $ ex "abc")   [ex "abc", ex "def"]                   "abc"
    , test   (Right $ ex "abc")   [ex "def", ex "abc"]                   "abc"
    , test   (Left $ "Multiple matches: [(Desc [Exact \"ab\"] [],\"ab\"),(Desc [Exact \"bc\"] [],\"bc\")]")    [ex "ab", ex "bc"]                     "abc"
    ] where
        test e ms s = (show s ++ show ms) ~: e ~=? match s ms
        ex s = (M.Desc [M.Exact s] [], s)
