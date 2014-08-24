{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Regression
Description : Tool for running the Demi regression suite against the SemiDemi matcher
-}

import DemiParser
import BestMatch
import qualified Matcher as M
import Data.Either
import Data.Aeson
import Data.Maybe
import Data.List
import Data.Ord
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

main = do
    demi <- readFile "../SemiDemiData/tvs.demi"
    tests <- readFile "../SemiDemiData/testdata/tvs.txt"
    expected <- readFile "../SemiDemiData/testdata/testdata.json"
    putStrLn $ regression demi expected tests


regression :: String -> String -> String -> String
regression d e t = case (parse d) of
    (Left err) -> err
    (Right ms) -> (foldr (++) "" $ map (either (++ "\n\n") id) $ results) ++ "\nTotal: " ++ (show $ length ts) ++ "\n Failed: " ++ (show $ length $ lefts $ results)
        where
            ts = lines t
            es = map parseJson $ lines e
            results = map (test ms es) $ zip ts [0..]

test :: [Matcher String] -> [String] -> (String, Int) -> Either String String
test ms es (t, i) = if e == snd m then Right $ "" else Left $ "\n\nFAIL (" ++ (show i) ++ "): " ++ t ++ "\n\nEXPECTED: " ++ (showExpected e) ++ "\n\nGOT: " ++ (show m) ++ "\n"
    where
        m = matchWithDefault (M.Desc [] [], "generic_smarttv_browser") t ms
        e = es !! i
        showExpected e = show $ head $ filter (\m -> snd m == e) ms

matchWithDefault :: Matcher String -> String -> [Matcher String] -> Matcher String
matchWithDefault def t ms = case match t ms of
    (Left err) -> def
    (Right m) -> m

data Case = Case { wurflId::String } deriving (Eq, Ord, Show)

parseJson :: String -> String
parseJson = wurflId . fromJust . decode . BS.pack

instance FromJSON Case where
    parseJSON (Object v) = Case <$> v .: "wurfl_id"
    parseJSON _          = mzero
