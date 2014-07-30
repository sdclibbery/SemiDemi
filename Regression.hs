{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Regression
Description : Tool for running the Demi regression suite against the SemiDemi matcher
-}

import DemiParser
import BestMatch
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
	(Right ms) -> (foldr (\a b -> a ++ "\n" ++ b) "" $ failures $ ms) ++ "\nTotal: " ++ (show $ length ts) ++ "\nFailed: " ++ (show $ length $ failures ms)
	(Left err) -> err
	where
		ts = lines t
		es = lines e
		failures ms = lefts $ map (test ms es) $ zip ts [0..]

test :: [Matcher String] -> [String] -> (String, Int) -> Either String String
test ms es (t, i) = do
	m <- match t ms
	if e == snd m then Right "" else Left $ "\nFAIL: " ++ t ++ "\n    EXPECTED: " ++ e ++ "\n         GOT: " ++ (snd m)
	where
		e = parseJson $ es !! i

data Case = Case { wurflId::String } deriving (Eq, Ord, Show)

parseJson :: String -> String
parseJson = wurflId . fromJust . decode . BS.pack

instance FromJSON Case where
    parseJSON (Object v) = Case <$> v .: "wurfl_id"
    parseJSON _          = mzero
