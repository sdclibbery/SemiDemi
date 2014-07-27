{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : DemiMaker
Description : Tool for creating a .demi file from a set of regression test cases
-}

import Data.Aeson
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Ord
import Data.Function
import Matcher
import Text.Regex
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

main = do
	cases <- readFile "../SemiDemiData/testdata/testdata.json"
	writeFile "../SemiDemiData/tvs.demi" $ (makeDemi cases)

makeDemi :: String -> String
makeDemi = unlines . map output . sort . nubBy ((==) `on` wurflId) . parseJson
	where
		output c = (markup (wurflId c) (refUa c)) ++ "\t" ++ wurflId c

data Case = Case { wurflId::String, refUa::String, testUa::String } deriving (Eq, Ord, Show)

parseJson :: String -> [Case]
parseJson = catMaybes . map (decode . BS.pack) . lines

instance FromJSON Case where
    parseJSON (Object v) = Case <$> v .: "wurfl_id" <*> v .: "user_agent" <*> v .: "uagent"
    parseJSON _          = mzero

markup :: String -> String -> String
markup wid = id -- markupExact wid . markupVersions

markupExact :: String -> String -> String
markupExact src s = foldr replace s parts
	where
		parts = filter (\p -> length p > 1) $ splitOneOf "_-" src
		replace r s = subRegex (mkRegexWithOpts ("(" ++ r ++ ")") False False) s "[+\\1]"

markupVersions :: String -> String
markupVersions s = subRegex r s "[v]"
	where
		r = mkRegex "[0-9]+(\\.[0-9]+)+"
