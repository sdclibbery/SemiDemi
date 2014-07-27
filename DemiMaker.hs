{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : DemiMaker
Description : Tool for creating a .demi file from a set of regression test cases
-}

import Data.Aeson
import Data.Maybe
import Data.List
import Data.Ord
import Data.Function
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

main = do
	cases <- readFile "../SemiDemiData/testdata/testdata.json"
	writeFile "../SemiDemiData/tvs.demi" $ makeDemi cases

makeDemi :: String -> String
makeDemi = unlines . map output . sort . nubBy ((==) `on` wurflId) . parseJson
	where
		output c = refUa c ++ "\t" ++ wurflId c

data Case = Case { wurflId::String, refUa::String, testUa::String } deriving (Eq, Ord, Show)

parseJson :: String -> [Case]
parseJson = catMaybes . map (decode . BS.pack) . lines

instance FromJSON Case where
    parseJSON (Object v) = Case <$> v .: "wurfl_id" <*> v .: "user_agent" <*> v .: "uagent"
    parseJSON _          = mzero
