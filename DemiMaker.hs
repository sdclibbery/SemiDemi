{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : DemiMaker
Description : Tool for creating a .demi file from a set of regression test cases
-}

import Data.Aeson
import Data.Maybe
import Data.List
import Data.Ord
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

main = do
	cases <- readFile "../SemiDemiData/testdata/testdata.json"
	writeFile "../SemiDemiData/tvs.demi" $ process cases

process :: String -> String
process = unlines . map output . sort . nubBy idsMatch . parse
	where
		idsMatch l r = wurflId l == wurflId r
		output c = refUa c ++ "\t" ++ wurflId c

data Case = Case { wurflId::String, refUa::String, testUa::String } deriving (Eq, Ord, Show)

parse :: String -> [Case]
parse = catMaybes . map (decode . BS.pack) . lines

instance FromJSON Case where
    parseJSON (Object v) = Case <$> v .: "wurfl_id" <*> v .: "user_agent" <*> v .: "uagent"
    parseJSON _          = mzero
