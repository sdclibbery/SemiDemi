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
markup wid s = foldr replace s replacements
	where
		replace r s = subRegex (mkRegexWithOpts (fst r) False False) s (snd r)
		replacements = [
				  version "Opera/"
				, version "Presto/"
				, exact "3view.stb.2010"
				, version "Mozilla/"
				, version "AppleWebKit/"
				, version "kazoku/"
				, version "Safari/"
				, exact "ADB"
				, exact "ATSCE"
				, version "ATSCE/"
				, version "NETTV/"
				, version "HbbTV/"
				, exact "BANGOLUFSEN;A3"
				, exact "BANGOLUFSEN; A3TEST"
				, exact "BBC-Forge-URL-Monitor-Twisted"
				, version "rv:"
				, version "HTML/"
				, version "OreganMediaBrowser/"
				, version "Gecko/"
				, version "Firefox/"
				, exact "bt_cardinal.stb.2011"
				, exact "BT-Cardinal-G2512/4.30.1"
				, exact "BT-Cardinal-G2512/4.30.2"
				, exact "BT-Cardinal-G2256/4.30.2"
				, exact "BT-Cardinal-G21024/4.30.2"
				, exact "BT-Cardinal-G2512/4.30.3"
				, exact "BT-Cardinal-G2256/4.30.3"
				, exact "BT-Cardinal-G21024/4.30.3"
				, version "Opera TV Store/"
				, version "Version/"
				, exact "cambridge/752BD"
				, exact "C32Z18FIPTV"
				, exact "Brand bush"
				, exact "Cello"
				, exact "CVT-IDTVTESTUA"
				, version "Revision:"
				, version "NetFront/"
				, exact "electra.stb.2011"
				, version "Qt/"
				, exact "Eminent EM7285"
				, version "Dalvik/"
				, version "Android "
				, exact "GameStick"
				, exact "ANTGalio"
				, version "ANTGalio/"
				, version "Linux"
				, exact "GV102ZRH"
				, exact "GEEYA"
				, exact "VW11FVRHD"
				, version "stm"
				, version "SDK"
				, version "PATCH/"
				, exact "Hisense"
				, exact "hitachi-v.tv.2011"
				, version "Linux "
				, exact "Humax"
				, exact "HDR-FOX T2"
				, exact "HD-FOX T2"
				, exact "HDR-5100S"
				, exact "hdr1000s"
				, exact "Freesat"
				, version "Freesat/"
				, exact "ce-html"
				, exact "hb1000s"
				, exact "U;"
				, exact "IcecryptT2300HD"
				, version "LG Browser/"
				, version "BD."
				, exact "LG NetCast.Media-2011"
				, exact "BLURAY 2014"
				, exact "HIGHEND"
				, version "armv"
				, exact "LGE"
				, version "NetCast "
				, exact "1.0L"
				, exact "1.0M"
				, exact "LG.Media-2010"
				, version "BR."
				, exact "LG.Media-2010"
				, exact "Media/HR[0-9]{3}"
				, exact "avdn/LG"
				, exact "Media/B[A-Z][0-9]{3,4}[A-Z]{0,2};"
				, exact "BLURAY 2013"
			]
		version s = ("(" ++ s ++ ")[0-9._]+", "\\1[v]")
		exact s = ("(" ++ s ++ ")", "[+\\1]")
