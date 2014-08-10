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
				-- Matchers for entire UAs
				  exact "DO_NOT_MATCH_GENERIC_SMARTTV"
				, exact "Opera/9.99 \\(Linux_SDK; test\\)Linux_SDK/1.0 Version/9.99"
				, exact "Opera/10.60 \\(Linux armv6l ; U; CE-HTML/1.0 NETTV/3.0.1;; en\\) Presto/2.6.33 Version/10.60"
				, exact "Opera/9.80 \\(Linux armv6l ; U; HbbTV/1.1.1 \\(; ; ; ; ; \\) CE-HTML/1.0 NETTV/3.2.1; en\\) Presto/2.6.33 Version/10.60"
				-- End Matchers for entire UAs
				, exact "3view.stb.2010"
				, exact "ADB"
				, exact "ATSCE"
				, exact "BANGOLUFSEN"
				, exact "A3;"
				, exact "A3TEST;"
				, exact "BBC-Forge-URL-Monitor-Twisted"
				, exact "bt_cardinal.stb.2011"
				, exact "BT-Cardinal-G2512/4.30.1"
				, exact "BT-Cardinal-G2512/4.30.2"
				, exact "BT-Cardinal-G2256/4.30.2"
				, exact "BT-Cardinal-G21024/4.30.2"
				, exact "BT-Cardinal-G2512/4.30.3"
				, exact "BT-Cardinal-G2256/4.30.3"
				, exact "BT-Cardinal-G21024/4.30.3"
				, exact "cambridge/752BD"
				, exact "C32Z18FIPTV( Cello)?"
				, exact "Brand bush"
				, exact "CVT-IDTVTESTUA"
				, exact "electra.stb.2011"
				, exact "Eminent EM7285"
				, exact "GameStick"
				, exact "GV102ZRH"
				, exact "GEEYA"
				, exact "VW11FVRHD"
				, exact "Hisense"
				, exact "hitachi-v.tv.2011"
				, exact "Humax"
				, exact "HDR-FOX T2"
				, exact "HD-FOX T2"
				, exact "HDR-5100S"
				, exact "hdr1000s"
				, exact "LG UA"
				, exact "hb1000s"
				, exact "IcecryptT2300HD"
				, exact "LG NetCast.Media-2011"
				, exact "BLURAY 2014"
				, exact "HIGHEND"
				, exact "MIDRANGE"
				, exact ";LGE"
				, exact ";1.0L"
				, exact ";1.0M"
				, exact "LG.Media-2010"
				, exact "Media/HR[0-9]{3}"
				, exact "Media/B[A-Z][0-9]{3,4}[A-Z]{0,2};?"
				, exact "BLURAY 2013"
				, exact "MEDIASTREAMER"
				, exact "LG NetCast.TV-[0-9]{4}"
				, exact "avdn/LG.TV-2011"
				, exact "LGE; ?42LE7500-ZA;"
				, exact "avrt/html4 avst/off"
				, exact "Manhattan[;,] HS8300"
				, exact "(HDT2)"
				, exact "MathEmbedded"
				, exact "MEDIATEK TV"
				, exact "Motorola"
				, exact "KreaTV"
				, exact "Ekioh"
				, exact "Netgear-NeoTV-NTV[0-9]{3}"
				, exact "Netbox"
				, exact "Nintendo Wii; U; ; [0-9]{4}"
				, exact "Nintendo WiiU"
				, exact "Roku/DVP-[0-9]{4}"
				, exact "Opera TV/Blinktest"
				, exact "Opera TV Store/[0-9]{4}"
				, exact "OPPO BDP-10X"
				, exact "OreganMediaBrowser"
				, exact "Onyx/CelloTV"
				, exact "OUYA"
				, exact "Panasonic.bd.[0-9]{4}"
				, exact "Panasonic.bd.pro4[a-z].[0-9]{4}"
				, exact "Panasonic.bd.mtk2d.[0-9]{4}"
				, exact "Mediatek"
				, exact "Panasonic.bd.mtk3d[0-9].[0-9]{4}"
				, exact "Panasonic.tv.(high|low|mid|volume)?.?[0-9]{4}"
				, exact "Viera"
				, exact "SAGEMCOM"
				, exact "HDSP"
				, exact "PHILIPS-AVM-2012"
				, exact "Philips, BDP[0-9]{4}[A-Z]?,"
				, exact "Philips, HMP7100"
				, exact "Philips; ; ; PHILIPSTV;"
				, exact "PHILIPS_OLS_2010"
				, exact "Philips; ; ; ; "
				, exact "Philips;;;;"
				, exact "Philips;MT55[0-9]{2};"
				, exact "Pure-VL61770"
				, exact "DVP-[0-9]{4}EU"
				, exact "PHILIPSTV/"
				, exact "RSIW98"
				, exact "RTI95"
				, exact "SMART-TV"
				, exact "Maple [0-9.]{1,10}"
				, exact "Series=[0-9]{4}\\(BD\\)"
				, exact "Series=[0-9]{4}\\(HT\\)"
				, exact "Series=[0-9]{4}\\(TV\\)"
				, exact "Series=[0-9]{4}\\(STB\\)"
				, exact "Smart-BD"
				, exact "HT-[A-Z][0-9]{4};"
				, exact "BD-[A-Z][0-9]{4};"
				, exact "B-HM5160"
				, exact "Samsung"
				, exact "Fox[A-Z]"
				, exact "GOLF[A-Z]"
				, exact "X1[0-9]"
				, exact "NT14U"
				, exact "AquosTV"
				, exact "Sharp"
				, exact "LE[0-9]{3};"
				, exact "smartbridge content provider"
				, exact "sony.bdp.[0-9]{4}"
				, exact "BDP[0-9][A-Z](_[A-Z]X[A-Z]?)?"
				, exact "Version/10/30"
				, exact "sony.hbbtv.tv.[0-9]{4}"
				, exact "sony.tv.[0-9]{4}"
				, exact "Tesco"
				, exact "Technika Media Streamer"
				, exact "Technika22-212i"
				, exact "SureSoftBrowser"
				, exact "TECHNIKATK500SDTR212"
				, exact "TOSHIBA; BDX2250KB;"
				, exact "Toshiba.tv.[0-9]{4}"
				, exact "[0-9]{4}-BDX(-[23]D)?"
				, exact "avst/on"
				, exact "avst=off"
				, exact "TOSHIBA-[0-9]{4}[A-Z]?-RL/TL(/[VW]L)?"
				, exact "TOSHIBA-DTV-2014A"
				, exact "GMI-SS-Browser"
				, exact "trident.tv.[0-9]{4}"
				, exact "OPPO PLAYER BDP-10[35]EU"
				, exact "Panasonic.bd[23]d.2013"
				, exact "Panasonic.tv.(mtk|pro4).2013"
				, exact "BDV6G_[EN]J"
				, exact "BDP2014"
				, exact "TOSHIBA-2013"
				, exact "verismobrowser/ Verismo-BlackUI"
				, exact "VESTEL.2013-v.stb.2013"
				, exact "(VESTEL.[0-9]{4})?-v.stb.[0-9]{4}"
				, exact "[A-Z]{0,9}-v.tv.[0-9]{4}"
				, exact "GV101YRH"
				, exact "WDSimpleBrowser"
				, exact "Maple[0-9]{4}"
				, exact "Westerndigital/Streamer2010"
				, exact "YouViewHTML"
				, exact "Loewe; SL[0-9]{3};"
			]
		exact s = ("(" ++ s ++ ")", "[+\\1]")
