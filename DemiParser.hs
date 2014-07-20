{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : DemiParser
Description : Parse a list of full Matchers: descriptions plus user data
-}

module DemiParser (
	DemiMatcher,
	parse
) where
import qualified Matcher as M
import qualified Parser as Parser
import qualified BestMatch as BM
import Data.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative

-- |One demi matcher
type DemiMatcher = BM.Matcher String

-- |Given an input demi file string, parse it into a list of full demi matchers
parse :: String -> (Either String [DemiMatcher])
parse t = P.parseOnly (P.many1 parseMatcher) $ pack t

parseMatcher :: P.Parser DemiMatcher
parseMatcher = do
	matcherStr <- P.many1 $ P.notChar '\t'
	P.char '\t'
	userdata <- P.many1 $ P.satisfy $ P.notInClass "\r\n"
	P.many' P.endOfLine
	return (fromRight' $ Parser.parse matcherStr, userdata)
		where
			fromRight' (Right r) = r
