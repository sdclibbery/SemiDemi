{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : DemiParser
Description : Parse a list of full Matchers: descriptions plus user data
-}

module DemiParser (
    DemiMatcher,
    DemiParser.parse
) where
import qualified Matcher as M
import qualified Parser as P
import qualified BestMatch as BM
import qualified Data.ByteString.Char8 as BS
import Text.Parsec
import Text.Parsec.String
import qualified Data.List as L
import qualified Data.Either as E

-- |One demi matcher
type DemiMatcher = BM.Matcher String

-- |Given an input demi file string, parse it into a list of full demi matchers
parse :: String -> (Either String [DemiMatcher])
parse t = case (Text.Parsec.parse parseDemi "demi" t) of
    Left err -> Left $ show err
    Right ms -> return ms

parseDemi :: Parser [DemiMatcher]
parseDemi = do
    parseComments
    many $ do
        parseComments
        m <- parseMatcher
        parseComments
        return m

parseComments :: Parser ()
parseComments = skipMany $ do
    char '#'
    manyTill anyChar (newline <|> (eof >> return '\n'))
    optional eol
    return ()

parseMatcher :: Parser DemiMatcher
parseMatcher = do
    matcherStr <- many1 $ noneOf "\t"
    char '\t'
    userdata <- many1 $ noneOf "\r\n"
    eol
    case (P.parse matcherStr) of
        Left err -> fail err
        Right r -> return (r, userdata)

eol :: Parser ()
eol = do
    many $ oneOf "\r\n"
    return ()
