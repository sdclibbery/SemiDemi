{-# LANGUAGE OverloadedStrings #-}

module Parser (
	parse
) where
import qualified Matcher as M
import Data.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative

parse :: String -> (Either String M.Desc)
parse t = P.parseOnly parseDesc $ pack t

parseDesc :: P.Parser M.Desc
parseDesc = do
	fs <- P.many' (parseVersion <|> parseExact <|> parseFuzzy <|> parseFullFuzzy)
	ds <- P.manyTill (parseDisallowed <|> fail "Parse error") P.endOfInput
	return $ M.Desc fs ds

parseFuzzy :: P.Parser M.Flow
parseFuzzy = do
	f <- P.many1 $ P.notChar '|'
	return $ M.Fuzzy f

parseFullFuzzy :: P.Parser M.Flow
parseFullFuzzy = do
	P.string "|?"
	e <- P.many1 $ P.notChar '|'
	P.string "|"
	return $ M.FullFuzzy e

parseExact :: P.Parser M.Flow
parseExact = do
	P.string "|+"
	e <- P.many1 $ P.notChar '|'
	P.string "|"
	return $ M.Exact e

parseVersion :: P.Parser M.Flow
parseVersion = do
	P.string "|v|"
	return M.Version

parseDisallowed :: P.Parser M.Disallowed
parseDisallowed = do
	P.string "|-"
	d <- P.many1 $ P.notChar '|'
	P.string "|"
	return $ M.Disallowed d

