{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Parser
Description : Parse a Matcher description from a markes up input string
-}

module Parser (
	Parser.parse
) where
import qualified Matcher as M
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import qualified Control.Applicative as Ap

-- |Given a marked up matcher string, parse it into a Matcher Desc structure.
parse :: String -> (Either String M.Desc)
parse t = case (Text.Parsec.parse parseDesc "matcher" t) of
	Left err -> Left $ show err
	Right r -> return r

parseDesc :: Parser M.Desc
parseDesc = do
	fs <- many (parseExact Ap.<|> parseFuzzy)
	ds <- manyTill parseDisallowed eof
	return $ M.Desc fs ds

parseEscaped :: Char -> Parser String
parseEscaped c = do
	ss <- many1 $ escaped Ap.<|> nonEscaped
	return $ Prelude.concat ss
	where
		nonEscaped = many1 $ noneOf ['\\', c]
		escaped = do
			char '\\'
			c <- oneOf "\\[]"
			return [c]

parseFuzzy :: Parser M.Flow
parseFuzzy = do
	f <- parseEscaped '['
	return $ M.Fuzzy f

parseExact :: Parser M.Flow
parseExact = do
	try $ string "[+"
	e <- parseEscaped ']'
	string "]"
	return $ M.Exact e

parseDisallowed :: Parser M.Disallowed
parseDisallowed = do
	try $ string "[-"
	d <- parseEscaped ']'
	string "]"
	return $ M.Disallowed d

