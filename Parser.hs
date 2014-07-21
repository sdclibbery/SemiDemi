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
	Right r -> Right r

parseDesc :: Parser M.Desc
parseDesc = do
	fs <- many (parseVersion Ap.<|> parseExact Ap.<|> parseFullFuzzy Ap.<|> parseFuzzy)
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

parseFullFuzzy :: Parser M.Flow
parseFullFuzzy = do
	try $ string "[?"
	e <- parseEscaped ']'
	string "]"
	return $ M.FullFuzzy e

parseExact :: Parser M.Flow
parseExact = do
	try $ string "[+"
	e <- parseEscaped ']'
	string "]"
	return $ M.Exact e

parseVersion :: Parser M.Flow
parseVersion = do
	try $ string "[v]"
	return M.Version

parseDisallowed :: Parser M.Disallowed
parseDisallowed = do
	try $ string "[-"
	d <- parseEscaped ']'
	string "]"
	return $ M.Disallowed d

