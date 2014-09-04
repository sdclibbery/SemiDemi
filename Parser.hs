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
    Right r -> if M.empty r then Left $ "Empty matcher: " ++ t else return r

parseDesc :: Parser M.Desc
parseDesc = do
    fs <- many (parseInvariant Ap.<|> parseVersion Ap.<|> parseFuzzy)
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

parseInvariant :: Parser M.Flow
parseInvariant = do
    try $ string "[+"
    i <- parseEscaped ']'
    string "]"
    return $ M.Invariant i

parseVersion :: Parser M.Flow
parseVersion = do
    try $ string "[v"
    e <- parseEscaped ']'
    string "]"
    return $ M.Version (identifier e)
      where
        identifier = reverse . dropWhile isVersion . reverse
        isVersion = flip elem "0123456789._"

parseFuzzy :: Parser M.Flow
parseFuzzy = do
    f <- parseEscaped '['
    return $ M.Fuzzy f

parseDisallowed :: Parser M.Disallowed
parseDisallowed = do
    try $ string "[-"
    d <- parseEscaped ']'
    string "]"
    return $ M.Disallowed d

