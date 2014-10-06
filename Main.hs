{-|
Module      : Main
Description : Main SemiDemi app: takes a user agent string, reads a.demi file, and outputs the wurfl id matching the user agent string.
-}

import System.Environment
import DemiParser
import BestMatch

main = do
	[target] <- getArgs
	demi <- readFile "../SemiDemiData/tvs.demi"
	putStrLn $ either id id $ process target demi

process :: String -> String -> Either String String
process target demi = do
	ms <- parse demi
	m <- match target ms
	return $ snd m
