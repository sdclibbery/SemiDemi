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
	putStrLn $ process target demi

process :: String -> String -> String
process target demi = case (parse demi) of
	(Right ms) -> snd $ match target ms
	(Left err) -> err
