{-|
Module      : BestMatch
Description : Find the best match for given string
-}

module BestMatch (
	Matcher,
    match
) where
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Data.Ord
import qualified Matcher as M

-- |Matcher: Description of the matcher, plus some user data
type Matcher a = (M.Desc, a)

-- |Match a list of Matchers, returning the one that scored highest
match :: Show a => M.MatchString -> [Matcher a] -> Either String (Matcher a)
match s = result . filter (isJust . fst) . getScores
	where
		getScores = map (\m -> (M.score (fst m) s, m))
		result [] = Left "No matches"
		result (m:[]) = Right $ snd m
		result ms = Left $ "Multiple matches: " ++ (show $ map snd ms)

