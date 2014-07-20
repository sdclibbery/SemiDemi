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
match :: M.MatchString -> [Matcher a] -> Matcher a
match s = snd . pickBest . getScores
	where
		getScores = map (\m -> (M.score (fst m) s, m))
		pickBest = maximumBy (comparing fst) . filter (isJust . fst)
