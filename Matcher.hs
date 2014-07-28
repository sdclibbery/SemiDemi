{-|
Module      : Matcher
Description : Define a matcher, and score it against a given string
-}

module Matcher (
    MatchString,
    Score,
    Flow(..),
    Disallowed(..),
    Desc(..),
    score,
    isVersion
) where
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Data.Ord
import qualified Data.MemoCombinators as Memo

-- |The type of all strings used in this module
type MatchString = String

-- |The type of a score given to a string after matching it against a matcher
type Score = Int

-- |A Matcher Description: how to match against a string
data Desc = Desc [Flow] [Disallowed] deriving (Show, Eq)

-- |Parts of a Matcher Description that must be matched in a sequence (flow)
data Flow = Fuzzy MatchString | FullFuzzy MatchString | Exact MatchString | Version deriving (Show, Eq)

-- |Disallowed parts that must not be in the string being matched
data Disallowed = Disallowed MatchString deriving (Show, Eq)

-- |Match a matcher description against a given string, returning a score.
-- |The higher the score, the better the match between the matcher and the string
score :: Desc -> MatchString -> Maybe Score
score (Desc fs ds) t = do
    d <- if any (\(Disallowed d) -> isInfixOf d t) ds then Nothing else Just 0
    r <- scoreFlow fs t
    return r

-- |Determine whether a Char is part of a version string
isVersion :: Char -> Bool
isVersion c = isDigit c || c == '.' || c == '_'

scoreFlow :: [Flow] -> MatchString -> Maybe Score
scoreFlow fs t = go 0 fs t
    where
        go s [] t = return $ s - length t
        go s (f:fs) t = do
            (s', t') <- case f of
                (Exact e) -> exact e t
                Version -> version t
                (Fuzzy f) -> return (0, t)
                (FullFuzzy f) -> return $ fullFuzzy f t
            go (s + s') fs t'

exact :: MatchString -> MatchString -> Maybe (Score, MatchString)
exact needle t = do
    (dropped, matched, t') <- findString needle t
    return (matched*3 - dropped, t') -- Triple points for exact matching

version :: MatchString -> Maybe (Score, MatchString)
version t = Just (1 - dropped, t'')
    where
        t' = dropWhile (not . isVersion) t
        t'' = dropWhile isVersion t'
        dropped = length t - length t'
        matched = length t' - length t''

fullFuzzy :: MatchString -> MatchString -> (Score, MatchString)
fullFuzzy needle t = (s, tr ++ remainder)
    where
        (_, (s, tr)) = fuzzyMatch (0, (-(length t' + length needle), "")) needle t'
        t' = take lim t -- Limit the amount of the string to match against; limits fuzziness of match, but runs faster
        remainder = drop lim t
        lim = max 10 $ 2 * length needle


-- Helpers

findString :: MatchString -> MatchString -> Maybe (Int, Int, MatchString)
findString needle t = case splits of
    (dropped:matched:ts) -> Just (length dropped, length matched, concat ts) -- !! Very inefficient
    _ -> Nothing
    where
        splits = split (onSublist needle) t

-- Full levenshtein style fuzzy matching. Good but slow.
-- Keep score, matching along the way. Like a modified Levenshtein algorithm.
-- Returns the score and the remaining leftover string from the best match.
-- When no match is found, have to try discarding a char from each string and recursively see which worked better.
-- Score +1 for a matching char, and -1 for every char that has to be discarded.
fuzzyMatch :: FuzzyState -> String -> String -> FuzzyState
fuzzyMatch = fuzzyMatchMemo
    where
        fuzzyMatchMemo = Memo.memo3 memoState memoString memoString go
        go r [] [] = r
        go (s, b) [] ts  = (s - length ts, b)
        go (s, b) ns []  = (s - length ns, b)
        go (s, b) (n:ns) (t:ts)
            | n == t = fuzzyMatchMemo (s + 1, (s + 1 - length ns, ts)) ns ts -- !! Inefficient! calcing remaining length of ns on every match
            | otherwise = maximumBy (comparing fst) [
                    fuzzyMatchMemo (s - 1, b) ns (t:ts),
                    fuzzyMatchMemo (s - 1, b) (n:ns) ts
                ]
memoString = Memo.list Memo.char
memoInt = Memo.integral
type FuzzyState = (Int, (Int, String))
memoState = Memo.pair memoInt (Memo.pair memoInt memoString)
