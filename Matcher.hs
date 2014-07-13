module Matcher (
    Flow(..),
    Disallowed(..),
    Desc(..),
    score
) where
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Data.Ord
import qualified Data.MemoCombinators as Memo

type MatchString = String
type Score = Int

data Flow = Fuzzy MatchString | Exact MatchString | Version deriving (Show, Eq)
data Disallowed = Disallowed MatchString deriving (Show, Eq)

data Desc = Desc [Flow] [Disallowed] deriving (Show, Eq)

score :: Desc -> MatchString -> Maybe Score
score (Desc fs ds) t = do
    d <- if any (\(Disallowed d) -> isInfixOf d t) ds then Nothing else Just 0
    r <- scoreFlow fs t
    return r

scoreFlow :: [Flow] -> MatchString -> Maybe Score
scoreFlow fs t = go 0 fs t
    where
        go s [] t = return $ s - length t
        go s (f:fs) t = do
            (s', t') <- case f of
                (Exact e) -> exact e t
                Version -> version t
                (Fuzzy f) -> return $ fuzzy f t
            go (s + s') fs t'

exact :: MatchString -> MatchString -> Maybe (Score, MatchString)
exact needle t = do
    (dropped, matched, t') <- findString needle t
    return (matched - dropped, t')

version :: MatchString -> Maybe (Score, MatchString)
version t = if matched > 0 then Just (1 - dropped, t'') else Nothing
    where
        t' = dropWhile (not . isVersion) t
        t'' = dropWhile isVersion t'
        dropped = length t - length t'
        matched = length t' - length t''
        isVersion c = isDigit c || c == '.'

fuzzy :: MatchString -> MatchString -> (Score, MatchString)
fuzzy needle t = (s, t')
    where
        (_, (s, t')) = fuzzyMatch (0, (-(length t + length needle), "")) needle t

-- Helpers

-- !! VERY inefficient
findString :: MatchString -> MatchString -> Maybe (Int, Int, MatchString)
findString needle t = case splits of
    (dropped:matched:ts) -> Just (length dropped, length matched, concat ts)
    _ -> Nothing
    where
        splits = split (onSublist needle) t

memoString = Memo.list Memo.char
memoInt = Memo.integral
type FuzzyState = (Int, (Int, String))
memoState = Memo.pair memoInt (Memo.pair memoInt memoString)

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
            | n == t = fuzzyMatchMemo (s + 1, (s + 1 - length ns, ts)) ns ts -- !! Inefficient! calcing length of ns on every match
            | otherwise = maximumBy (comparing fst) [
                    fuzzyMatchMemo (s - 1, b) ns (t:ts),
                    fuzzyMatchMemo (s - 1, b) (n:ns) ts
                ]

