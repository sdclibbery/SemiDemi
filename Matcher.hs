{-|
Module      : Matcher
Description : Define a matcher, and score it against a given string
-}

module Matcher (
    MatchString,
    Flow(..),
    Disallowed(..),
    Desc(..),
    empty,
    matches,
    score
) where
import Data.List
import Data.Array
import Data.Maybe
import Text.Regex

-- |The type of all strings used in this module
type MatchString = String

-- |A Matcher Description: how to match against a string
data Desc = Desc [Flow] [Disallowed] deriving (Show, Eq)

-- |Parts of a Matcher Description that must be matched exactly
data Flow = Exact MatchString | Fuzzy MatchString | Version MatchString MatchString deriving (Show, Eq)

-- |Disallowed parts that must not be in the string being matched
data Disallowed = Disallowed MatchString deriving (Show, Eq)

-- |Test whether a matcher is empty (contains no match elements)
empty :: Desc -> Bool
empty (Desc es ds) = (null $ filter (not.isFuzzy) es) && null ds
    where
        isFuzzy (Fuzzy _) = True
        isFuzzy _ = False

-- |See if a given matcher matches a given target string
matches :: Desc -> MatchString -> Bool
matches (Desc fs ds) t = disallowed && exact
    where
        disallowed = if any (\(Disallowed d) -> isInfixOf d t) ds then False else True
        exact = if all matchExact fs then True else False
        matchExact (Exact e) = isInfixOf e t
        matchExact _ = True

-- |Score a given target string against a matcher. The lower the score, the closer the match.
score :: Desc -> MatchString -> Float
score (Desc fs _) t = (majorDistance t fs) + (minorDistance t fs)

majorDistance :: MatchString -> [Flow] -> Float
majorDistance t fs = fromIntegral $ editDistance full $ foldr normalise t fs
  where
    full = foldl' (\s f -> s ++ toString f) "" fs
    toString (Fuzzy s) = s
    toString (Exact s) = s
    toString (Version s _) = s
    normalise (Version s _) t = subRegex (mkRegexWithOpts (intersperse '\\' s ++ "[0-9._]+") False False) t s
    normalise _ t = t

minorDistance :: MatchString -> [Flow] -> Float
minorDistance t fs = foldr score 0.0 fs
  where
    score (Version s v) sc = sc + (fromIntegral $ editDistance v version) / (fromIntegral $ max (length v) (length version))
        where
            version = head $ fromJust $ matchRegex (mkRegexWithOpts (intersperse '\\' s ++ "([0-9._]+)") False False) t
    score _ sc = sc

editDistance :: (Eq a) => [a] -> [a] -> Int
editDistance xs ys = levMemo ! (n, m)
  where
    levMemo = array ((0,0),(n,m)) [((i,j),lev i j) | i <- [0..n], j <- [0..m]]
    n = length xs
    m = length ys
    xa = listArray (1, n) xs
    ya = listArray (1, m) ys
    lev 0 v = v
    lev u 0 = u
    lev u v
      | xa ! u == ya ! v = levMemo ! (u-1, v-1)
      | otherwise        = 1 + minimum [levMemo ! (u, v-1),
                                        levMemo ! (u-1, v),
                                        levMemo ! (u-1, v-1)] 
