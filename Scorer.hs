{-|
Module      : Scorer
Description : Score a matcher against a given string
-}

module Scorer (
    score
) where
import Data.List
import Data.Array
import Data.Maybe
import Text.Regex
import Matcher

-- |Score a given target string against a matcher. The lower the score, the closer the match.
score :: Desc -> MatchString -> Int
score (Desc fs _) t = editDistance full $ foldr normalise t fs
  where
    full = foldl' (\s f -> s ++ toString f) "" fs
    toString (Fuzzy s) = s
    toString (Exact s) = s
    toString (Version s) = s
    normalise (Version s) t = subRegex (mkRegexWithOpts (intersperse '\\' s ++ "[0-9._]+") False False) t s
    normalise _ t = t

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
