module Matcher (
    Flow(..),
    Disallowed(..),
    Desc(..),
    score
) where
import Data.List
import Data.Array
import Data.Maybe
import Data.Char

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
        go s fs "" = return $ s - length fs
        go s (f:fs) t = do
            (s', t') <- case f of
                (Exact e) -> exact e t
                Version -> version t
                (Fuzzy f) -> return $ fuzzy f t
            go (s + s') fs t'

exact :: MatchString -> MatchString -> Maybe (Score, MatchString)
exact needle t = if isPrefixOf needle t' then Just (l - dropped, drop l t') else Nothing
    where
        t' = dropWhile (not . (== (head needle))) t
        dropped = length t - length t'
        l = length needle

version :: MatchString -> Maybe (Score, MatchString)
version t = if matched > 0 then Just (1 - dropped, t'') else Nothing
    where
        t' = dropWhile (not . isVersion) t
        t'' = dropWhile isVersion t'
        dropped = length t - length t'
        matched = length t' - length t''
        isVersion c = isDigit c || c == '.'

fuzzy :: MatchString -> MatchString -> (Score, MatchString)
-- Use lev over all prefixes; pick where editdistance is lowest
fuzzy needle t = (0, t)




editDistance :: (Eq a) => [a] -> [a] -> Int
editDistance xs ys = levMemo ! (n, m)
  where levMemo = array ((0,0),(n,m)) [((i,j),lev i j) | i <- [0..n], j <- [0..m]]
        n = length xs
        m = length ys
        xa = listArray (1, n) xs
        ya = listArray (1, m) ys
        lev 0 v = v
        lev u 0 = u
        lev u v
          | xa ! u == ya ! v = levMemo ! (u-1, v-1)
          | otherwise        = 1 + minimum [levMemo ! (u, v-1),
                                            levMemo ! (u-1, v),                                          levMemo ! (u-1, v-1)] 
