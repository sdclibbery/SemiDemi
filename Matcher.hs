{-|
Module      : Matcher
Description : Define a matcher, and match it against a given string
-}

module Matcher (
    MatchString,
    Flow(..),
    Disallowed(..),
    Desc(..),
    empty,
    matches
) where
import Data.List
import Data.Maybe

-- |The type of all strings used in this module
type MatchString = String

-- |A Matcher Description: how to match against a string
data Desc = Desc [Flow] [Disallowed] deriving (Eq)

-- |Parts of a Matcher Description that must be matched in order
data Flow = Invariant MatchString | Fuzzy MatchString | Version MatchString deriving (Eq)

-- |Disallowed parts that must not be in the string being matched
data Disallowed = Disallowed MatchString deriving (Eq)

instance Show Desc where
    show (Desc fs ds) = concatMap show fs ++ concatMap show ds

instance Show Flow where
    show (Invariant i) = "[+" ++ i ++ "]"
    show (Fuzzy f) = f
    show (Version v) = "[v" ++ v ++ "000]"

instance Show Disallowed where
    show (Disallowed d) = "[-" ++ d ++ "]"

-- |Test whether a matcher is empty (contains no match elements)
empty :: Desc -> Bool
empty (Desc fs ds) = (null $ filter (not.isFuzzy) fs) && null ds
    where
        isFuzzy (Fuzzy _) = True
        isFuzzy _ = False

-- |See if a given matcher matches a given target string
matches :: Desc -> MatchString -> Bool
matches (Desc fs ds) t = disallowed && invariant
    where
        disallowed = if any (\(Disallowed d) -> isInfixOf d t) ds then False else True
        invariant = if all matchInvariant fs then True else False
        matchInvariant (Invariant i) = isInfixOf i t
        matchInvariant _ = True

