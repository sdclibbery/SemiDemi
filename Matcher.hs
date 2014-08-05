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
    matches
) where
import Data.List

-- |The type of all strings used in this module
type MatchString = String

-- |A Matcher Description: how to match against a string
data Desc = Desc [Flow] [Disallowed] deriving (Show, Eq)

-- |Parts of a Matcher Description that must be matched exactly
data Flow = Exact MatchString | Fuzzy MatchString deriving (Show, Eq)

-- |Disallowed parts that must not be in the string being matched
data Disallowed = Disallowed MatchString deriving (Show, Eq)

-- |Test whether a matcher is empty (contains no match elements)
empty :: Desc -> Bool
empty (Desc es ds) = null es && null ds

-- |See if a given matcher matches a given target string
matches :: Desc -> MatchString -> Bool
matches (Desc fs ds) t = disallowed && exact
    where
        disallowed = if any (\(Disallowed d) -> isInfixOf d t) ds then False else True
        exact = if all (\(Exact e) -> isInfixOf e t) fs then True else False
