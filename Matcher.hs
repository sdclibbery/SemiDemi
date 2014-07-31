{-|
Module      : Matcher
Description : Define a matcher, and score it against a given string
-}

module Matcher (
    MatchString,
    Exact(..),
    Disallowed(..),
    Desc(..),
    matches
) where
import Data.List

-- |The type of all strings used in this module
type MatchString = String

-- |A Matcher Description: how to match against a string
data Desc = Desc [Exact] [Disallowed] deriving (Show, Eq)

-- |Parts of a Matcher Description that must be matched exactly
data Exact = Exact MatchString deriving (Show, Eq)

-- |Disallowed parts that must not be in the string being matched
data Disallowed = Disallowed MatchString deriving (Show, Eq)

-- |See if a given matcher matches a given target string
matches :: Desc -> MatchString -> Bool
matches (Desc es ds) t = disallowed && exact
    where
        disallowed = if any (\(Disallowed d) -> isInfixOf d t) ds then False else True
        exact = if all (\(Exact e) -> isInfixOf e t) es then True else False
