-- |
-- Module:     Utils
-- Copyright:  Copyright (c) 2009 Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Utils where

import Data.Char (isSpace)
import Data.Monoid (Monoid, mappend, mempty)

-- | @mconscat prd f g@ catenates to successive elements in a list if @prd@ of
-- these two elements is 'True'.  @f@ and @g@ are applied to the first and
-- second element respectively before they are 'mappend'ed.
mconscat :: Monoid m => (m -> m -> Bool) -> (m -> m) -> (m -> m) -> [m] -> [m]
mconscat prd f g = foldr h []
    where h x xs = let (x', xs') = partition x xs
                   in  f x `mappend` g x' : xs'

          partition x (x':xs) | prd x x' = (x', xs)
          partition _ xs                 = (mempty, xs)

-- | Drops all elements for which the given predicate is 'True' from the end of
-- the list.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd prd = reverse . dropWhile prd . reverse

-- | Drops all elements for which the given predicate is 'True' from the
-- beginning and the end of the list.
dropAround :: (a -> Bool) -> [a] -> [a]
dropAround prd =  dropWhileEnd prd . dropWhile prd

-- | Strips all whitespace from both the left and the right hand side of the
-- given 'String'.
strip :: String -> String
strip = dropAround isSpace

-- | Strips all whitespace from the left side of the given 'String'.
stripStart :: String -> String
stripStart = dropWhile isSpace

-- | Strips all whitespace from the right side of the given 'String'.
stripEnd :: String -> String
stripEnd = dropWhileEnd isSpace

