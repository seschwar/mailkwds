-- |
-- Module:     Utils
-- Copyright:  Copyright (c) 2009 Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Utils where

import Data.Char (isSpace)
import Data.Monoid (Monoid, mappend, mempty)

mconscat :: Monoid m => (m -> m -> Bool) -> (m -> m) -> (m -> m) -> [m] -> [m]
mconscat prd f g = foldr h []
    where h x xs = let (x', xs') = partition x xs
                   in  f x `mappend` g x' : xs'

          partition x (x':xs) | prd x x' = (x', xs)
          partition _ xs                 = (mempty, xs)

-- | Strips all elements for which @prd@ is 'True' from the beginning and the
-- end of a list.
stripWhile :: (a -> Bool) -> [a] -> [a]
stripWhile prd = rstripWhile prd . lstripWhile prd

-- | Strips all elements for which @prd@ is 'True' from the beginning of a list.
lstripWhile :: (a -> Bool) -> [a] -> [a]
lstripWhile = dropWhile

-- | Strips all elements for which @prd@ is 'True' from the end of a list.
rstripWhile :: (a -> Bool) -> [a] -> [a]
rstripWhile prd = reverse . lstripWhile prd . reverse

-- | Strips all whitespace from both the left and the right hand side of the
-- given 'String'.
strip :: String -> String
strip = stripWhile isSpace

-- | Strips all whitespace from the left side of the given 'String'.
lstrip :: String -> String
lstrip = lstripWhile isSpace

-- | Strips all whitespace from the right side of the given 'String'.
rstrip :: String -> String
rstrip = rstripWhile isSpace

