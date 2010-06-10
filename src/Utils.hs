-- |
-- Module:     XLabel.Utils
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    MIT
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Utils
    ( applyEach
    , mconscat
    ) where

import Data.Monoid (Monoid, mappend, mempty)

-- | Apply each given function to specified argument and return the results.
applyEach :: Functor f => f (a -> b) -> a -> f b
applyEach fs x = fmap ($ x) fs

-- | @mconscat prd f g@ catenates to successive elements in a list if @prd@ of
-- these two elements is 'True'.  @f@ and @g@ are applied to the first and
-- second element respectively before they are 'mappend'ed.
mconscat :: Monoid m => (m -> m -> Bool) -> (m -> m) -> (m -> m) -> [m] -> [m]
mconscat prd f g = foldr h []
  where
    h x xs = let (x', xs') = partition x xs
             in  f x `mappend` g x' : xs'
    partition x (x':xs) | prd x x' = (x', xs)
    partition _ xs                 = (mempty, xs)

