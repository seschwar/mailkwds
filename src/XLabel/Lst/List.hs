{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module:     XLabel.Lst.List
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module XLabel.Lst.List
    ( module Data.List
    , Lst
    , empty
    , singleton
    , cons
    , snoc
    , dropAround
    , dropWhileEnd
    ) where

import Data.Bool (Bool)
import Data.Function ((.))
import Data.List

type Lst = []

empty :: [a]
empty = []

singleton :: a -> [a]
singleton = (:[])

cons :: a -> [a] -> [a]
cons = (:)

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

dropAround :: (a -> Bool) -> [a] -> [a]
dropAround f = dropWhileEnd f . dropWhile f

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd f = reverse . dropWhile f . reverse

