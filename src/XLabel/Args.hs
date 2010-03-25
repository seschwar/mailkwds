{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Module:     XLabel.Args
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module XLabel.Args (Config(..), parseArgs) where

import Control.Applicative (liftA2)
import Control.Arrow ((+++))
import Control.Category ((>>>))
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.List (union, nub, (\\))
import Data.Map (Map, empty, insert, singleton)
import Data.String (IsString(..))
import Parsimony hiding (empty, labels)
import Parsimony.Error (newErrorUnknown)
import Parsimony.Pos (incSourceColumn)
import Parsimony.Stream (Stream, Token(..))
import XLabel.ByteString (ByteString)
import XLabel.Core (foldHeaders)

-- | The configuration options for the program.
data Config a = Config
    { catenate :: [a] -> [a]         -- ^ whether to catenate or refold headers
    , command  :: [a] -> [a] -> [a]  -- ^ the command to apply to the 'Label's
    , help     :: Bool
    , input    :: Map a a            -- ^ the headers to parse
    , labels   :: [a]                -- ^ the 'Label's to be added/removed
    , output   :: [(a, a)]           -- ^ the headers to print
    , version  :: Bool
    }

instance Show a => Show (Config a) where
    show x = "Config "
        ++ "{ catenate :: [a] -> [a]"
        ++ ", command :: [a] -> [a] -> [a]"
        ++ ", help = " ++ show (help x)
        ++ ", input = " ++ show (input x)
        ++ ", labels = " ++ show (labels x)
        ++ ", output = " ++ show (output x)
        ++ ", version = " ++ show (version x)
        ++ " }"

-- | Parse the command line arguments and create an appropriate configuration.
parseArgs :: [String] -> Either String (Config ByteString)
parseArgs args = ((++) "Unable to parse command line arguments:\n" . show)
                 +++ (sanitizeConfig . \x -> foldr (>>>) id x $ config)
                 $ parse pArgs args  -- (>>>) ensures that the map's values get
  where
    config = Config                  -- overwritten correctly
        { catenate = foldHeaders
        , command  = flip const
        , help     = False
        , input    = empty
        , labels   = []
        , output   = []
        , version  = False
        }

-- | Ensure a halfway sane configuration.
sanitizeConfig :: (Eq a, IsString a) => Config a -> Config a
sanitizeConfig c = c
    { input    = sanitizeInput $ input c
    , output   = sanitizeOutput $ output c
    , labels   = nub . reverse $ labels c  -- output labels in the same order
    }                                      -- as specified on the command line
  where
    sanitizeInput  x | x == empty = singleton "x-label" ","
                     | otherwise  = x
    sanitizeOutput [] = [("X-Label", ", ")]
    sanitizeOutput x  = nub $ reverse x  -- adjust order to command line

--------------------------------------------------------------------------------
-- The command line argument 'Parser'.

-- | Generalization of 'Parsimony.Char.char'.
token :: (Eq a, Stream t a, Show a) => a -> Parser t a
token t = satisfy (== t) <?> show [t]

-- | Generalization of 'Parsimony.Char.satisfy'.
satisfy :: (Stream t a, Show a) => (a -> Bool) -> Parser t a
satisfy f = try $ anyToken >>= \t ->
    if f t
       then return t
       else unexpected (show t)

-- | Make 'String's tokens so that we can parse @[String]@ with Parsimony.
instance Token [Char] where
    updatePos _ p = incSourceColumn p 1
    showToken     = id

-- | Parse a list of 'String's to a list of 'Config' modifying functions.
pArgs :: (IsString a, Ord a) => Parser [String] [Config a -> Config a]
pArgs = many pOption <+> pSep <+> fmap (:[]) pCommand <+> many pLabel
    where (<+>) = liftA2 (++)

pOption :: (IsString a, Ord a) => Parser [String] (Config a -> Config a)
pOption = pCat <|> pHelp <|> pInput <|> pOutput <|> pVersion <|> pUnrecognized <?> "option"

pCat :: Parser [String] (Config a -> Config a)
pCat = (token "-c" <|> token "--cat") *> pure (cat id) <?> "catenate"
    where cat x c = c { catenate = x }

pHelp :: Parser [String] (Config a -> Config a)
pHelp = (token "-h" <|> token "--help") *> pure (hel True) <?> "help"
    where hel x c = c { help = x }

pInput :: (IsString a, Ord a) => Parser [String] (Config a -> Config a)
pInput = (choice $ map token ["-f", "--from", "-i", "--input"])
         *> (inp <$> anyToken <*> anyToken)
         <?> "input"
  where
    inp x y c = c { input =
        insert (fromString $ map toLower x) (fromString y) (input c) }

pOutput :: IsString a => Parser [String] (Config a -> Config a)
pOutput = (choice $ map token ["-o", "--output", "-t", "--to"])
          *> (out <$> anyToken <*> anyToken)
          <?> "output"
    where out x y c = c { output = (fromString x, fromString y) : output c }

pVersion :: Parser [String] (Config a -> Config a)
pVersion = (token "-v" <|> token "--version") *> pure (ver True) <?> "version"
    where ver x c = c { version = x }

pUnrecognized :: Parser [String] (Config a -> Config a)
pUnrecognized = satisfy (liftA2 (&&) (isPrefixOf "-") (/= "--"))
                *> parseError newErrorUnknown
                <?> "unrecognized"

pSep :: Parser [String] [Config a -> Config a]
pSep = skip (token "--") *> pure [] <?> "separator"

pCommand :: Eq a => Parser [String] (Config a -> Config a)
pCommand = (<?> "command") . choice . map (\(c, f) -> token c *> pure (com f))
           $ cmds
  where
    com x c = c { command = x }
    cmds =
        [ ("add",    flip union)
        , ("clear",  const $ const [])
        , ("set",    const)
        , ("remove", flip (\\))
        , ("tidy",   flip const)
        ]

pLabel :: IsString a => Parser [String] (Config a -> Config a)
pLabel = lab <$> anyToken <?> "label"
    where lab x c = c { labels = fromString x : labels c }

