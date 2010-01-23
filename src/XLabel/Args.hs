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
import Data.List (union, nub, (\\))
import Data.Map (Map, empty, insert, singleton)
import Parsimony hiding (empty, labels)
import XLabel.Core (Label, foldHeaders)
import XLabel.Utils (token)

-- | The configuration options for the program.
data Config a = Config
    { catenate :: [String] -> [String]  -- ^ whether to catenate or refold headers
    , command  :: [a] -> [a] -> [a]     -- ^ the command to apply to the 'Label's
    , help     :: Bool
    , input    :: Map String String     -- ^ the headers to parse
    , labels   :: [a]                   -- ^ the 'Label's to be added/removed
    , output   :: [(String, String)]    -- ^ the headers to print
    , version  :: Bool
    }

instance Show a => Show (Config a) where
    show x = "Config "
             ++ "{ catenate :: [String] -> [String]"
             ++ ", command :: [a] -> [a] -> [a]"
             ++ ", help = " ++ show (help x)
             ++ ", input = " ++ show (input x)
             ++ ", labels = " ++ show (labels x)
             ++ ", output = " ++ show (output x)
             ++ ", version = " ++ show (version x)
             ++ " }"

-- | Parse the command line arguments and create an appropriate configuration.
parseArgs :: [String] -> Either String (Config Label)
parseArgs args = ((++) "Unable to parse command line arguments:\n" . show)
                 +++ (sanitizeConfig . \x -> foldr (>>>) id x $ config)
                 $ parse pArgs args  -- (>>>) ensures that the map's values get
    where config = Config            -- overwritten correctly
              { catenate = foldHeaders
              , command  = flip const
              , help     = False
              , input    = empty
              , labels   = []
              , output   = []
              , version  = False
              }

-- | Ensure a halfway sane configuration.
sanitizeConfig :: Eq a => Config a -> Config a
sanitizeConfig c = c
    { input    = sanitizeInput $ input c
    , output   = sanitizeOutput $ output c
    , labels   = nub . reverse $ labels c  -- output labels in the same order
    }                                      -- as specified on the command line
    where sanitizeInput  x | x == empty = singleton "x-label" ","
                           | otherwise  = x
          sanitizeOutput [] = [("X-Label", ", ")]
          sanitizeOutput x  = nub $ reverse x  -- adjust order to command line

pArgs :: Parser [String] [Config Label -> Config Label]
pArgs = many pOption <+> pSep <+> fmap (:[]) pCommand <+> many pLabel
    where (<+>) = liftA2 (++)

pOption :: Parser [String] (Config Label -> Config Label)
pOption = pCat <|> pHelp <|> pInput <|> pOutput <|> pVersion <?> "option"

pCat :: Parser [String] (Config a -> Config a)
pCat = (token "-c" <|> token "--cat") *> pure (cat id) <?> "catenate"
    where cat x c = c { catenate = x }

pHelp :: Parser [String] (Config a -> Config a)
pHelp = (token "-h" <|> token "--help") *> pure (hel True) <?> "help"
    where hel x c = c { help = x }

pInput :: Parser [String] (Config Label -> Config Label)
pInput = (token "-i" <|> token "--input") *> (inp <$> anyToken <*> anyToken)
         <?> "input"
    where inp x y c = c { input = insert (map toLower x) y $ input c }

pOutput :: Parser [String] (Config Label -> Config Label)
pOutput = (token "-o" <|> token "--output") *> (out <$> anyToken <*> anyToken)
          <?> "output"
    where out x y c = c { output = (x, y) : output c }

pVersion :: Parser [String] (Config a -> Config a)
pVersion = (token "-v" <|> token "--version") *> pure (ver True) <?> "version"
    where ver x c = c { version = x }

pSep :: Parser [String] [Config a -> Config a]
pSep = skip (token "--") *> pure [] <?> "separator"

pCommand :: Eq a => Parser [String] (Config a -> Config a)
pCommand = (<?> "command") . choice . map (\(c, f) -> token c *> pure (com f))
           $ cmds
    where com x c = c { command = x }
          cmds =
              [ ("add",    flip union)
              , ("clear",  const $ const [])
              , ("set",    const)
              , ("remove", flip (\\))
              , ("tidy",   flip const)
              ]

pLabel :: Parser [String] (Config Label -> Config Label)
pLabel = lab <$> anyToken <?> "label"
    where lab x c = c { labels = x : labels c }

