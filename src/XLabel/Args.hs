-- |
-- Module:     XLabel.Args
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module XLabel.Args (Config(..), parseArgs) where

import Control.Applicative (liftA2)
import Control.Arrow (right)
import Data.Char (toLower)
import Data.List (union, nub, (\\))
import Data.Map (Map, empty, insert, singleton)
import Parsimony hiding (empty, labels)
import Parsimony.Error (ParseError)
import XLabel.Core (Label)
import XLabel.Utils (token)

-- | The configuration options for the program.
data Config a = Config
    { catenate :: Bool                -- ^ whether to catenate or refold headers
    , input    :: Map String String   -- ^ the headers to parse
    , output   :: [(String, String)]  -- ^ the headers to print
    , command  :: [a] -> [a] -> [a]   -- ^ the command to apply to the 'Label's
    , labels   :: [a]                 -- ^ the 'Label's to be added/removed
    }

instance Show a => Show (Config a) where
    show x = "Config "
             ++ "{ catenate = " ++ show (catenate x)
             ++ ", input = " ++ show (input x)
             ++ ", output = " ++ show (output x)
             ++ ", command :: [a] -> [a] -> [a]"
             ++ ", labels = " ++ show (labels x)
             ++ " }"

data Flag a = Catenate Bool
            | Input String String
            | Output String String
            | Command ([a] -> [a] -> [a])
            | Label a

-- | Add a 'Flag' to the given 'Config'.
toConfig :: Flag a -> Config a -> Config a
toConfig (Catenate c) cfg = cfg { catenate = c }
toConfig (Input h s)  cfg = cfg { input    = insert (map toLower h) s $ input cfg }
toConfig (Output h s) cfg = cfg { output   = (h, s) : output cfg }
toConfig (Command c)  cfg = cfg { command  = c }
toConfig (Label l)    cfg = cfg { labels   = l : labels cfg }

-- | Parse the command line arguments and create an appropriate configuration.
parseArgs :: [String] -> Either ParseError (Config Label)
parseArgs args = right (sanitizeConfig . foldr toConfig defaults)
                 . parse pArgs $ args
    where defaults :: Config Label
          defaults = Config
              { catenate = False
              , input    = empty
              , output   = []
              , command  = flip const
              , labels   = []
              }

-- | Ensure a halfway sane configuration.
sanitizeConfig :: Eq a => Config a -> Config a
sanitizeConfig c = c
    { input  = sanitizeInput $ input c
    , output = sanitizeOutput $ output c
    , labels = nub $ labels c
    }
    where sanitizeInput x | x == empty = singleton "x-label" ","
                          | otherwise  = x

          sanitizeOutput [] = [("X-Label", ", ")]
          sanitizeOutput x  = nub x

pArgs :: Parser [String] [Flag Label]
pArgs = many pOption <+> pSep <+> fmap (:[]) pCommand <+> many pLabel
    where (<+>) = liftA2 (++)

pOption :: Parser [String] (Flag Label)
pOption = pCat <|> pInput <|> pOutput <?> "option"

pSep :: Parser [String] [Flag a]
pSep = skip (token "--") *> pure [] <?> "separator"

pCommand :: Eq a => Parser [String] (Flag a)
pCommand = (<?> "command") . choice . map (\(c, f) -> token c *> (Command <$> pure f)) $ cmds
    where cmds =
              [ ("add",    flip union)
              , ("clear",  const $ const [])
              , ("set",    const)
              , ("remove", flip (\\))
              , ("tidy",   flip const)
              ]

pLabel :: Parser [String] (Flag Label)
pLabel = Label <$> anyToken <?> "label"

pCat :: Parser [String] (Flag a)
pCat = (token "-c" <|> token "--catenate")
       *> (Catenate <$> pure True)
       <?> "catenate"

pInput :: Parser [String] (Flag Label)
pInput = (token "-i" <|> token "--input")
         *> (Input <$> anyToken <*> anyToken)
         <?> "input"

pOutput :: Parser [String] (Flag Label)
pOutput = (token "-o" <|> token "--output")
          *> (Output <$> anyToken <*> anyToken)
          <?> "output"

