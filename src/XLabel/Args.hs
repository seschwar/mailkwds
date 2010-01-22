-- |
-- Module:     XLabel.Args
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module XLabel.Args (Config(..), parseArgs) where

import Control.Applicative (liftA2)
import Control.Arrow (right)
import Control.Monad (mplus)
import Data.Char (toLower)
import Data.List (union, nub, (\\))
import Data.Map (Map, singleton)
import Data.Monoid (Monoid(..))
import Parsimony hiding (empty, labels)
import Parsimony.Error (ParseError)
import XLabel.Core (Label)
import XLabel.Utils (token)

-- | The configuration options for the program.
data Config a = Config
    { catenate :: Maybe Bool                 -- ^ whether to catenate or refold headers
    , input    :: Maybe (Map String String)  -- ^ the headers to parse
    , output   :: Maybe [(String, String)]   -- ^ the headers to print
    , command  :: Maybe ([a] -> [a] -> [a])  -- ^ the command to apply to the 'Label's
    , labels   :: Maybe [a]                  -- ^ the 'Label's to be added/removed
    }

instance Monoid (Config a) where
    mempty = Config
                 { catenate = Nothing
                 , input    = Nothing
                 , output   = Nothing
                 , command  = Nothing
                 , labels   = Nothing
                 }
    mappend x y = Config
                      { catenate = catenate y `mplus` catenate x
                      , input    = input y `mappend` input x
                      , output   = output x `mappend` output y
                      , command  = command y `mplus` command x
                      , labels   = labels x `mappend` labels y
                      }

instance Show a => Show (Config a) where
    show x = "Config "
             ++ "{ catenate = " ++ show (catenate x)
             ++ ", input = " ++ show (input x)
             ++ ", output = " ++ show (output x)
             ++ ", command :: [a] -> [a] -> [a]"
             ++ ", labels = " ++ show (labels x)
             ++ " }"

-- | Parse the command line arguments and create an appropriate configuration.
parseArgs :: [String] -> Either ParseError (Config Label)
parseArgs args = right (sanitizeConfig . mconcat) . parse pArgs $ args

-- | Ensure a halfway sane configuration.
sanitizeConfig :: Eq a => Config a -> Config a
sanitizeConfig c = c
    { catenate = catenate c `mplus` Just False
    , input    = input c `mplus` Just (singleton "x-label" ",")
    , output   = fmap nub (output c) `mplus` Just [("X-Label", ", ")]
    , command  = command c `mplus` Just (flip const)
    , labels   = fmap nub (labels c) `mplus` Just []
    }

pArgs :: Parser [String] [Config Label]
pArgs = many pOption <+> pSep <+> fmap (:[]) pCommand <+> many pLabel
    where (<+>) = liftA2 mappend

pOption :: Parser [String] (Config Label)
pOption = pCat <|> pInput <|> pOutput <?> "option"

pSep :: Parser [String] [Config a]
pSep = skip (token "--") *> pure [] <?> "separator"

pCommand :: Eq a => Parser [String] (Config a)
pCommand = (<?> "commands") . choice . map (\(c, f) ->
           token c *> ((\x -> mempty { command = Just x }) <$> pure f <?> "command")
           ) $ cmds
    where cmds =
              [ ("add",    flip union)
              , ("clear",  const $ const [])
              , ("set",    const)
              , ("remove", flip (\\))
              , ("tidy",   flip const)
              ]

pLabel :: Parser [String] (Config Label)
pLabel = (\x -> mempty { labels = Just [x] }) <$> anyToken <?> "label"

pCat :: Parser [String] (Config a)
pCat = (token "-c" <|> token "--cat")
       *> ((\x -> mempty { catenate = Just x }) <$> pure True)
       <?> "catenate"

pInput :: Parser [String] (Config Label)
pInput = (token "-i" <|> token "--input")
          *> ((\x y -> mempty { input = Just $ singleton (map toLower x) y })
          <$> anyToken <*> anyToken) <?> "input"

pOutput :: Parser [String] (Config Label)
pOutput = (token "-o" <|> token "--output")
          *> ((\x y -> mempty { output = Just [(x, y)] })
          <$> anyToken <*> anyToken) <?> "output"

