-- |
-- Module:     Args
-- Copyright:  Copyright (c) 2009-2010, Sebastian Schwarz <seschwar@googlemail.com>
-- License:    ISC
-- Maintainer: Sebastian Schwarz <seschwar@googlemail.com>
--

module Args (Config(..), parseArgs) where

import Control.Applicative (Applicative(..), liftA2, (<$>))
import Control.Arrow ((+++))
import Control.Category ((>>>))
import Data.Char (toLower)
import Data.List (isPrefixOf, nub, union, (\\))
import Data.Map (Map, empty, insert, singleton)
import Data.String (IsString(..))
import Text.Parsec hiding (labels, satisfy, token)
import Text.Parsec.Error
import ByteString (ByteString)
import XLabel (foldHeaders)

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

-- | Generalization of 'Text.Parsec.Char.char'.
token :: (Eq a, Show a, Stream s m a) => a -> ParsecT s u m a
token t = satisfy (== t) <?> show [t]

-- | Generalization of 'Text.Parsec.Char.satisfy'.
satisfy :: (Show a, Stream s m a) => (a -> Bool) -> ParsecT s u m a
satisfy f = tokenPrim (\c -> show [c])
                      (\pos _ _ -> incSourceColumn pos 1)
                      (\c -> if f c then Just c else Nothing)

-- | Parse the command line arguments and create an appropriate configuration.
parseArgs :: [String] -> Either String (Config ByteString)
parseArgs args = sanitizeError
    +++ (sanitizeConfig . \x -> foldr (>>>) id x config)
    $ parse pArgs "Args" args  -- (>>>) ensures that the map's values
  where                        -- get overwritten correctly
    config = Config
        { catenate = foldHeaders
        , command  = flip const
        , help     = False
        , input    = empty
        , labels   = []
        , output   = []
        , version  = False
        }

-- | Print @UnExpect@ed errors @Message@s or the complete @ParseError@ if there
-- is none.
sanitizeError :: ParseError -> String
sanitizeError e = (++ "Try `x-label --help` for help.")
    . (\xs -> if null xs
                 then "Unable to parse command line arguments:\n" ++ show e
                 else unlines xs)
    . (\xs -> [x | UnExpect x <- xs]) . errorMessages $ e

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
-- The command line argument parser.

-- | Parse a list of 'String's to a list of 'Config' modifying functions.
pArgs :: (IsString a, Monad m, Ord a)
    => ParsecT [String] u m [Config a -> Config a]
pArgs = many pOption <+> pSep <+> pCommand
    where (<+>) = liftA2 (++)

pOption :: (IsString a, Monad m, Ord a)
    => ParsecT [String] u m (Config a -> Config a)
pOption = pCat <|> pHelp <|> pInput <|> pOutput <|> pVersion
    <|> unexpected . (++) "unrecognized option: "
    =<< satisfy (liftA2 (&&) (isPrefixOf "-") (/= "--"))

pCat :: Monad m => ParsecT [String] u m (Config a -> Config a)
pCat = (token "-c" <|> token "--catenate") *> pure (cat id) <?> "catenate"
    where cat x c = c { catenate = x }

pHelp :: Monad m => ParsecT [String] u m (Config a -> Config a)
pHelp = (token "-h" <|> token "--help") *> pure (hel True) <?> "help"
    where hel x c = c { help = x }

pInput :: (IsString a, Monad m, Ord a)
    => ParsecT [String] u m (Config a -> Config a)
pInput = (choice $ map token ["-f", "--from", "-i", "--input"])
         *> (inp <$> anyToken <*> anyToken)
         <?> "input"
  where
    inp x y c = c { input =
        insert (fromString $ map toLower x) (fromString y) (input c) }

pOutput :: (IsString a, Monad m) => ParsecT [String] u m (Config a -> Config a)
pOutput = (choice $ map token ["-o", "--output", "-t", "--to"])
          *> (out <$> anyToken <*> anyToken)
          <?> "output"
    where out x y c = c { output = (fromString x, fromString y) : output c }

pVersion :: Monad m => ParsecT [String] u m (Config a -> Config a)
pVersion = (token "-v" <|> token "--version") *> pure (ver True) <?> "version"
    where ver x c = c { version = x }

pSep :: Monad m => ParsecT [String] u m [a]
pSep = optional (token "--") *> pure [] <?> "separator"

pCommand :: (Eq a, IsString a, Monad m)
    => ParsecT [String] u m [Config a -> Config a]
pCommand = option [] $ liftA2 (++) (fmap (:[]) pCommands) (many pLabel)

pCommands :: (Eq a, Monad m) => ParsecT [String] u m (Config a -> Config a)
pCommands = (choice . map (\(c, f) -> token c *> pure (com f) <?> "command")
    $ cmds) <|> unexpected . (++) "unknown command: " =<< anyToken
    <|> unexpected "no command given"
  where
    com x c = c { command = x }
    cmds =
        [ ("add",    flip union)
        , ("clear",  const $ const [])
        , ("set",    const)
        , ("remove", flip (\\))
        , ("tidy",   flip const)
        ]

pLabel :: (IsString a, Monad m) => ParsecT [String] u m (Config a -> Config a)
pLabel = lab <$> anyToken <?> "label"
    where lab x c = c { labels = fromString x : labels c }

