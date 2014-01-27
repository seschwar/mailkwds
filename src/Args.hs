-- |
-- Module:     Args
-- Copyright:  Copyright (c) 2009-2014, Sebastian Schwarz <seschwar@gmail.com>
-- License:    MIT
-- Maintainer: Sebastian Schwarz <seschwar@gmail.com>
--

module Args
    ( Config(..)
    , parseArgs
    ) where

import Control.Applicative        (Applicative(..), liftA2, (<$>))
import Control.Arrow              ((+++))
import Control.Category           ((>>>))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char                  (toLower)
import Data.List                  (isPrefixOf, nub, union, (\\))
import Data.Map                   (Map, empty, insert, singleton)
import Data.String                (IsString(..))
import MailKwds                   (foldHeaders)
import Text.Parsec                hiding (satisfy, token)
import Text.Parsec.Error

-- | The configuration options for the program.
data Config a = Config
    { catenate :: [a] -> [a]         -- ^ whether to catenate or refold headers
    , command  :: [a] -> [a] -> [a]  -- ^ the command to apply to the 'Label's
    , help     :: Bool
    , input    :: Map a a            -- ^ the headers to parse
    , keywords :: [a]                -- ^ the 'Label's to be added/removed
    , output   :: [(a, a)]           -- ^ the headers to print
    , version  :: Bool
    }

-- For debugging proposes
instance Show a => Show (Config a) where
    show x = "Config "
        ++ "{ catenate :: [a] -> [a]"
        ++ ", command :: [a] -> [a] -> [a]"
        ++ ", help = " ++ show (help x)
        ++ ", input = " ++ show (input x)
        ++ ", keywords = " ++ show (keywords x)
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
parseArgs args = sanitizeError +++ (sanitizeConfig . \x -> foldr (>>>) id x config)
    $ parse pArgs "Args" args  -- (>>>) ensures that the map's values
  where                        -- get overwritten correctly
    config = Config
        { catenate = foldHeaders
        , command  = flip const
        , help     = False
        , input    = empty
        , keywords   = []
        , output   = []
        , version  = False
        }

-- | Print @UnExpect@ed errors @Message@s or the complete @ParseError@ if there
-- is none.
sanitizeError :: ParseError -> String
sanitizeError e = if null msg
                     then "Unable to parse command line arguments:\n" ++ show e
                     else msg
                  ++ "Try `mailkwds --help` for help."
    where msg = unlines . map messageString . errorMessages $ e

-- | Ensure a halfway sane configuration.
sanitizeConfig :: (Eq a, IsString a) => Config a -> Config a
sanitizeConfig c = c
    { input    = sanitizeInput $ input c
    , output   = sanitizeOutput $ output c
    , keywords = nub . reverse $ keywords c  -- output keywords in the same order
    }                                        -- as specified on the command line
  where
    sanitizeInput  x | x == empty = singleton "keywords" ","
                     | otherwise  = x
    sanitizeOutput [] = [("Keywords", ", ")]
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
pLabel = lab <$> anyToken <?> "keyword"
    where lab x c = c { keywords = fromString x : keywords c }
