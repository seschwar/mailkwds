import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (isPrefixOf, union, (\\))
import Data.String.Utils (strip, split)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    when (null args) (error "No operation specified.")
    interact $ unlines . rewrite1 (operator (head args) (tail args)) . lines

operator :: Eq a => String -> [a] -> [a] -> [a]
operator "add"    a b = a `union` b
operator "clear"  _ _ = []
operator "remove" a b = b \\ a
operator "set"    a _ = a
operator s        _ _ = error $ "Invalid operation: " ++ s

rewrite1 :: ([String] -> [String]) -> [String] -> [String]
rewrite1 _ []      = []
rewrite1 _ ("":ss) = "":ss
rewrite1 f (s:ss)  | "X-Label:" `isPrefixOf` s = rewrite2 f ss [s]
rewrite1 f (s:ss)  = s : rewrite1 f ss

rewrite2 :: ([String] -> [String]) -> [String] -> [String] -> [String]
rewrite2 f ((c:s):ss) a | isSpace c = rewrite2 f ss ((c:s):a)
rewrite2 f ss         a = ("X-Label: " ++ (lst2hdr . f . hdr2lst . concat . reverse $ a)) : rewrite1 f ss

hdr2lst :: String -> [String]
hdr2lst ('X':'-':'L':'a':'b':'e':'l':':':s) = hdr2lst s
hdr2lst s = filter (/= "") $ map strip (split "," s)

lst2hdr :: [String] -> String
lst2hdr []     = ""
lst2hdr (s:[]) = s
lst2hdr (s:ss) = s ++ ", " ++ lst2hdr ss

