import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf, stripPrefix, union, (\\))
import Data.Maybe (fromJust)
import Data.String.Utils (split)
import System.Environment (getArgs)

type Label = String

main :: IO ()
main = do
    args <- getArgs
    when (null args) (error "No operation specified.")
    interact $ unlines . rewrite1 (operator (head args) (tail args)) [] . lines

operator :: Eq a => String -> [a] -> [a] -> [a]
operator "add"    a b = a `union` b
operator "clear"  _ _ = []
operator "remove" a b = b \\ a
operator "set"    a _ = a
operator s        _ _ = error $ "Invalid operation: " ++ s

rewrite1 :: ([Label] -> [Label]) -> [Label] -> [String] -> [String]
rewrite1 f ls ss      | null ss || null (head ss)
                      = case f . hdr2lst . concat . reverse $ ls of
                             [] -> ss
                             l -> ("X-Label: " ++ lst2hdr l) : ss
rewrite1 f _  (s:ss)  | "X-Label:" `isPrefixOf` s
                      = rewrite2 f [fromJust $ stripPrefix "X-Label:" s] ss
rewrite1 f ls (s:ss)  = s : rewrite1 f ls ss

rewrite2 :: ([Label] -> [Label]) -> [Label] -> [String] -> [String]
rewrite2 f ls ((c:cs):ss) | isSpace c = rewrite2 f ((c:cs):ls) ss
rewrite2 f ls ss          = rewrite1 f ls ss

hdr2lst :: String -> [Label]
hdr2lst = filter (not . null) . map strip . split ","

lst2hdr :: [Label] -> String
lst2hdr = intercalate ", "

strip :: String -> String
strip = rstrip . lstrip

lstrip :: String -> String
lstrip = dropWhile isSpace

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

