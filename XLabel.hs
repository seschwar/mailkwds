import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (isPrefixOf, union, (\\))
import Data.String.Utils (split)
import System.Environment (getArgs)

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

rewrite1 :: ([String] -> [String]) -> [String] -> [String] -> [String]
rewrite1 f a ss      | null ss || null (head ss)
                     = case f . hdr2lst . concat . reverse $ a of
                            [] -> ss
                            ls -> ("X-Label: " ++ lst2hdr ls) : ss
rewrite1 f _ (s:ss)  | "X-Label:" `isPrefixOf` s = rewrite2 f [s] ss
rewrite1 f a (s:ss)  = s : rewrite1 f a ss

rewrite2 :: ([String] -> [String]) -> [String] -> [String] -> [String]
rewrite2 f a ((c:cs):ss) | isSpace c = rewrite2 f ((c:cs):a) ss
rewrite2 f a ss          = rewrite1 f a ss

hdr2lst :: String -> [String]
hdr2lst ('X':'-':'L':'a':'b':'e':'l':':':s) = hdr2lst s
hdr2lst s = filter (not . null) $ map strip (split "," s)

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

lst2hdr :: [String] -> String
lst2hdr []     = ""
lst2hdr (s:[]) = s
lst2hdr (s:ss) = s ++ ", " ++ lst2hdr ss

