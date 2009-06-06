import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (intercalate, stripPrefix, union, (\\))
import System.Environment (getArgs)

type Label = String

main :: IO ()
main = do
    args <- getArgs
    when (null args) (error "No operation specified.")
    interact $ unlines . rewrite (operator (head args) (tail args)) [] . lines

operator :: Eq a => String -> [a] -> [a] -> [a]
operator "add"    a b = a `union` b
operator "clear"  _ _ = []
operator "remove" a b = b \\ a
operator "set"    a _ = a
operator s        _ _ = error $ "Invalid operation: " ++ s

rewrite :: ([Label] -> [Label]) -> [Label] -> [String] -> [String]
rewrite f ls ss     | null ss || null (head ss)
                    = case f . hdr2lst . concat . reverse $ ls of
                           [] -> ss
                           l -> ("X-Label: " ++ lst2hdr l) : ss
rewrite f ls (s:ss) = case stripPrefix "X-Label: " s of
                           Nothing -> s : rewrite f ls ss
                           Just s' -> rewrite' f [s'] ss

rewrite' :: ([Label] -> [Label]) -> [Label] -> [String] -> [String]
rewrite' f ls ((c:cs):ss) | isSpace c = rewrite' f ((c:cs):ls) ss
rewrite' f ls ss          = rewrite f ls ss

hdr2lst :: String -> [Label]
hdr2lst = filter (not . null) . map strip . split (== ',')

lst2hdr :: [Label] -> String
lst2hdr = intercalate ", "

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile p xs of
                  []  -> []
                  xs' -> x : split p xs''
                      where (x, xs'') = break p xs'

strip :: String -> String
strip = rstrip . lstrip

lstrip :: String -> String
lstrip = dropWhile isSpace

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

