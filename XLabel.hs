import Control.Monad (when)
import Data.List (isPrefixOf, union, (\\))
import Data.String.Utils (strip, split)
import System.Environment (getArgs)
import System.IO (hGetContents, stdin)

main :: IO ()
main = do
    args <- getArgs
    msg  <- hGetContents stdin
    when (length args < 1) (error "No operation specified.")
    rewrite (operator (head args) (tail args)) (lines msg) []

operator :: Eq a => String -> [a] -> [a] -> [a]
operator "add"    a b = a `union` b
operator "clear"  _ _ = []
operator "remove" a b = b \\ a
operator "set"    a _ = a
operator s        _ _ = error $ "Invalid operation: " ++ s

rewrite :: ([String] -> [String]) -> [String] -> [String] -> IO ()
rewrite op []      a = putStrLn $ "X-Label: " ++ lst2hdr (op a)
rewrite op ("":ss) a = do
    let ls = op a
    when (length ls > 0) (putStrLn $ "X-Label: " ++ lst2hdr ls)
    mapM_ putStrLn ("":ss)
rewrite op (s:ss) a | "X-Label:" `isPrefixOf` s = unfoldHdr op ss (a ++ hdr2lst s)
rewrite op (s:ss) a = putStrLn s >> rewrite op ss a

unfoldHdr :: ([String] -> [String]) -> [String] -> [String] -> IO ()
unfoldHdr op (s:ss) a | " " `isPrefixOf` s = unfoldHdr op ss (a ++ hdr2lst s)
unfoldHdr op (s:ss) a | "\t" `isPrefixOf` s = unfoldHdr op ss (a ++ hdr2lst s)
unfoldHdr op ss     a = rewrite op ss a

hdr2lst :: String -> [String]
hdr2lst ('X':'-':'L':'a':'b':'e':'l':':':s) = hdr2lst s
hdr2lst s = filter (/= "") $ map strip (split "," s)

lst2hdr :: [String] -> String
lst2hdr []     = ""
lst2hdr (s:[]) = s
lst2hdr (s:ss) = s ++ ", " ++ lst2hdr ss

