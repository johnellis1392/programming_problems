import System.IO
import Control.Monad
import Data.List (intersperse)
import qualified Data.HashMap.Strict as M

--
-- Unfinished...
--

r :: String -> IO [(String, String)]
r filename = do
  contents <- readFile filename
  return $ [(a, b) | [a, b] <- fmap words $ lines contents]

score :: (String, String) -> Int
score (a, b) = 

main = do
  putStrLn $ "Starting..."
  s <- r "input.test.txt"
  putStrLn $ concat $ intersperse ", " $ fmap unwords $ s
