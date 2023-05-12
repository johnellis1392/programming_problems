{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

lengthOfLastWord :: T.Text -> Int
lengthOfLastWord = T.length . last . filter (not . T.null) . T.splitOn " "

tests :: [(T.Text, Int)]
tests = [
  ("Hello World", 5),
  ("   fly me   to   the moon  ", 4),
  ("luffy is still joyboy", 6)]

runTest :: (T.Text, Int) -> T.Text
runTest (s, n) =
  let res = lengthOfLastWord s
  in if res == n then
      T.pack "Success"
     else
      T.pack $ "Failure: " ++ show res ++ " != " ++ show n

main :: IO ()
main = do
  putStrLn "Running..."
  print $ runTest <$> tests
