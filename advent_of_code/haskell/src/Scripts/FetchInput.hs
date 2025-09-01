module Scripts.FetchInput (fetchInput, saveInput, saveInputForYear, saveAllInput) where

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Network.HTTP.Simple
import System.Directory (doesFileExist)

(+++) :: BS.ByteString -> BS.ByteString -> BS.ByteString
(+++) = BS.append

-- sessionId :: BS.ByteString
-- sessionId = "[SESSION ID]"

show' :: (Show a) => a -> BS.ByteString
show' = BS.pack . show

years :: [BS.ByteString]
years = [show' y | y <- [2015 .. 2023] :: [Int]]

days :: [BS.ByteString]
days = [show' y | y <- [1 .. 25] :: [Int]]

requestUrl :: BS.ByteString -> BS.ByteString -> BS.ByteString
requestUrl year day = "https://adventofcode.com/" +++ year +++ "/day/" +++ day +++ "/input"

fetchInput :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO String
fetchInput sessionId year day = do
  let url = requestUrl year day
  request' <- parseRequest $ BS.unpack $ "POST " +++ url
  let request = setRequestHeaders [("Cookie", "session=" +++ sessionId)] request'
  response <- httpLBS request
  return . LBS.unpack . getResponseBody $ response

saveInput :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO ()
saveInput sessionId year day = do
  let filename = "./input/aoc" ++ BS.unpack year ++ "/day" ++ BS.unpack day ++ ".input.txt"
  fileExists <- doesFileExist filename
  if fileExists
    then do
      print $ "File " ++ filename ++ " already exists; skipping."
      return ()
    else do
      input <- fetchInput sessionId year day
      writeFile filename input
      print $ "Saved File " ++ filename ++ "."
      return ()

saveInputForYear :: BS.ByteString -> BS.ByteString -> IO ()
saveInputForYear sessionId year = mapM_ (saveInput sessionId year) days

saveAllInput :: BS.ByteString -> IO ()
saveAllInput sessionId = mapM_ (saveInputForYear sessionId) years