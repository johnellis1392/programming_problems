module Common.Profiling (profile) where

import Data.Time.Clock.System (SystemTime (systemNanoseconds), getSystemTime)

profile :: (a -> b) -> a -> IO b
profile f a = do
  before <- getSystemTime
  let res = f a
  after <- getSystemTime
  let diff = systemNanoseconds after - systemNanoseconds before
  let ns = diff `mod` 1000000
  let ms = (diff `mod` 1000000000) - ns
  putStrLn $ "Time: " ++ show ms ++ "ms." ++ show ns ++ "ns"
  return res