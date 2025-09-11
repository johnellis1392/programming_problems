{-# LANGUAGE OverloadedStrings #-}

module LengthOfLastWord where

import qualified Data.Text as T

lengthOfLastWord :: T.Text -> Int
lengthOfLastWord = T.length . last . filter (not . T.null) . T.splitOn " "
