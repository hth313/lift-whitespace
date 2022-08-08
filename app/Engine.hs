{-# LANGUAGE OverloadedStrings #-}
module Engine ( scanfile
              , BadWhiteSpace(..)
              ) where

import Control.Monad
import Data.Aeson
import Data.Char
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Text (Text, pack)
import System.Environment
import System.FilePath
import System.IO
import Debug.Trace

data BadWhiteSpace = BadWhiteSpace {
    kind :: Text
  , message :: Text
  , file :: Text
  , line :: Int
} deriving (Eq, Show)

instance ToJSON BadWhiteSpace where
    -- this generates a Value
    toJSON (BadWhiteSpace kind message file line) =
        object [ "type" .= kind, "message" .= message
               , "file" .= file, "line" .= line]

    -- byteString builder
    toEncoding (BadWhiteSpace kind message file line) =
        pairs ("type" .= kind <> "message" .= message <>
               "file" .= file <> "line" .= line)

scanfile path =
  let analyze1 pair@(n, line)
        | makefile path,         -- allow leading tabs in makefiles
          x:xs <- line,
          x == '\t' = analyze (n, xs)
        | otherwise = analyze pair
      analyze :: (Int, String) -> [BadWhiteSpace]
      analyze (n, line) = catMaybes [trailing, tabs]
        where
          trailing
            | (_, xs@(_:_)) <- spanEnd isSpace line =
                let s = case xs of
                          [_] -> mempty
                          otherwise -> "s"
                in Just $ badSpace $
                     count (length xs) <> " trailing whitespace" <> s <>
                     " in line " <> show n
            | otherwise = Nothing
          tabs =
            case filter ('\t' ==) (trimEnd line) of
              [] -> Nothing
              [_] -> Just $ badSpace $ "tab character in line " <> show n
              otherwise -> Just $ badSpace $ "tab characters in line " <> show n
          badSpace message = BadWhiteSpace "bad whitespace" (pack message) packedPath n
          packedPath = pack path
  in do
    lines <- liftM lines (readFile path)
    pure $ concatMap analyze1 (zip [1..] lines)

makefile "Makefile" = True
makefile path = case takeExtension path of
                  ".make" -> True
                  otherwise -> False

count 1 = "one"
count 2 = "two"
count 3 = "three"
count 4 = "four"
count 5 = "five"
count 6 = "six"
count 7 = "seven"
count n = show n
