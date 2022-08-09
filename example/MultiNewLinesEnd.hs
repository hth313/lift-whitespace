module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import System.Environment
import Engine

main :: IO () 
main = do
  files <- getArgs
  result <- liftM (encode . concat) (forM files scanfile)
  L.putStrLn result


