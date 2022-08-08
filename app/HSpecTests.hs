{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Engine
import Test.Hspec

main :: IO ()
main = do
  run1 <- liftM concat (mapM scanfile [ "example/Spaced1.hs"
                                      , "example/Spaced2.hs"])
  run2 <- liftM concat (mapM scanfile [ "example/NotSpaced.hs" ])

  hspec $ do
    it "Unsuccessful run" $ do
      run1 `shouldBe` expect1
    it "Check successful run" $ do
      run2 `shouldBe` []
  where
    expect1 = [ BadWhiteSpace
                  "bad whitespace"
                  "two trailing whitespaces in line 5"
                  "example/Spaced1.hs"
                  5
              , BadWhiteSpace
                  "bad whitespace"
                  "tab character in line 5"
                  "example/Spaced1.hs"
                  5
              , BadWhiteSpace
                  "bad whitespace"
                  "six trailing whitespaces in line 9"
                  "example/Spaced1.hs"
                  9
              , BadWhiteSpace
                  "bad whitespace"
                  "25 trailing whitespaces in line 13"
                  "example/Spaced1.hs"
                  13
              , BadWhiteSpace
                  "bad whitespace"
                  "one trailing whitespace in line 9"
                  "example/Spaced2.hs"
                  9
              ]
