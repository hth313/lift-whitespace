{-# LANGUAGE OverloadedStrings #-}
module Main where

import Engine
import Test.Hspec

main :: IO ()
main = do
  run1 <- fmap concat (mapM scanfile [ "example/Spaced1"
                                     , "example/Spaced2"])
  run2 <- fmap concat (mapM scanfile [ "example/NotSpaced" ])
  run3 <- fmap concat (mapM scanfile [ "example/empty" ])
  run4 <- fmap concat (mapM scanfile [ "example/SpaceAtEnd" ])
  run5 <- fmap concat (mapM scanfile [ "example/MultiNewLinesEnd" ])

  hspec $ do
    it "Unsuccessful run" $ do
      run1 `shouldBe` expect1
    it "Check successful run" $ do
      run2 `shouldBe` []
    it "Empty file" $ do
      run3 `shouldBe` []
    it "Space at end" $ do
      run4 `shouldBe` expect4
    it "Multi newlines at end" $ do
      run5 `shouldBe` expect5

  where
    expect1 = [ BadWhiteSpace
                  "bad whitespace"
                  "two trailing whitespaces"
                  "example/Spaced1"
                  5
              , BadWhiteSpace
                  "bad whitespace"
                  "tab character"
                  "example/Spaced1"
                  5
              , BadWhiteSpace
                  "bad whitespace"
                  "six trailing whitespaces"
                  "example/Spaced1"
                  9
              , BadWhiteSpace
                  "bad whitespace"
                  "25 trailing whitespaces"
                  "example/Spaced1"
                  13
              , BadWhiteSpace
                  "bad whitespace"
                  "one trailing whitespace"
                  "example/Spaced2"
                  9
              ]
    expect4 = [ BadWhiteSpace
                    "bad whitespace"
                    "one trailing whitespace"
                    "example/SpaceAtEnd"
                    9
              , BadWhiteSpace
                    "bad whitespace"
                    "five trailing whitespaces"
                    "example/SpaceAtEnd"
                    14
              , BadWhiteSpace
                    "file ending"
                    "whitespace at end of file"
                    "example/SpaceAtEnd"
                    14
              ]
    expect5 = [ BadWhiteSpace
                    "bad whitespace"
                    "one trailing whitespace"
                    "example/MultiNewLinesEnd"
                    9
              , BadWhiteSpace
                    "file ending"
                    "extra newlines at end of file"
                    "example/MultiNewLinesEnd"
                    15
              ]
