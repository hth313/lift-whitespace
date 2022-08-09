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
  run3 <- liftM concat (mapM scanfile [ "example/empty" ])
  run4 <- liftM concat (mapM scanfile [ "example/SpaceAtEnd.hs" ])
  run5 <- liftM concat (mapM scanfile [ "example/MultiNewLinesEnd.hs" ])

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
                  "example/Spaced1.hs"
                  5
              , BadWhiteSpace
                  "bad whitespace"
                  "tab character"
                  "example/Spaced1.hs"
                  5
              , BadWhiteSpace
                  "bad whitespace"
                  "six trailing whitespaces"
                  "example/Spaced1.hs"
                  9
              , BadWhiteSpace
                  "bad whitespace"
                  "25 trailing whitespaces"
                  "example/Spaced1.hs"
                  13
              , BadWhiteSpace
                  "bad whitespace"
                  "one trailing whitespace"
                  "example/Spaced2.hs"
                  9
              ]
    expect4 = [ BadWhiteSpace
                    "bad whitespace"
                    "one trailing whitespace"
                    "example/SpaceAtEnd.hs"
                    9
              , BadWhiteSpace
                    "bad whitespace"
                    "five trailing whitespaces"
                    "example/SpaceAtEnd.hs"
                    14
              , BadWhiteSpace
                    "file ending"
                    "whitespace at end of file"
                    "example/SpaceAtEnd.hs"
                    14
              ]
    expect5 = [ BadWhiteSpace
                    "bad whitespace"
                    "one trailing whitespace"
                    "example/MultiNewLinesEnd.hs"
                    9
              , BadWhiteSpace
                    "file ending"
                    "extra newlines at end of file"
                    "example/MultiNewLinesEnd.hs"
                    15
              ]
