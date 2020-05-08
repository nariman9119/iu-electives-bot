module CourseSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Time

import Course



spec :: Spec
spec = do
  describe "Tests for MyProject module" $ do

    it "2 + 2 = 4" $ do
      2 + 2 `shouldBe` 4

    it "reverse . reverse = id" $ property $ \xs ->
      reverse (reverse xs) == (xs :: [Int])

     