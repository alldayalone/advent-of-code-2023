module Test(main) where
import Main (walk, walk', extendGrid, extendMatrix)
import Data.Matrix as Matrix (fromLists)

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "walk" $ do
    describe "walk from start (2)" $ do
      it "0 steps" $ do
        walk ([1,2,1], 1) 0 `shouldBe` ([1,2,1], 1)

      it "1 steps" $ do
        walk ([1,2,1], 1) 1 `shouldBe` ([1,1,1,1], 2)
      
      it "2 steps" $ do
        walk ([1,2,1], 1) 2 `shouldBe` ([1,1,1,1], 3)

      it "3 steps" $ do
        walk ([1,2,1], 1) 3 `shouldBe` ([1,2,1], 3)

      it "4 steps" $ do
        walk ([1,2,1], 1) 4 `shouldBe` ([1,2,1,1], 4)

      it "5 steps" $ do
        walk ([1,2,1], 1) 5 `shouldBe` ([1,2,1,1,1], 5)

      it "6 steps" $ do
        walk ([1,2,1], 1) 6 `shouldBe` ([1,2,1,2,1], 5)

      it "more steps" $
        property $ \x -> x < 7 || (walk ([1,2,1], 1) x) == ([1,2,1,x-4,1], 5)

    describe "walk from start (3)" $ do
      it "0 steps" $ do
        walk ([1,3,1], 1) 0 `shouldBe` ([1,3,1], 1)

      it "1 steps" $ do
        walk ([1,3,1], 1) 1 `shouldBe` ([1,1,2,1], 2)
      
      it "2 steps" $ do
        walk ([1,3,1], 1) 2 `shouldBe` ([1,1,1,1,1], 3)

      it "3 steps" $ do
        walk ([1,3,1], 1) 3 `shouldBe` ([1,2,1,1], 3)

      it "4 steps" $ do
        walk ([1,3,1], 1) 4 `shouldBe` ([1,3,1], 3)

      it "5 steps" $ do
        walk ([1,3,1], 1) 5 `shouldBe` ([1,3,1,1], 4)

      it "6 steps" $ do
        walk ([1,3,1], 1) 6 `shouldBe` ([1,3,1,1,1], 5)

      it "more steps" $
        property $ \x -> x < 7 || (walk ([1,3,1], 1) x) == ([1,3,1,x-5,1], 5)

    describe "walk from end" $ do
      it "0    steps" $ do
        walk ([1], 3) 0 `shouldBe` ([1], 3)
      
      it "1    steps" $ do
        walk ([1], 3) 1 `shouldBe` ([1,1], 4)

      it "2    steps" $ do
        walk ([1], 3) 2 `shouldBe` ([1,1,1], 5)

      it "more steps" $ do
        property $ \x -> x < 3 || (walk ([1], 3) x) == ([1,x-1,1], 5)

    describe "walk left" $ do
      it "0 steps" $ do
        walk ([1,3,1], 3) 0 `shouldBe` ([1,3,1], 3)

      it "1 steps" $ do
        walk ([1,3,1], 3) (-1) `shouldBe` ([1,2,1,1], 3)
      
      it "2 steps" $ do
        walk ([1,3,1], 3) (-2) `shouldBe` ([1,1,1,1,1], 3)

      it "3 steps" $ do
        walk ([1,3,1], 3) (-3) `shouldBe` ([1,1,2,1], 2)

      it "4 steps" $ do
        walk ([1,3,1], 3) (-4) `shouldBe` ([1,3,1], 1)

      it "5 steps" $ do
        walk ([1,3,1], 3) (-5) `shouldBe` ([1,1,3,1], 1)

      it "6 steps" $ do
        walk ([1,3,1], 3) (-6) `shouldBe` ([1,1,1,3,1], 1)

      it "more steps" $
        property $ \x -> x > (-7) || (walk ([1,3,1], 3) x) == ([1,-x-5,1,3,1], 1)


    describe "bug fixes" $ do
      it "1" $ do
        walk'([1,2,1], 3) 5 `shouldBe` ([1,2,1,4,1], 5)

      it "2" $ do
        walk' ([1], 1) 5 `shouldBe` ([1,4,1], 3)

      it "3" $ do
        walk' ([1], 1) (-5) `shouldBe` ([1,4,1], 1)
        
      it "4" $ do
        walk' ([1,7,1,1,3,1,4,1,1,1,1,1,1,6,1,3,1,2,1,4,1,1,1,3,1,1,4,1,1,1,3,1,1,1,1,1,1,1,2,1,4,1,3,1,1,1,1,3,1,3,1,8,1,6,1], 4) (-10) `shouldBe` ([1,1,7,1,1,3,1,4,1,1,1,1,1,1,6,1,3,1,2,1,4,1,1,1,3,1,1,4,1,1,1,3,1,1,1,1,1,1,1,2,1,4,1,3,1,1,1,1,3,1,3,1,8,1,6,1], 1)

      it "5" $ do
        walk ([1,7,1,1], 4) (-10) `shouldBe` ([1,1,7,1,1], 1)

      it "6" $ do
        walk ([1,1,7,1], 1) 10 `shouldBe` ([1,1,7,1,1], 5)

      it "7" $ do
        walk ([1,7,1], 1) 9 `shouldBe` ([1,7,1,1], 4)
  describe "extendGrid" $ do
    describe "forward" $ do
      it "0,3" $ do
        extendGrid ([".", "#", "."], [1,2,1], [1,2,1], ".") 
          `shouldBe` [".", "#", "."]

      it "1,2" $ do
        extendGrid ([".", "#", "."], [1,2,1], [1,1,1,1], ".") 
          `shouldBe` [".", "#", "#", "."]

      it "4" $ do
        extendGrid ([".", "#", "."], [1,2,1], [1,2,1,1], ".") 
          `shouldBe`  [".", "#", ".", "."]
        
      it "5" $ do
        extendGrid ([".", "#", "."], [1,2,1], [1,2,1,1,1], ".") 
          `shouldBe` [".", "#", ".", ".","."]

      it "6" $ do
        extendGrid ( [".", "#", "."], [1,2,1], [1,2,1,2,1], ".") 
          `shouldBe`  [".", "#", ".", ".","."]

      it "more steps" $
          property $ \x -> x < 7 || 
            (extendGrid ([".", "#", "."], [1,2,1], [1,2,1,x-5,1], ".")) 
              ==  [".", "#", ".", ".","."]

    describe "extendMatrix" $ do
      it "0,3" $ do
        extendMatrix (Matrix.fromLists [[".", "."], ["#", "."], [".", "."]]) [1,2,1] [1,2,1] 
          `shouldBe` (Matrix.fromLists [[".", "."], ["#", "."], [".", "."]])

      it "1,2" $ do
        extendMatrix (Matrix.fromLists [[".", "."], ["#", "."], [".", "."]]) [1,2,1] [1,1,1,1]
          `shouldBe` (Matrix.fromLists [[".", "."], ["#", "."], ["#", "."], [".", "."]])
        
      it "4" $ do
        extendMatrix (Matrix.fromLists [[".", "."], ["#", "."], [".", "."]]) [1,2,1] [1,2,1,1]
          `shouldBe` (Matrix.fromLists [[".", "."], ["#", "."], [".", "."], [".", "."]])
         
      it "debug1" $ do
        extendMatrix (Matrix.fromLists [["#"]]) [1] [1,5,1]
          `shouldBe` (Matrix.fromLists [["#"], ["."], ["."]])

      it "debug2" $ do
        extendMatrix (Matrix.fromLists [["#", "#", "#"]]) [1] [1,4,1]
          `shouldBe` (Matrix.fromLists [["#", "#", "#"], [".", ".", "."], [".", ".", "."]])
