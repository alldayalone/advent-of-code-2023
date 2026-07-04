module Test(main) where
import Main (splitSegment, splitSegmentUnsigned, perm, slicesToPart)

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "splitSegmentUnsigned" $ do
    it "splitSegmentUnsigned [(0, 4000)] by 0" $
      splitSegmentUnsigned 0 [(0, 4000)] `shouldBe` [(0, 4000)]
    
    it "splitSegmentUnsigned [(0, 4000)] by 4000" $
      splitSegmentUnsigned 4000 [(0, 4000)] `shouldBe` [(0, 4000)]
    
    it "splitSegmentUnsigned [(0, 4000)] by 20" $
      splitSegmentUnsigned 20 [(0, 4000)] `shouldBe` [(0, 20), (21, 4000)]
    
    it "splitSegmentUnsigned [(0, 20), (21, 4000)] by 30" $
      splitSegmentUnsigned 30 [(0, 20), (21, 4000)] `shouldBe` [(0, 20), (21, 30), (31, 4000)]

    it "splitSegmentUnsigned [(0, 20), (21, 4000)] by 10" $
      splitSegmentUnsigned 10 [(0, 20), (21, 4000)] `shouldBe` [(0, 10), (11, 20), (21, 4000)]

  describe "splitSegment" $ do
    it "splitSegment x > 20 [(0, 4000)]" $
      splitSegment ">" 20 [(0, 4000)] `shouldBe` [(0, 20), (21, 4000)]
    
    it "splitSegment x < 20 [(0, 4000)]" $
      splitSegment "<" 20 [(0, 4000)] `shouldBe` [(0, 19), (20, 4000)]
    
  describe "perm" $ do
    it "perm empty" $
      perm ([] :: [[Int]]) `shouldBe` []

    it "perm 1" $
      perm [[1]] `shouldBe` [[1]]

    it "perm 2" $
      perm [[1,2]] `shouldBe` [[1],[2]]

    it "perm 1x1x1" $
      perm [[1], [2], [3]] `shouldBe` [[1,2,3]]

    it "perm 1x2" $
     perm [[1], [2,3]] `shouldBe` [[1,2],[1,3]]

    it "perm 2x2x2" $
      perm [[1, 2], [3, 4], [5, 6]] `shouldBe` [[1,3,5], [1,3,6], [1,4,5], [1,4,6], [2,3,5], [2,3,6], [2,4,5], [2,4,6]]

  describe "slicesToPart" $ do
    it "slicesToPart 1" $
      slicesToPart [(1, 10)] `shouldBe` ([1], 10)

    it "slicesToPart 2" $
      slicesToPart [(1, 10), (2, 30)] `shouldBe` ([1,2], 300)
