module Test(main) where
import Main (splitSegment, splitSegmentUnsigned)

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
    