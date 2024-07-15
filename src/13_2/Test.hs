module Test(main) where
import Main (longestPalindrome)
import Data.Vector as Vector (Vector, eqBy, take, tail, reverse, fromList, length, replicate, slice, (//), empty, singleton)
import qualified System.Exit as Exit
import Test.HUnit

tests :: Test
tests = TestList [
  TestLabel "test1" $ TestCase (assertEqual "Should solve aabbac" (Vector.fromList "aabbaa", 0) (longestPalindrome (Vector.fromList "aabbaac")))
  , TestLabel "test2" $ TestCase (assertEqual "Should solve abbaabb" (Vector.fromList "bbaabb", 1) (longestPalindrome (Vector.fromList "abbaabb")))
  , TestLabel "test3" $ TestCase (assertEqual "Should solve abcaabb" (Vector.fromList "bb", 5) (longestPalindrome (Vector.fromList "abcaabb")))
  ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess