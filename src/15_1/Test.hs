module Test(main) where
import Main (hash)
import qualified System.Exit as Exit
import Test.HUnit

tests :: Test
tests = TestList [
  TestLabel "1" $ TestCase (assertEqual "HASH" 52 (hash "HASH"))
  , TestLabel "2" $ TestCase (assertEqual "rn=1" 30 (hash "rn=1"))
  , TestLabel "3" $ TestCase (assertEqual "cm-" 253 (hash "cm-"))
  ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess