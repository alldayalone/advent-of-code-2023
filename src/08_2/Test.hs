module Test(main) where
import qualified System.Exit as Exit
import Test.HUnit
 
tests :: Test
tests = TestList [
    TestLabel "test9" (TestCase (assertEqual "1" True True))
    ]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess