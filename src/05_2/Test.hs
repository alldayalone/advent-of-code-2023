module Test(main) where
import Main (breakPointsToRanges,rangesToBreakPoints,splitRangeByBreakPoints,mapSeed,mapSeeds)
import qualified System.Exit as Exit
import Test.HUnit
 
test1 :: Test
test1 = TestCase (assertEqual "should return []" [] (breakPointsToRanges []))

test2 :: Test
test2 = TestCase (assertEqual "should return correct for single pt" [(10, 1)] (breakPointsToRanges [10]))

test3 :: Test
test3 = TestCase (assertEqual "should return correct for 2 pts" [(10, 5)] (breakPointsToRanges [10, 14]))

test4 :: Test
test4 = TestCase (assertEqual "should return correct for 3 pts" [(10, 4), (14, 3)] (breakPointsToRanges [10, 14, 16]))
 
test5 :: Test
test5 = TestCase (assertEqual "should generate breakpoints for single range" [1, 100] (rangesToBreakPoints (1, 100) []))

test6 :: Test
test6 = TestCase (assertEqual "should generate breakpoints for 1 range and 1 mappers" [1, 10, 13, 100] (rangesToBreakPoints (1, 100) [(100, 10, 3)]))

test7 :: Test
test7 = TestCase (assertEqual "should generate breakpoints for 1 range and 2 mappers" [1, 10, 13, 15, 100] (rangesToBreakPoints (1, 100) [(100, 10, 3), (0, 13, 2)]))

test7_2 :: Test
test7_2 = TestCase (assertEqual "should generate breakpoints for 1 range and 1 mapper out of range" [82, 83] (rangesToBreakPoints (82, 2) [(50, 98, 2), (52, 50, 48)]))

test8 :: Test
test8 = TestCase (assertEqual "should split 1 range with 2 mappers" [(1, 9), (10, 3), (13, 2), (15, 86)] (splitRangeByBreakPoints (1, 100) [(100, 10, 3), (0, 13, 2)]))

test8_2 :: Test
test8_2 = TestCase (assertEqual "should split 1 range with 1 mapper out of range" [(82, 2)] (splitRangeByBreakPoints (82, 2) [(50, 98, 2), (52, 50, 48)]))

test9 :: Test
test9 = TestCase (assertEqual "should map seed" [(1, 9), (100, 3), (0, 2), (15, 86)] (mapSeed (1, 100) [(100, 10, 3), (0, 13, 2)]))

test9_2 :: Test
test9_2 = TestCase (assertEqual "should map 1 seed with 1 mapper out of range" [(84, 2)] (mapSeed (82, 2) [(50, 98, 2), (52, 50, 48)]))

test10 :: Test
test10 = TestCase (assertEqual "should map multiple seeds" [(1, 9), (100, 3), (0, 2), (15, 86), (120, 2)] (mapSeeds [(1, 100), (120, 2)] [(100, 10, 3), (0, 13, 2)]))

-- Full Case
test11 :: Test
test11 = TestCase  (assertEqual "should map multiple seeds" [(1, 9), (100, 3), (0, 2), (15, 86), (120, 2)] (mapSeeds [(1, 100), (120, 2)] [(100, 10, 3), (0, 13, 2)]))


tests :: Test
tests = TestList [
    TestLabel "test1" test1
    ,TestLabel "test2" test2
    ,TestLabel "test3" test3
    ,TestLabel "test4" test4
    ,TestLabel "test5" test5
    ,TestLabel "test6" test6
    ,TestLabel "test7" test7
    ,TestLabel "test7_2" test7_2
    ,TestLabel "test8" test8
    ,TestLabel "test8_2" test8_2
    ,TestLabel "test9" test9
    ,TestLabel "test9_2" test9_2
    ,TestLabel "test10" test10
    ,TestLabel "test11" test11
    ]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess