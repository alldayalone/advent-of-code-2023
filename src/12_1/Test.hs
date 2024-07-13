module Test(main) where
import Main (solve)
import qualified System.Exit as Exit
import Test.HUnit
 
test1 :: Test
test1 = TestCase (assertEqual "Should solve 1" 1 (solve "#.#.###" [1,1,3] Nothing))

test2 :: Test
test2 = TestCase (assertEqual "Should solve 2" 1 (solve "???.###" [1,1,3] Nothing))

test3 :: Test
test3 = TestCase (assertEqual "Should solve 3" 1 (solve "#??.###" [1,1,3] Nothing))

test4 :: Test
test4 = TestCase (assertEqual "Should solve 4" 0 (solve ".??.###" [1,1,3] Nothing))

test401 :: Test
test401 = TestCase (assertEqual "Should solve 401" 0 (solve "?.###" [1,1,3] Nothing))

test402 :: Test
test402 = TestCase (assertEqual "Should solve 402" 0 (solve "###" [1,1,3] Nothing))

test403 :: Test
test403 = TestCase (assertEqual "Should solve 403" 0 (solve "#.###" [1,1,3] Nothing))

test404 :: Test
test404 = TestCase (assertEqual "Should solve 404" 0 (solve "###" [1,3] Nothing))

test405 :: Test
test405 = TestCase (assertEqual "Should solve 405" 0 (solve "#" [3] Nothing))

test5 :: Test
test5 = TestCase (assertEqual "Should solve 5" 1 (solve "#?.###" [1,3] Nothing))

test6 :: Test
test6 = TestCase (assertEqual "Should solve 6" 1 (solve ".?.###" [1,3] Nothing))

test7 :: Test
test7 = TestCase (assertEqual "Should solve 7" 1 (solve "#" [1] Nothing))

test8 :: Test
test8 = TestCase (assertEqual "Should solve 8" 1 (solve "???.###" [1,1,3] Nothing))

test9 :: Test
test9 = TestCase (assertEqual "Should solve 9" 4 (solve ".??..??...?##." [1,1,3] Nothing))

test10 :: Test
test10 = TestCase (assertEqual "Should solve 10" 10 (solve "?###????????" [3,2,1] Nothing))

test11 :: Test
test11 = TestCase (assertEqual "Should solve 11" 0 (solve "####????????" [3,2,1] Nothing))

test12 :: Test
test12 = TestCase (assertEqual "Should solve 12" 10 (solve "???????" [2,1] Nothing))

test13 :: Test
test13 = TestCase (assertEqual "Should solve 13" 6 (solve "??????" [2,1] Nothing))

test14 :: Test
test14 = TestCase (assertEqual "Should solve 14" 4 (solve "????" [1] Nothing))

test15 :: Test
test15 = TestCase (assertEqual "Should solve 15" 4 (solve "#??????" [2,1] Nothing))

test16 :: Test
test16 = TestCase (assertEqual "Should solve 16" 3 (solve "?????" [2,1] Nothing))

test17 :: Test
test17 = TestCase (assertEqual "Should solve 17" 3 (solve "???" [1] Nothing))

test18 :: Test
test18 = TestCase (assertEqual "Should solve 18" 1 (solve "????" [2,1] Nothing))

test19 :: Test
test19 = TestCase (assertEqual "Should solve 19" 0 (solve "???" [2,1] Nothing))

test20 :: Test
test20 = TestCase (assertEqual "Should solve 20" 1 (solve ".????" [2,1] Nothing))

test21 :: Test
test21 = TestCase (assertEqual "Should solve 21" 2 (solve "#????" [2,1] Nothing))

test22 :: Test
test22 = TestCase (assertEqual "Should solve 22" 1 (solve "#??????#??." [2,7] Nothing))

test23 :: Test
test23 = TestCase (assertEqual "Should solve 23" 2 (solve ".??##?????##??#..?#?" [12,2] Nothing))

tests :: Test
tests = TestList [
    TestLabel "test1" test1
    , TestLabel "test2" test2
    , TestLabel "test3" test3
    , TestLabel "test4" test4
    , TestLabel "test401" test401
    , TestLabel "test402" test402
    , TestLabel "test403" test403
    , TestLabel "test404" test404
    , TestLabel "test405" test405
    , TestLabel "test5" test5
    , TestLabel "test6" test6
    , TestLabel "test7" test7
    , TestLabel "test8" test8
    , TestLabel "test9" test9
    , TestLabel "test10" test10
    , TestLabel "test11" test11
    , TestLabel "test12" test12
    , TestLabel "test13" test13
    , TestLabel "test14" test14
    , TestLabel "test15" test15
    , TestLabel "test16" test16
    , TestLabel "test17" test17
    , TestLabel "test18" test18
    , TestLabel "test19" test19
    , TestLabel "test20" test20
    , TestLabel "test21" test21
    , TestLabel "test22" test22
    , TestLabel "test23" test23
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess