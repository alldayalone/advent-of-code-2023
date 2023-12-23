module Test(main) where
import Main (CardRank(..), Hand(..), count, parseHand, parseCardRank, replaceJokers)
import qualified System.Exit as Exit
import Test.HUnit
 
test1 :: Test
test1 = TestCase (assertEqual "1" True (FiveOfAKind (map parseCardRank "AAAAA") == FiveOfAKind (map parseCardRank "AAAAA")))

test2 :: Test
test2 = TestCase (assertEqual "2" True (FiveOfAKind(map parseCardRank "AAAAA") > FiveOfAKind (map parseCardRank "KAAAA")))

test3 :: Test
test3 = TestCase (assertEqual "3" True (FourOfAKind (map parseCardRank "KAAAA") < FourOfAKind (map parseCardRank "A3333")))

test4 :: Test
test4 = TestCase (assertEqual "4" [(Ace, 5)] ((reverse . count .map parseCardRank) "AAAAA"))

test5 :: Test
test5 = TestCase (assertEqual "4" [(Ace, 4), (King,1)] ((reverse . count .map parseCardRank) "AAAKA"))

test6 :: Test
test6 = TestCase (assertEqual "4" [(Ace, 3), (King,2)] ((reverse . count .map parseCardRank) "AKAKA"))

tests :: Test
tests = TestList [
    TestLabel "test1" test1
    ,TestLabel "test2" test2
    ,TestLabel "test3" test3
    ,TestLabel "test4" test4
    ,TestLabel "test5" test5
    ,TestLabel "test6" test6
    ,TestLabel "test7" (TestCase (assertEqual "5" (OnePair (map parseCardRank "32T3K")) (parseHand "32T3K")))
    ,TestLabel "test8" (TestCase (assertEqual "5" True (FourOfAKind [Ten,Five,Five,Joker,Five] < FourOfAKind [Queen,Queen,Queen,Joker,Ace])))
    ,TestLabel "test9" (TestCase (assertEqual "5" True (OnePair [Three,Two,Ten,Three,King] < FourOfAKind [Queen,Queen,Queen,Joker,Ace])))
    ,TestLabel "test9" (TestCase (assertEqual "5" [(King,1), (Ace, 4)] (replaceJokers [(Joker,1),(King,1), (Ace, 3)])))
    ,TestLabel "test9" (TestCase (assertEqual "5" [(King,1), (Ten, 4)] (replaceJokers [(King,1), (Ten, 2), (Joker,2)])))
    ]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess