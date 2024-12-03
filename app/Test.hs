import Test.HUnit
import Day2 (isIncreasing, isDecreasing, increasing, decreasing) -- Replace with your module name
import Test.HUnit (Test, assertBool)

-- Test cases for isIncreasing
test1 :: Test
test1 = TestCase (assertBool "Test increasing sequence" (isIncreasing [1, 2, 3]))

test2 :: Test
test2 = TestCase (assertBool "Test non-increasing sequence" (not (isIncreasing [1, 5, 3])))

test3 :: Test
test3 = TestCase (assertBool "Test empty list" (isIncreasing []))

test4 :: Test
test4 = TestCase (assertBool "Test Decreasing" (isDecreasing [7, 6, 4, 2, 1]))

test5 :: Test
test5 = TestCase (assertBool "Case 2" (isIncreasing [1, 3, 6, 7, 9]))

test6 :: Test
test6 = TestCase (assertBool "Case 3" (not (isIncreasing [1, 2, 7, 8, 9])))

test7 :: Test
test7 = TestCase (assertBool "Case 4" (not (isDecreasing [9, 7, 6, 2, 1])))

test8 :: Test
test8 = TestCase (assertBool "Case 5" (not (isIncreasing [1, 3, 2, 4, 5])))

test9 :: Test
test9 = TestCase (assertBool "Case 6" (not (isDecreasing [8, 6, 4, 4, 1])))


-- Combine all tests
tests :: Test
tests = TestList [TestLabel "Test1" test1,
                  TestLabel "Test2" test2,
                  TestLabel "Test3" test3,
                  TestLabel "Test4" test4,
                  TestLabel "Test5" test5,
                  TestLabel "Test6" test6,
                  TestLabel "Test7" test7,
                  TestLabel "Test8" test8,
                  TestLabel "Test9" test9]


-- Main function to run the tests
main :: IO ()
main = runTestTTAndExit tests