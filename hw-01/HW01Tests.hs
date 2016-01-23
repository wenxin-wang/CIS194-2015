-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------
testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n ,l) = toRevDigits n == l

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(123, [3,2,1]), (1, [1]), (0, []), (-17, [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (l1, l2) = doubleEveryOther l1 == l2

ex3Tests :: [Test]
ex3Tests = [ Test "testDoubleEveryOther test" testDoubleEveryOther
             [([4,9,5,5], [4,18,5,10]), ([0,0],[0,0]), ([1], [1]), ([], [])]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (l, n) = sumDigits l == n
ex4Tests :: [Test]
ex4Tests = [ Test "testSumDigits test" testSumDigits
             [([], 0), ([10,5,18,4], 19), ([-17], 0)]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (l, b) = luhn l == b
ex5Tests :: [Test]
ex5Tests = [ Test "testLuhn test" testLuhn
             [(5491553626258969, True), (123, False), (6011111178081641, True)]
           ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, src, dst, stg, mvs) = hanoi n src dst stg == mvs
testHanoiLength :: (Integer, Peg, Peg, Peg, Int) -> Bool
testHanoiLength (n, src, dst, stg, steps) = length (hanoi n src dst stg) == steps
testHanoi4Length :: (Integer, Peg, Peg, Peg, Peg, Int) -> Bool
testHanoi4Length (n, src, dst, stg1, stg2, steps) = length (hanoi4 n src dst stg1 stg2) == steps

ex6Tests :: [Test]
ex6Tests = [ Test "testHanoi test" testHanoi
             [(2, "a", "b", "c", [("a", "c"), ("a", "b"), ("c", "b")])],
             Test "testHanoiLength test" testHanoiLength
             [(2, "a", "b", "c", 3), (15, "a", "b", "c", 32767)],
             Test "testHanoi4Length test" testHanoi4Length
             [(2, "a", "b", "c", "d", 3), (15, "a", "b", "c", "d", 129)]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
