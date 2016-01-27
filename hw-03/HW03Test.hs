module HW03Test where

import HW03
import Testing

testRun :: Statement -> (Int,  Int) -> Bool
testRun fun (i, o) = s "Out" == o
  where s = run (extend empty "In" i) fun

factorialTests :: [Test]
factorialTests = [ Test "factorial test" (testRun factorial)
                  [(1, 1), (2, 2), (4, 24), (5, 120)]
                ]

squareRootTests :: [Test]
squareRootTests = [ Test "squareRoot test" (testRun squareRoot)
                  [(1, 1), (2, 1), (4, 2)]
                ]

fibonacciTests :: [Test]
fibonacciTests = [ Test "fibonacci test" (testRun fibonacci)
                  [(0, 1), (1, 1), (5, 8), (6, 13)]
                ]

allTests :: [Test]
allTests = concat [ factorialTests
                  , squareRootTests
                  , fibonacciTests
                  ]
