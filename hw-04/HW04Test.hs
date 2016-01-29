module HW03Test where

import HW04
import Testing

ex2Tests :: [Test]
ex2Tests = [ testF2 "eq test" (==)
             [(P [1], P [1], True), (P [], P [], True),
              (P [1,0], P [1], True), (P [0], P [], True),
              (P [], P [1], False), (P [1], P [2], False)]
           ]

ex3Tests :: [Test]
ex3Tests = [ testF1 "show test" show
             [(P [], ""), (P [0], ""),
              (P [1], "1"), (P [0,1], "x"), (P [0,0,1], "x^2"),
              (P [1,1], "x + 1"), (P [1,0,1], "x^2 + 1"), (P [1,0,2], "2x^2 + 1"),
              (P [1,-1,-2], "-2x^2 + -x + 1")
             ]
           ]

ex4Tests :: [Test]
ex4Tests = [ testF2 "plus test" (+)
             [(P [], P [], P []), (P [0], P [], P [0]),
              (P [1], P [1], P [2]), (P [0,1], P [1,0,0], P [1,1,0]),
              (P [1,1], P [1,0,1], P [2,1,1])
             ]
           ]

ex5Tests :: [Test]
ex5Tests = [ testF2 "times test" (*)
             [(P [], P [], P []), (P [0], P [], P [0]), (P [1], P [0], P [0]),
              (P [0,1], P [1,0,0], P [0,1,0,0]), (P [2,1], P [1,1,1], P [2,3,3,1])
             ]
           ]

ex7Tests :: [Test]
ex7Tests = [ testF2 "applyP test" applyP
             [(P [], 1, 0), (P [0], 1, 0),
              (x^2 + 2*x + 1, 1, 4), (x^2 + 2*x + 1, 2, 9)
             ]
           ]

ex9Tests :: [Test]
ex9Tests = [ testF1 "deriv test" deriv
             [(P [], 0), (P [0], 0), (P [1], 0),
              (x^2 + x + 1, 2*x + 1), (3*x^3 + 4*x, 9*x^2 + 4)
             ]
           ]

allTests :: [Test]
allTests = concat [ ex2Tests,
                    ex3Tests,
                    ex4Tests,
                    ex5Tests,
                    ex7Tests,
                    ex9Tests
                  ]
