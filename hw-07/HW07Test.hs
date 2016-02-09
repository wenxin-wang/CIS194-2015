module HW07Test where

import HW07
import Testing

ex2Tests :: [Test]
ex2Tests = [ testF2 "liftM test" liftM
             [((+1), Just 5, Just 6)]
           ]


allTests :: [Test]
allTests = concat [ ex2Tests
                  ]
