module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend s name v x = if x == name then v else s x

empty :: State
empty = const 0

-- Exercise 2 -----------------------------------------

bInt :: Bool -> Int
bInt True = 1
bInt False = 0

iBool :: Int -> Bool
iBool 1 = True
iBool 0 = False

evalE :: State -> Expression -> Int
evalE s (Var name) = s name
evalE _ (Val i) = i
evalE s (Op e1 bop e2) =
  case bop of
    Plus -> ve1 + ve2
    Minus -> ve1 - ve2
    Times -> ve1 * ve2
    Divide -> ve1 `div` ve2
    Gt -> bInt $ ve1 > ve2
    Ge -> bInt $ ve1 >= ve2
    Lt -> bInt $ ve1 < ve2
    Le -> bInt $ ve1 <= ve2
    Eql -> bInt $ ve1 == ve2
  where
    ve1 = evalE s e1
    ve2 = evalE s e2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign n v)= DAssign n v
desugar (Incr n)= DAssign n (Op (Var n) Plus (Val 1))
desugar (If p t f)= DIf p (desugar t) (desugar f)
desugar (While p s)= DWhile p (desugar s)
desugar (For asn p incr s)= DSequence (desugar asn) (DWhile p (desugar $ Sequence s incr))
desugar (Sequence s1 s2)= DSequence (desugar s1) (desugar s2)
desugar Skip = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign n v) = extend s n $ evalE s v
evalSimple s (DIf p t f) = evalSimple s $ if iBool $ evalE s p then t else f
evalSimple s dw@(DWhile p ds) = if iBool $ evalE s p then evalSimple (evalSimple s ds) dw else s
evalSimple s (DSequence ds1 ds2) = evalSimple (evalSimple s ds1) ds2
evalSimple s DSkip = s

run :: State -> Statement -> State
run s = evalSimple s . desugar

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   Out := 0;
   while (In >= Out * Out) {
     Out++
   };
   Out := Out - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "Out" (Val 0)
                   , While (Op (Var "In") Ge (Op (Var "Out") Times (Var "Out")))
                       (Incr "Out")
                   , Assign "Out" (Op (Var "Out") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
