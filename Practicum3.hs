module Practicum3 where

{-
Name:           <Name and family name>
VU-net id:      <VU-net id>
Student number: <Student number>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        <in case you used sources such as books or webpages
                 please mention them here>
-}



-- Exercises Arithmetical Expressions
data IntExp  = Lit Int | Add IntExp IntExp | Mul IntExp IntExp
  deriving Show

showintexp :: IntExp -> String
showintexp (Lit x) = show x
showintexp (Add x y) = "(" ++ showintexp x ++ "+" ++ showintexp y ++ ")"
showintexp (Mul x y) = "(" ++ showintexp x ++ "*" ++ showintexp y ++ ")"

evalintexp :: IntExp -> Int
evalintexp (Lit x) = x
evalintexp (Add x y) = evalintexp x + evalintexp y
evalintexp (Mul x y) = evalintexp x * evalintexp y

-- Exercises Combinatory Logic
data Term = S | K | I | App Term Term

instance Show Term where
  show a = showterm a

showterm :: Term -> String
showterm (App x y) = "(" ++ showterm x ++ showterm y ++ ")"
showterm S = "S"
showterm K = "K"
showterm I = "I"

-- Being a redex is equal to having a term applied to something else
isredex :: Term -> Bool
isredex (App I y) = True
isredex (App (App K x) y) = True
isredex (App (App (App S x) y) z) = True
isredex _ = False

hasredex :: Term -> Bool
hasredex (App x y) = isredex (App x y) || hasredex x || hasredex y
hasredex x = isredex x

isnormalform :: Term -> Bool
isnormalform x = not (hasredex x)

headstep :: Term -> Term
headstep (App I x) = x
headstep (App (App K x) y) = x
headstep (App (App (App S x) y) z) = App (App x y) (App x z)
headstep x = x

-- If the outer term is a redex, simplify. Else, recurse on children
doall :: Term -> Term
doall (App x y) = if isredex (App x y) 
                    then doall (headstep (App x y)) 
                  else if hasredex (App x y) 
                    then doall (App (doall x) (doall y))
                  else (App x y)
doall x = x



-- Exercises Equational Specifications
data Thing = U | V | W
  deriving (Show, Eq, Bounded, Enum)

nxt :: Thing -> Thing
nxt U = V
nxt V = W
nxt W = U

-- 
data I = T | F
  deriving (Show, Eq, Bounded, Enum)

s :: I -> I
s T = F
s F = T

p :: I -> I
p T = F
p F = T

