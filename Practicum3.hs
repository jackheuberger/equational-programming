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
hasredex = undefined

isnormalform :: Term -> Bool
isnormalform = undefined

headstep :: Term -> Term
headstep = undefined

doall :: Term -> Term
doall = undefined



-- Exercises Equational Specifications
data Thing = Undefined1
  deriving (Show, Eq, Bounded, Enum)

nxt :: Thing -> Thing
nxt = undefined

-- 
data I = Undefined2
  deriving (Show, Eq, Bounded, Enum)

s :: I -> I
s = undefined

p :: I -> I
p = undefined

