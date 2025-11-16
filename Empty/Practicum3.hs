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



-- Exercises Arithmetical Expression

data IntExp  = Lit Int | Add IntExp IntExp | Mul IntExp IntExp
  deriving Show

showintexp :: IntExp -> String
showintexp (Lit a)    = show a
showintexp (Add a b)  = "(" ++ showintexp a ++ "+" ++ showintexp b ++ ")"
showintexp (Mul a b)  = "(" ++ showintexp a ++ "*" ++ showintexp b ++ ")"

evalintexp :: IntExp -> Int
evalintexp (Lit a)    = a
evalintexp (Add a b)  = evalintexp a + evalintexp b
evalintexp (Mul a b)  = evalintexp a * evalintexp b


-- Exercises Combinatory Logic
data Term = S | K | I | App Term Term

instance Show Term where
  show a = showterm a


showterm :: Term -> String
showterm (App a b) = "(" ++ showterm a ++ showterm b ++ ")"
showterm S = "S"
showterm K = "K"
showterm I = "I"


isredex :: Term -> Bool
isredex (App I _) = True
isredex (App (App K _) _ ) = True
isredex (App (App (App S _) _ ) _) = True
isredex _ = False


hasredex :: Term -> Bool
hasredex (App a b) =  isredex (App a b) || hasredex a || hasredex b
hasredex _ = False

isnormalform :: Term -> Bool
isnormalform a = not (hasredex a)


headstep :: Term -> Term
headstep (App I p) = p
headstep (App (App K p) q ) = p
headstep (App (App (App S p) q ) r) = App (App p r) (App q r)
headstep p = p


doall :: Term -> Term
doall (App a b) | (hasredex (App a b) == True) =  if isredex (App a b) 
                                                    then headstep (App a b)
                                                  else (App (doall a) (doall b))
                | otherwise = (App a b)


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
