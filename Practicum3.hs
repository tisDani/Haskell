module Practicum3 where

{-    
Name:           <Daniela Mackay>
VU-net id:      <dmt380>
Student number: <2663452>
Discussed with: 
Remarks:        
Sources:        
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



doall :: Term -> Term  --                              
doall (App a b) | (hasredex (App a b) == True) =  if isredex (App a b) 
                                                    then headstep (App (headstep a) (headstep b))
                                                   else (App (headstep a) (headstep b))
                | otherwise = (App a b)
doall x = x 




{- version of doall where term is reduced to normal form, all redexes are reduced
doall :: Term -> Term  --                              
doall (App a b) | (hasredex (App a b) == True) =  if isredex (App a b) 
                                                    then doall (headstep (App a b))
                                                   else if isredex (a) 
                                                    then doall (App (headstep a) b)
                                                  else doall (App a (headstep b))
                | otherwise = (App a b)
doall x = x 
-}

-- Exercises Equational Specifications
data Thing = A | C | D  
  deriving (Show, Eq, Bounded, Enum)

nxt :: Thing -> Thing
nxt C = A 
nxt A = D
nxt D = C

-- 
data I = Zero | One
  deriving (Show, Eq, Bounded, Enum)

s :: I -> I
s Zero = One
s One = Zero

p :: I -> I
p Zero = One
p One = Zero

