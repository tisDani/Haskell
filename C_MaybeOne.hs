module Practicum2B where
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)


-- Definition of the monad MaybeOne used for partial functions
data MaybeOne a = NoResult
                | Result a deriving (Show, Eq)

instance Applicative MaybeOne where
    pure x = Result x
    (<*>)  = ap

instance Functor MaybeOne where
    fmap = liftM

instance Monad MaybeOne where
  NoResult   >>= _ = NoResult
  (Result n) >>= f = f n
  return x         = Result x


-- Partial function for division
myDividedBy :: Double -> Double -> MaybeOne Double
myDividedBy n d =
  if d  == 0
  then NoResult
  else Result (n / d)

-- Find the index of a number in a list
myIndexOf :: [Double] -> Double -> MaybeOne Int
myIndexOf l n = 
  if (myIndexOfAux l n == []) then NoResult
  else Result (length l - length(myIndexOfAux l n) + 1)

myIndexOfAux :: [Double] -> Double -> [Double] 
myIndexOfAux [] n = []
myIndexOfAux (l:ls) n |(n == l)  = (l:ls)
                      |(n /= l)  = (myIndexOfAux ls n )

-- Find the remainder of a string after removing a prefix
myRemainderString :: String -> String -> MaybeOne String
myRemainderString x y = 
  if (isPrefix x y == False)  then  NoResult
  else Result (drop (length x) y)

isPrefix :: String -> String -> Bool
isPrefix [] y = True
isPrefix x [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

-- Create an operator for our divide function
n // d = n `myDividedBy` d 

-- Example f using case (1/2)
f :: Double -> Double -> Double -> MaybeOne Double
f x y z = case x // y of
    NoResult           -> NoResult
    Result xDividedByy ->
      case xDividedByy // z of
        NoResult   -> NoResult
        Result   r -> Result r

-- Example f using case (2/2)
fShorter :: Double -> Double -> Double -> MaybeOne Double
fShorter x y z = case x // y of
    NoResult           -> NoResult
    Result xDividedByy -> xDividedByy // z

-- Example g using case
g :: Double -> Double -> Double -> Double -> MaybeOne Double
g x y z s =
  case x // y of
    NoResult           -> NoResult
    Result xDividedByy ->
      case y // z of
        NoResult           -> NoResult
        Result xDividedByz ->
          case y // s of
            NoResult           -> NoResult
            Result yDividedBys ->
              case z // s of
                NoResult           -> NoResult
                Result zDividedBys ->
                  let n = yDividedBys + zDividedBys
                      d = xDividedByy - xDividedByz
                  in n // d


-- Divide function for MaybeOne
(//) :: Double -> Double -> MaybeOne Double
v1 :: Double -> Double -> Double -> Double -> MaybeOne Double
v1 x y z s = 
  case x // y of
    NoResult           -> NoResult
    Result xDividedByy ->
      case z // s of
        NoResult           -> NoResult
        Result zDividedBys ->
          case y // s of
            NoResult           -> NoResult
            Result yDividedBys ->
              let zsMinusys = zDividedBys - yDividedBys in
              case z // x of
                NoResult           -> NoResult
                Result zDividedByx ->
                  case xDividedByy // zsMinusys of
                    NoResult            -> NoResult
                    Result xyDividedByd ->
                      let ysPluszx = yDividedBys + zDividedByx in
                      Result (xyDividedByd - ysPluszx)

-- Example f using >==
fBetter :: Double -> Double -> Double -> MaybeOne Double
fBetter x y z = (x // y) >>= dividedByZ
  where dividedByZ xdividedByy = xdividedByy // z

-- Example f using >= and lambda 
fBetterLambda :: Double -> Double -> Double -> MaybeOne Double
fBetterLambda x y z = (x // y) >>= (\xDividedByy -> xDividedByy // z)

-- Example g using >== and lambda
gBetter :: Double -> Double -> Double -> Double -> MaybeOne Double
gBetter x y z s =
  (x // y) >>=
  (\xDividedByy ->
    (x // z) >>=
    (\xDividedByz ->
      let d = xDividedByy - xDividedByz
      in (y // s) >>=
      (\yDividedBys ->
        (z // s) >>=
        (\zDividedBys ->
          let n = yDividedBys + zDividedBys
          in n // d
        )
      )
    )
  )

-- Alternate divide function for MaybeOne
v2 :: Double -> Double -> Double -> Double -> MaybeOne Double
v2 x y z s = 
  (x // y) >>=
  (\xDividedByy -> 
    (z // s) >>=
    (\zDividedBys -> 
      (y // s) >>=
      (\yDividedBys -> 
        let zsMinusys = zDividedBys - yDividedBys in
        (z // x) >>=
        (\zDividedByx -> 
          (xDividedByy // zsMinusys) >>=
          (\xyDividedByd -> 
            let ysPluszx = yDividedBys + zDividedByx in
            Result (xyDividedByd - ysPluszx)
            )
          )
        )
      )
    )

-- Example f using do
fDo :: Double -> Double -> Double -> MaybeOne Double
fDo x y z = do
  xDividedByy <- x // y
  xDividedByy // z

-- Example g using do
gDo :: Double -> Double -> Double -> Double -> MaybeOne Double
gDo x y z s = do
  xDividedByy <- x // y
  xDividedByz <- y // z
  let d = xDividedByy - xDividedByz
  yDividedBys <- y // s
  zDividedBys <- z // s
  let n = yDividedBys + zDividedBys
  n // d

-- Example f using do-return
fPerfect :: Double -> Double -> Double -> MaybeOne Double
fPerfect x y z = do
  xDividedByy <- x // y
  result      <- xDividedByy // z
  return result

-- Example g using do-return
gPerfect :: Double -> Double -> Double -> Double -> MaybeOne Double
gPerfect x y z s = do
  xDividedByy <- x // y
  xDividedByz <- y // z
  let denominator = xDividedByy - xDividedByz
  yDividedBys <- y // s
  zDividedBys <- z // s
  let numerator = yDividedBys + zDividedBys
  result <- numerator // denominator
  return result

-- Alternate divide function for MaybeOne
v3 :: Double -> Double -> Double -> Double -> MaybeOne Double
v3 x y z s = do
  xDividedByy <- x // y
  zDividedBys <- z // s
  yDividedBys <- y // s
  let zsMinusys = zDividedBys - yDividedBys
  zDividedByx <- z // x
  xyDividedByd <- xDividedByy // zsMinusys 
  let ysPluszx = yDividedBys + zDividedByx
  result  <-  Result (xyDividedByd - ysPluszx)
  return result