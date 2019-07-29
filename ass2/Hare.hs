{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

data RE :: * -> * where 
  Empty   :: RE ()
  Fail    :: RE a
  Char    :: [Char] -> RE Char
  Seq     :: RE a -> RE b -> RE (a, b)
  Choose  :: RE a -> RE a -> RE a
  Star    :: RE a -> RE [a]
  Action  :: (a -> b) -> RE a -> RE b

match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure ()
match Fail = failure
match (Char cs) = do
  x <- readCharacter
  guard (x `elem` cs)
  pure x
match (Seq a b) = do
  ra <- match a
  rb <- match b
  pure (ra, rb)
match (Choose a b) =
      match a
  <|> match b
match (Star a) = 
        addFront <$> match a <*> match (Star a)
    <|> pure []
  where
    addFront x xs = (x:xs)
match (Action f a) = f <$> match a

matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)

infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action flattenTuple (x `Seq` xs)

-- TODO: using accumulator??
string :: String -> RE String
string []     = emptyRE
string (x:xs) = cons (Char [x]) (string xs)
                        
rpt :: Int -> RE a -> RE [a]
rpt 0 re = emptyRE
rpt n re = cons (re) (rpt (n-1) re)

rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) re
    | (x > y)   = Action (const []) Fail
    | otherwise = (rptRange (x+1, y) re) `Choose` (rpt x re)

option :: RE a -> RE (Maybe a)
option re = (Action Just re) `Choose` (Action (\_ -> Nothing) Empty)

plus :: RE a -> RE [a]
plus re = Action flattenTuple (re `Seq` Star re)

choose :: [RE a] -> RE a
choose res = chooser res Fail
  where chooser :: [RE a] -> RE a -> RE a
        chooser [] acc      = acc
        chooser (x:xs) acc  = chooser xs (acc `Choose` x)

-- Helpers
flattenTuple :: (a, [a]) -> [a]
flattenTuple (x, xs) = (x:xs)

emptyRE :: RE [t]
emptyRE = Action (const []) Empty
