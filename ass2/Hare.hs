{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  Char :: [Char] -> RE Char
  Seq :: RE a -> RE b -> RE (a, b)
  Choose :: RE a -> RE a -> RE a
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b

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
match (Choose a b) = match a <|> match b
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
cons x xs =  Action stringifyTuple (x `Seq` xs)

string :: String -> RE String
string [] = Action (const []) Empty
string (x:xs) = cons (Char [x]) (string xs)
                        
rpt :: Int -> RE a -> RE [a]
rpt n re = error "'rpt' unimplemented"

rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) re = error "'rptRange' unimplemented"

option :: RE a -> RE (Maybe a)
option re = error "'option' unimplemented"

plus :: RE a -> RE [a]
plus re = Action stringifyTuple (re `Seq` Star re)

choose :: [RE a] -> RE a
choose res = error "'choose' unimplemented"

-- Helpers
stringifyTuple :: (a, [a]) -> [a]
stringifyTuple (x, xs) = (x:xs)
