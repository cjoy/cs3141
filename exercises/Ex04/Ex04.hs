module Ex04 where

import Text.Read (readMaybe)

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
tokenise = mapM parseToken . words

instance Show Token where
  show (Number x) = show x
  show (Operator _) = "opp"

newtype Calc a = C ([Int] -> Maybe ([Int], a))

pop :: Calc Int
pop = C popper
  where popper :: [Int] -> Maybe ([Int], Int)
        popper [] = Nothing
        popper (x:xs) = Just (xs, x)

push :: Int -> Calc ()
push i = C (\xs -> Just (i:xs, ()))

instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of 
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s -> 
      case sf s of 
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> 
      case sa s of 
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a

evaluate :: [Token] -> Calc Int
evaluate [] = return 0 >> pop
evaluate ((Number x):xs) = push x >> evaluate xs
evaluate ((Operator o):xs) = do x <- pop
                                y <- pop
                                push (x `o` y) >> evaluate xs
                                                              
calculate :: String -> Maybe Int
calculate s = deconTuple (deconCalc <$> evaluate <$> tokenise s)
  where deconTuple :: Maybe (Maybe ([Int], Int)) -> Maybe Int
        deconTuple (Just (Just(ls, a))) = Just a
        deconTuple c = Nothing
        deconCalc :: Calc Int -> Maybe ([Int], Int)
        deconCalc s = deconCalcUtil s []
          where deconCalcUtil (C i) = i
