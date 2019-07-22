s :: Monad m => [m a] -> m [a]
s [] = return []
s (a:as) = do
  a
  s as
  pure (a : as)
