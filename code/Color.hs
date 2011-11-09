data Color = Red | Green | Blue deriving Eq

instance Ord Color where
  Red < Green = True
  Red < Blue = True
  Green < Blue = True

main = do
  print $ Red > Blue
  putStrLn "Ok"
