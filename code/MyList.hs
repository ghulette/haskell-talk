-- Roll your own lists

data MyList a = Nil
              | Cons a (MyList a)

infixr 9 `Cons`

toList :: MyList a -> [a]
toList Nil = []
toList (Cons x xs) = x : (toList xs)

main :: IO ()
main = do
  print $ toList $ 1 `Cons` 2 `Cons` 3 `Cons` Nil
