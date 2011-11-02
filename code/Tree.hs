data Tree a = Tip | Branch a (Tree a) (Tree a)

instance Eq (Tree a) where
  Branch x l1 r1 == Branch y l2 r2 = x == y && l1 == l2 && r1 == r2
  Tip == Tip = True
  _ == _  = False

empty :: Tree Int
empty = Tip

leaf :: Int -> Tree Int
leaf x = Branch x Tip Tip

insert :: Int -> Tree Int -> Tree Int
insert x Tip = leaf x
insert x (Branch y l r) | x < y     = Branch y (insert x l) r
                        | otherwise = Branch y l (insert x r)

height :: Tree a -> Int
height Tip = 0
height (Branch _ l r) = 1 + max (height l) (height r)

main :: IO ()
main = do
  let t = insert 5 (insert 25 (insert 15 (insert 20 empty)))
  print $ height t