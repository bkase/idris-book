import Tree

listToTree : Ord a => List a -> Tree a
listToTree xs = foldl (\acc => \x => insert x acc) Empty xs

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ (val :: treeToList right)

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = (evaluate x) + (evaluate y)
evaluate (Sub x y) = (evaluate x) - (evaluate y)
evaluate (Mul x y) = (evaluate x) * (evaluate y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing o@(Just _) = o
maxMaybe o@(Just x) Nothing = o
maxMaybe (Just x) (Just y) = Just (max x y)

-- See Biggest.idr for biggestTriangle

