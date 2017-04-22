import Data.Vect

makeFlatVect : Vect m (Vect 0 elem)
makeFlatVect = replicate _ []

transposeMat : Vect n (Vect m elem) -> Vect m (Vect n elem)
transposeMat [] = makeFlatVect
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

map : (a -> b) -> Vect n a -> Vect n b
map f [] = []
map f (x :: xs) = f x :: map f xs

addMat : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMat [] [] = []
addMat (x :: xs) (y :: ys) = zipWith (+) x y :: addMat xs ys

multWithNewElem : Num a => (rows : Vect height (Vect (S width) a)) -> (cols : Vect (S height) (Vect width a)) -> (newElem : a) -> Vect (S height) (Vect (S width) a)
multWithNewElem [] (restCol :: restCols) newElem = [newElem :: restCol]
multWithNewElem (restRow :: restRows) (_ :: restCols) newElem =
  let recurse = multWithNewElem restRows restCols newElem in
      restRow :: recurse

multInductive : Num a => (row : Vect m a) -> (col : Vect m a) -> (rows : Vect height (Vect (S width) a)) -> (cols : Vect (S height) (Vect width a)) -> Vect (S height) (Vect (S width) a)
multInductive row col rows cols = let newElem = sum $ zipWith (*) row col in
                                  multWithNewElem rows cols newElem

multHelper : Num a => (xs : Vect n (Vect m a)) -> (ysTrans : Vect p (Vect m a)) -> Vect n (Vect p a)
multHelper [] [] = []
multHelper (x :: xs) [] = makeFlatVect
multHelper [] (y :: ys) = []
multHelper (row :: xs) (col :: ys) = let rest = multHelper xs (col :: ys) in
                                     let rest2 = multHelper (row :: xs) ys in
                                     multInductive row col rest rest2

multMat : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMat xs ys = let ysTrans = transposeMat ys in
                    reverse $ multHelper xs ysTrans

main : IO ()
main = putStrLn $ show $ multMat [[1,2], [3, 4], [5, 6]] [[7,8,9,10], [11,12,13,14]]
