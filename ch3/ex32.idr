import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = S (my_length xs)

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = reverse xs ++ [x]

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_vec_map : (a -> b) -> Vect n a -> Vect n b
my_vec_map f [] = []
my_vec_map f (x :: xs) = f x :: my_vec_map f xs


