module Main

palindrome : Nat -> String -> Bool
palindrome minLen str = (length str) > minLen && lowered == reverse lowered
  where
    lowered : String
    lowered = toLower str

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten xs = take 10 $ reverse $ sort xs

over_length : Nat -> List String -> Nat
over_length maxLen strs = length $ filter (\s => length s > maxLen) strs

main : IO ()
main = repl "Enter a string:" $ show . (palindrome 5)

