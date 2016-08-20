{-
Problem 6: Find out whether a list is a palindrome
Note: A palindrome can be read forward or backward; e.g. (x a m a x).

Examples:
- isPalindrome [1,2,3] -> False
- isPalindrome "madamimadam" -> True
- isPalindrome [1,2,4,8,16,8,4,2,1] -> True
-}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list
    | length xs < length ys = xs == reverse (tail ys)
    | otherwise = xs == (reverse ys)
    where (xs, ys) = splitAt ((length list) `div` 2) list