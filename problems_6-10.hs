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

{-
Problem 7: Flatten a nested list structure.
Note: Transform a list, possibly holding lists as elements into a `flat'
      list by replacing each list with its elements (recursively).

Examples:
- flatten (Elem 5) -> [5]
- flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) -> [1,2,3,4,5]
- flatten (List []) -> []
-}

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

{-
Problem 8: Eliminate consecutive duplicates of list elements.
Note: If a list contains repeated elements they should be replaced with a
      single copy of the element. The order of the elements should not be
      changed.

Example:
- compress "aaaabccaadeeee" -> "abcade"
-}

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == head xs = compress xs
    | otherwise = x : compress xs

{-
Problem 9: Pack consecutive duplicates of list elements into sublists.
Note: If a list contains repeated elements they should be placed in separate
      sublists.

Example:
- "aaaabccaadeeee" -> ["aaaa","b","cc","aa","d","eeee"]
-}

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack xs =
    let sublist = same xs
    in [sublist] ++ pack (drop (length sublist) xs)
    where same all@(x:xs) = takeWhile (== x) all
