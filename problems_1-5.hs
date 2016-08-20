{-
Problem 1: Find the last element of a list.

Examples:
- myLast [1,2,3,4] -> 4
- myLast ['x','y','z'] -> 'z'
-}

myLast :: [a] -> a
myLast [] = error "Can't get last element of an empty list."
myLast [x] = x
myLast (x:xs) = myLast xs

{-
Problem 2: Find the last but one element of a list.

Examples:
- myButLast [1,2,3,4] -> 3
- myButLast ['a'..'z'] -> 'y'
-}

myButLast :: [a] -> a
myButLast [] = error "Can't get but last of an empty list."
myButLast [_] = error "Can't get but last of an a single item list."
myButLast [x, _] = x
myButLast (x:xs) = myButLast xs

{-
Problem 3: Find the K'th element of a list (note: the first element in the list is number 1).

Examples:
- elementAt [1,2,3] 2 -> 2
- elementAt "haskell" 5 -> 'e'
-}

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Invalid index."
elementAt (x:xs) i
    | i == 1 = x
    | otherwise = elementAt xs (i - 1) 

{-
Problem 4: Find the number of elements of a list.

Examples:
- myLength [123, 456, 789] -> 3
- myLength "Hello, world!" -> 13
-}

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

{-
Problem 5: Reverse a list.

Examples:
- myReverse "A man, a plan, a canal, panama!" -> "!amanap ,lanac a ,nalp a ,nam A"
- myReverse [1,2,3,4] -> [4,3,2,1]
-}

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
