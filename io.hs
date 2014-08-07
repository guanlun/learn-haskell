breakIntoLines :: String -> [String]
breakIntoLines [] = []
breakIntoLines s = a : breakIntoLines (removeFirst '\n' b)
	where (a, b) = span (/= '\n') s

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst c cs@(c2:rest)
  | c == c2                    = rest
  | otherwise 					= cs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
	where
		lesser = filter (< p) xs
		greater = filter (>= p) xs

linkStrings :: [String] -> String
linkStrings sa = foldl (\ x xs -> x ++ xs) [] sa

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains list el = if el == head list then True else contains (tail list) el

duplicates :: Eq a => [a] -> [a] -> [a]
duplicates [] _ = []
duplicates _ [] = []
duplicates list1 (x:xs) = if (isInList1 x) then x : (duplicates list1 xs) else (duplicates list1 xs)
	where isInList1 x = contains list1 x

