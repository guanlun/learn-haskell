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

path_file :: String -> String
path_file x = path_file_iter x []

path_file_iter :: String -> String -> String
path_file_iter [] buffer = buffer
path_file_iter ('/':xs) buffer = path_file_iter xs []
path_file_iter (x:xs) buffer = path_file_iter xs (buffer ++ [x])

path_dir :: String -> String
path_dir x = path_dir_iter x []

path_dir_iter :: String -> String -> String
path_dir_iter [] buffer = []
path_dir_iter ('/':xs) buffer = buffer ++ "/" ++ path_file_iter xs []
path_dir_iter (x:xs) buffer = path_dir_iter xs (buffer ++ [x])

-- Split `str` using a list of Chars `dils`
split :: [Char] -> String -> [String]
split dils str = go str "" [] where
    go a b c
        | trace (printf "a = %s, b = %s, c = %s" a b (show c)) False = undefined
    go [] buffer results = results ++ [buffer]
    go (x:xs) buffer results
        | x `elem` dils = go xs [] (results ++ [buffer])
        | otherwise = go xs (buffer ++ [x]) results

splitBySpace :: String -> [String]
splitBySpace = split " "

