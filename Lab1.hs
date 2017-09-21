{-main program, run with a list and an int
error handling was the hardest part-}
smallestksets [] _ = do
 error "error empty list"
smallestksets xs k = do
 putStrLn "Sum  Pos1 Pos2 Sublist  " 
 putStrLn.unlines.map convertstr$ (ksmallesthelper xs k)

{-converts my tuple to char-}
convertstr :: (Int, Int, Int, [Int]) -> [Char]
convertstr (x, i, j, y) = show x++"  "  ++ show i ++"   " ++ show j++"   " ++ show y++" "


{-Bool solution discussed with claes andersson.
works like a tree, if you go left in a recursion you can not go right-}
sublists :: [Int] -> Bool -> [[Int]]
sublists (x:[]) _ = [[x]]
sublists xs canIright 
 | canIright == True = [xs] ++ sublists(init xs) False ++ sublists (tail xs) True
 | otherwise = [xs] ++ sublists(init xs) False

{-Solutions discussed with Dennis Persson
checking the len of the OG list and traverses backwards
when len = 1 we go up 1 on i and return to the OG len for j-} 
indexing :: [[Int]] -> Int -> Int -> Int ->[(Int, Int, Int, [Int])]
indexing [] _ _ _ = []
indexing (x:xs) i j k
 | 1 < (length (x)) = [(sum x, i, j, x)] ++ indexing (xs) i (j-1) k
 | otherwise = [(sum x, i, j, x)] ++ indexing (xs) (i+1) k k

{-inits the run of list, index and sum and returns a list of tuples-}
fullrun :: [Int] -> [(Int, Int, Int, [Int])]
fullrun (x:xs) = indexing (sublists (x:xs) True) 1 (length (x:xs)) (length(x:xs))

{-takes the k smallest from the sorted list, aka the k first-}
ksmallesthelper :: [Int] -> Int -> [(Int, Int, Int, [Int])] 
ksmallesthelper xs k = take k (quicksort(fullrun xs))

{-quicksort example inspired from haskell doc
normal quicksort that sorts my lists with tuples on the first element (the sum)
bigger than head goes back rest goes infront, recurse until sorted-}
quicksort :: (Ord a) => [(a)] -> [(a)]
quicksort [] = []
quicksort (x:xs) = (quicksort firsthalf) ++ [x] ++ (quicksort secondhalf)
 where
  firsthalf = [a | a <- xs, a <= x]
  secondhalf = [a | a <- xs, a > x]







--main :: IO ()
--main xs k = putStrLn.unlines.map convertstr$(ksmallesthelper [1,2,3] 6)




--filtersublists :: [Int] -> [[Int]]
--filtersublists xs = filter (/=[]) (sublists xs)
{-i start j end k sum
positions :: [[Int]] ->Int -> Int -> Int -> [([Int], Int, Int, Int)]
positions (x:xs) i j k
 | null xs = [(x, i, j, sum x)]
 | 
-}

--indexes :: [[Int]] -> Int -> Int ->([[Int]], Int, Int, Int)
--indexes (x:xs) i j = (take i (x:xs) ++ drop j (x:xs),  i,  j, sum [1,2,3] )

--sumsublists :: [[Int]] -> [Int]
--sumsublists [] = []
--sumsublists (x:xs) = sum x : sumsublists xs

--everything :: [a] -> [[Int]]
--everything xs = sumsublists filtersublists sublists 
--sublists :: [a] -> [[a]]
--sublists [] = []
--sublists (x:xs) = sublists()

--something :: [Int] -> [[Int]]
--something [] = [[]]
--something (x:xs) = [x:xs | x <- something]


