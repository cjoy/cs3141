module Ex02 where
import Test.QuickCheck
import Data.List
-- implement the following functions, which meet some (but not all!) of the 
-- properties of a correct sorting function

-- prop2 & 4, but not prop1 & 3 & 5
dodgySort1 :: [Int] -> [Int]
dodgySort1 xs = xs

-- prop1 & 2 & 3, but not prop4 & 5
dodgySort2 :: [Int] -> [Int]
dodgySort2 xs = sort (single_max_element_list xs ++ xs)

-- prop1 & 3 & 4, but not prop2 & 5
dodgySort3 :: [Int] -> [Int]
dodgySort3 xs = sort (decrement_max_element_list xs)


-- prop1 & 2 & 3 & 4, but not prop5
dodgySort4 :: [Int] -> [Int]
dodgySort4 xs = sort (mess_with_dup_element xs)


-- Properties of sorting function    
-- reversing the input never changes the output
sortProp1 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp1 sortFn xs = sortFn xs == sortFn (reverse xs)

-- every element in the original list must be inside the list returned by sortFn
sortProp2 :: ([Int] -> [Int]) -> Int -> [Int] -> [Int] -> Bool
sortProp2 sortFn x xs ys = x `elem` sortFn (xs ++ [x] ++ ys)

-- is always in non-descending order
sortProp3 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp3 sortFn xs = isSorted (sortFn xs)
  where 
    isSorted (x1 : x2 : xs) = (x1 <= x2) && isSorted (x2 : xs)
    isSorted _ = True

-- original list and sorted list should have same length
sortProp4 :: ([Int] -> [Int]) -> [Int] -> Bool    
sortProp4 sortFn xs = length xs == length (sortFn xs)

-- output the same as insertion sort
sortProp5 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp5 sortFn xs 
  = sortFn xs == insertionSort xs

insertionSort :: [Int] -> [Int]
insertionSort xs = foldr insertSorted [] xs
  where 
    insertSorted x [] = [x]
    insertSorted x (y : ys) 
      | x <= y = x : y : ys
      | otherwise = y : insertSorted x ys

-- Helper functions 

single_max_element_list :: [Int] -> [Int]
single_max_element_list [] = []
single_max_element_list xs = [maximum xs]

decrement_max_element_list :: [Int] -> [Int]
decrement_max_element_list [] = []
decrement_max_element_list xs =  [(maximum xs)-1] ++ (delete (maximum xs) xs)

mess_with_dup_element :: [Int] -> [Int]
mess_with_dup_element [] = []
mess_with_dup_element [x] = [x]
mess_with_dup_element xs = (replicate remaining s) ++ (nub xs)
  where
    l = (length xs)
    nl = length (nub xs)
    remaining = l - nl
    s = maximum xs