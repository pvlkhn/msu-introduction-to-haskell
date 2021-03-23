import Data.Set (Set, insert, empty)

delDuplicatesInternal:: Ord list_elem => [list_elem] -> Set list_elem -> [list_elem]

delDuplicatesInternal [] alreadyFound = []

delDuplicatesInternal (list_head:list_tail) alreadyFound
    | wasAlreadyFound = delDuplicatesInternal list_tail alreadyFound
    | otherwise = list_head : delDuplicatesInternal list_tail newAlreadyFound
    where
        wasAlreadyFound = list_head `elem` alreadyFound
        newAlreadyFound = list_head `insert` alreadyFound


delDuplicates:: Ord list_elem => [list_elem] -> [list_elem]

delDuplicates [] = []

delDuplicates some_list = delDuplicatesInternal some_list emptySet
    where
        emptySet = Data.Set.empty

