import Data.List (nub)

inserterFunctor:: Ord list_elem => list_elem -> Int -> [list_elem] -> [list_elem]
inserterFunctor mergingElement insertingPos permutation = before ++ [mergingElement] ++ after
    where
        before = take insertingPos permutation
        after = drop insertingPos permutation

mergeIntoPermutations:: Ord list_elem => [[list_elem]] -> list_elem -> Int -> [[list_elem]]
mergeIntoPermutations allPermutations mergingElement insertingPos = map inserter allPermutations
    where
        inserter = inserterFunctor mergingElement insertingPos

mergeIntoPermutationsRecursive:: Ord list_elem => [[list_elem]] -> list_elem -> Int -> [[list_elem]]
mergeIntoPermutationsRecursive allPermutations mergingElement insertingPos
    | permuatationLength > insertingPos = insertedOnCurrentPos ++ insertedOnNextPoses
    | otherwise = insertedOnCurrentPos
    where
        permuatationLength = length (head allPermutations)
        insertedOnCurrentPos = mergeIntoPermutations allPermutations mergingElement insertingPos
        insertedOnNextPoses = mergeIntoPermutationsRecursive allPermutations mergingElement (insertingPos + 1)

allPermutations:: Int -> [[Int]]
allPermutations 0 = [[]]

allPermutations permutationSize = mergeIntoPermutationsRecursive allSmallerPermutations permutationSize 0
    where
        allSmallerPermutations = allPermutations (permutationSize - 1)

allPermutationsFromGivenListInternal:: Ord list_elem => [list_elem] -> [[list_elem]]
allPermutationsFromGivenListInternal someList
    | permutationSize > 0 = mergeIntoPermutationsRecursive allSmallerPermutations listHead 0
    | otherwise = [[]]
    where
        permutationSize = length someList
        listHead = head someList
        listTail = tail someList
        allSmallerPermutations = allPermutationsFromGivenListInternal listTail


allPermutationsFromGivenList:: Ord list_elem => [list_elem] -> [[list_elem]]
allPermutationsFromGivenList someList = nub (allPermutationsFromGivenListInternal someList)
