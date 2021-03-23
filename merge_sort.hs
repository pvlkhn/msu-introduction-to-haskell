
splitPart:: (Ord list_elem, Integral part_size) => [list_elem] -> part_size -> ([list_elem], [list_elem])

splitPart [] part_size = ([], [])
splitPart (list_head:list_tail) 1 = ([list_head], list_tail)

splitPart (list_head:list_tail) part_size = (first_part_with_head, second_part)
    where
        new_part_size = part_size - 1
        (first_part, second_part) = splitPart list_tail new_part_size
        first_part_with_head = (list_head:first_part)


mergeSortedLists:: Ord list_elem => [list_elem] -> [list_elem] -> [list_elem]

mergeSortedLists some_list [] = some_list
mergeSortedLists [] some_list = some_list

mergeSortedLists (left_head:left_tail) (right_head:right_tail)
    | left_head < right_head = left_head:new_left
    | otherwise = right_head:new_right
    where
        old_left = left_head:left_tail
        new_right = (mergeSortedLists old_left right_tail)
        old_right = right_head:right_tail
        new_left = (mergeSortedLists left_tail old_right)


mergeSort4:: Ord list_elem => [list_elem] -> [list_elem]
mergeSort4 [] = []

mergeSort4 some_list
    | source_len > 1 = merged_parts
    | otherwise = some_list
    where
        source_len = length some_list
        part1_size = source_len `div` 2
        part2_size = source_len - part1_size
        (part1, part2) = splitPart some_list part1_size

        part11_size = part1_size `div` 2
        (part11, part12) = splitPart part1 part11_size
        sorted_part11 = mergeSort part11
        sorted_part12 = mergeSort part12
        sorted_part1 = mergeSortedLists sorted_part11 sorted_part12

        part21_size = part2_size `div` 2
        (part21, part22) = splitPart part2 part21_size
        sorted_part21 = mergeSort part21
        sorted_part22 = mergeSort part22
        sorted_part2 = mergeSortedLists sorted_part21 sorted_part22

        merged_parts = mergeSortedLists sorted_part1 sorted_part2

