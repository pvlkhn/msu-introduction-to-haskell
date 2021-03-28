getRequiredLenSublistsInternal:: (Ord list_elem) => [list_elem] -> Int -> [[list_elem]]

getRequiredLenSublistsInternal some_list required_len
    | list_len > required_len = [requested_sublist] ++ requested_tail_sublists
    | otherwise = [requested_sublist]
    where
        list_len = length some_list
        list_tail = tail some_list
        requested_sublist = take required_len some_list
        requested_tail_sublists = getRequiredLenSublistsInternal list_tail required_len


getAllSublistsInternal:: (Ord list_elem) => [list_elem] -> Int -> [[list_elem]]
getAllSublistsInternal some_list 0 = [[]] ++ getAllSublistsInternal some_list 1

getAllSublistsInternal some_list min_len
    | list_len > min_len = all_requested_len_sublists ++ more_len_sublists
    | otherwise = [some_list]
    where
        list_len = length some_list
        all_requested_len_sublists = getRequiredLenSublistsInternal some_list min_len
        more_len_sublists = getAllSublistsInternal some_list (min_len + 1)


getAllSublists:: (Ord list_elem) => [list_elem] -> [[list_elem]]
getAllSublists some_list = getAllSublistsInternal some_list 0