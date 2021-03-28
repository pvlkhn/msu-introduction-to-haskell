deleteAllSublistEntriesInternal:: (Ord list_elem) => [list_elem] -> [list_elem] -> [list_elem]
deleteAllSublistEntriesInternal [] [list_elem] = []


deleteAllSublistEntriesInternal source_list filtering_list
    | source_len < filtering_len = source_list
    | start_from_filtering = deleteAllSublistEntriesInternal skipped_start filtering_list
    | otherwise = list_head:filtered_tail
    where
        source_len = length source_list
        filtering_len = length filtering_list
        start_part = take filtering_len source_list
        start_from_filtering = start_part == filtering_list
        skipped_start = drop filtering_len source_list
        list_head = head source_list
        list_tail = tail source_list
        filtered_tail = deleteAllSublistEntriesInternal list_tail filtering_list


deleteAllSublistEntries:: (Ord list_elem) => [list_elem] -> [list_elem] -> [list_elem]

deleteAllSublistEntries source_list filtering_list
    | list_changed = source_list
    | otherwise = deleteAllSublistEntries new_list filtering_list
    where
        new_list = deleteAllSublistEntriesInternal source_list filtering_list
        list_changed = new_list == source_list