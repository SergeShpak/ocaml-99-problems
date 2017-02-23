open OUnit2;;

let list_to_string (l: string list) =
  let print_string_symb (str: string) =
    "\"" ^ str ^ "\""
    in
  let rec aux (l: string list) (acc: string) = match l with
      [] -> acc
    | [el] -> acc ^ (print_string_symb el)
    | h :: t -> let expanded_acc = (acc ^ print_string_symb(h)) ^ " ; "
      in aux t expanded_acc
    in
    let wrap_in_brackets (str : string) =
      "[ " ^ str ^ " ]"
    in
    wrap_in_brackets (aux l "")

let find_in_list (el: 'a) (l: 'a list) =
  let rec aux (l: 'a list) (pos: int) =
    match l with
      [] -> -1
    | h :: t -> 
      if h = el then pos
      else aux t (pos + 1) in
  aux l 1

let test_ListsLast_ReturnsLastElement ctx = 
  assert_equal (Some "d") (Lists.last [ "a" ; "b" ; "c" ; "d"])


let test_ListsLast_ReturnsNoneIfListIsEmpty ctx =
  assert_equal None (Lists.last[])


let test_ListsLastTwo_ReturnsLastTwoElements ctx =
  assert_equal (Some ("c", "d")) (Lists.last_two [ "a" ; "b" ; "c" ; "d"])


let test_ListsLastTwo_ReturnsNoneIfListContainsLessThanTwoElements ctx =
  assert_equal None (Lists.last_two ["a"]);
  assert_equal None (Lists.last_two [])


let test_ListsAt_ReturnsElementAtGivenPosition ctx =
  assert_equal (Some "c") (Lists.at 3 [ "a" ; "b"; "c"; "d"; "e" ])


let test_ListsAt_ReturnsNoneIfIndexIsOutOfBound ctx =
  assert_equal None (Lists.at 3 ["a"]);
  assert_equal None (Lists.at 0 ["a"]);
  assert_equal None (Lists.at (-1) ["a"])


let test_ListsAt_ReturnsNoneIfListIsEmpty ctx =
  assert_equal None (Lists.at 1 [])


let test_ListsLength_ReturnsNumberOfElementsInList ctx =
  assert_equal 3 (Lists.length [ "a" ; "b" ; "c"]);
  assert_equal 0 (Lists.length [])


let test_ListsRev_ReturnsReversedList ctx =
  assert_equal [ "a" ; "b" ; "c" ] (Lists.rev [ "c" ; "b" ; "a" ])


let test_ListsRev_SingleElementListStaysSame ctx =
  assert_equal [ "a" ] (Lists.rev [ "a" ])


let test_ListsRev_EmptyListStaysSame ctx =
  assert_equal [] (Lists.rev [])


let test_ListsIsPalindrome_RecognizesPalindrome ctx =
  let palindrome_to_check = ["x" ; "a" ; "m" ; "a" ; "x" ] 
  and not_palindrome_to_check = [ "a" ; "b" ] in
  assert_bool ((list_to_string palindrome_to_check) ^ " is not a palindrome") 
    (Lists.is_palindrome palindrome_to_check) ;
  assert_equal false (Lists.is_palindrome not_palindrome_to_check)


let test_ListsIsPalindrome_EmptyListIsNotPalindrome ctx =
  assert_equal false (Lists.is_palindrome [])


let test_ListsFlatten_FlattensList ctx =
  let list_to_flatten = 
    [ Lists.One "a" ; 
      Lists.Many [ Lists.One "b" ; 
                   Lists.Many [ Lists.One "c" ; Lists.One "d" ] ; 
                   Lists.One "e" 
                 ] 
    ]
  and expected_list = ["a"; "b"; "c"; "d"; "e"]
  in
  let flattened_list = Lists.flatten list_to_flatten
  in 
  assert_equal expected_list flattened_list


let test_ListsCompress_CompressesList ctx =
  let list_to_compress = 
    ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] 
  and expected_list = ["a"; "b"; "c"; "a"; "d"; "e"]
  in
  let compressed_list = Lists.compress list_to_compress in
  assert_equal expected_list compressed_list


let test_ListsPack_PacksList ctx =
  let list_to_pack =
    ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
  and expected_list = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; 
                       ["d"; "d"]; ["e"; "e"; "e"; "e"]]
  in
  let packed_list = Lists.pack list_to_pack in
  assert_equal expected_list packed_list


let test_ListsEncode_PerformsRLEncoding ctx =
  let list_to_encode = 
    ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  and expected_list = 
    [Lists.RLEMany (4, "a"); Lists.RLEOne "b"; Lists.RLEMany(2, "c"); 
    Lists.RLEMany(2, "a"); Lists.RLEOne "d"; Lists.RLEMany (4, "e")]
  in
  let encoded_list = Lists.encode list_to_encode
  in
  assert_equal expected_list encoded_list


let test_ListsEncode_ReturnsEmptyListIfPassedListEmpty ctx =
  assert_equal [] (Lists.encode [])


let test_ListsDecode_PerformsRLDecoding ctx =
  let list_to_decode = 
    [Lists.RLEMany (4, "a"); Lists.RLEOne "b"; Lists.RLEMany(2, "c"); 
     Lists.RLEMany(2, "a"); Lists.RLEOne "d"; Lists.RLEMany (4, "e")]
  in
  let expected_list = 
    ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  in
  let decoded_list = Lists.decode list_to_decode
  in
  assert_equal expected_list decoded_list


let test_ListsDuplicate_DuplicatesCorrectly ctx =
  let list_to_duplicate = ["a";"b";"c";"c";"d"]
  in
  let expected_list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
  in
  let result = Lists.duplicate list_to_duplicate
  in
  assert_equal expected_list result


let test_ListsReplicate_ReplicatesCorrectly ctx =
  let list_to_replicate = ["a";"b";"c"]
  in
  let times = 3
  in
  let expected_list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
  in
  let result = Lists.replicate list_to_replicate times
  in
  assert_equal expected_list result

let test_ListsReplicate_ZeroTimesYieldsEmptyList ctx =
  let list_to_replicate = ["a";"b";"c"]
  in
  assert_equal [] (Lists.replicate list_to_replicate 0)


let test_ListsDrop_DropsCorrectElement ctx =
  let list_to_alter = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] and
  index = 3 and
  expected_list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] in
  let result = Lists.drop list_to_alter index in
  assert_equal expected_list result


let test_ListsDrop_IfNegIndexReturnsSameList ctx =
  let list_to_alter = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] and
  index = -1 and
  expected_list = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] in
  let result = Lists.drop list_to_alter index in
  assert_equal expected_list result


let test_ListsDrop_IfIndexIsBiggerThanLengthReturnsSameList ctx =
  let list_to_alter = ["a" ; "b" ; "c"] and
  index = 4 and
  expected_list = ["a" ; "b" ; "c"] in 
  let result = Lists.drop list_to_alter index in
  assert_equal expected_list result


let test_ListsSplit_SplitsNormally ctx =
  let list_to_split = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] and
  first_part_length = 3 and
  expected_result = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) in
  let result = Lists.split list_to_split first_part_length in
  assert_equal expected_result result


let test_ListsSplit_IfLengthBiggerReturnsFullEmpty ctx =
  let list_to_split = ["a";"b";"c"] and
  first_part_length = 4 and
  expected_result = (["a";"b";"c"], []) in
  let result = Lists.split list_to_split first_part_length in
  assert_equal expected_result result


let test_ListsSplit_IfLengthNegativeReturnsEmptyFull ctx = 
  let list_to_split = ["a";"b";"c"] and
  first_part_length = -1 and
  expected_result = ([], ["a";"b";"c"]) in
  let result = Lists.split list_to_split first_part_length in
  assert_equal expected_result result


let test_ListsSplit_IfLengthZeroReturnsEmptyFull ctx = 
  let list_to_split = ["a";"b";"c"] and
  first_part_length = 0 and
  expected_result = ([], ["a";"b";"c"]) in
  let result = Lists.split list_to_split first_part_length in
  assert_equal expected_result result


let test_ListsSlice_SlicesNormally ctx =
  let list_to_slice = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] and
  start_ind = 2 and end_ind = 6 and
  expected_result = ["c"; "d"; "e"; "f"; "g"] in
  let result = Lists.slice list_to_slice start_ind end_ind in
  assert_equal expected_result result

let test_ListsSlice_IfStartEndEqualReturnsSingleEl ctx =
  let list_to_slice = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] and
  start_ind = 6 and end_ind = 6 and
  expected_result = ["g"] in
  let result = Lists.slice list_to_slice start_ind end_ind in
  assert_equal expected_result result

let test_ListsSlice_IfStartGreaterEndSwaps ctx =
  let list_to_slice = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] and
  start_ind = 6 and end_ind = 2 and
  expected_result = ["c"; "d"; "e"; "f"; "g"] in
  let result = Lists.slice list_to_slice start_ind end_ind in
  assert_equal expected_result result

let test_ListsSlice_IfNegIndexIsDifferenceFromRear ctx =
  let list_to_slice = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] and
  start_ind = -1 and end_ind = -4 and
  expected_result = ["g";"h";"i";"j"] in
  let result = Lists.slice  list_to_slice start_ind end_ind in
  assert_equal expected_result result

let test_ListsSlice_IfEndOutOfBoundReturnsEndOfList ctx =
  let list_to_slice = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] and
  start_ind = 7 and end_ind = 12 and
  expected_result = ["h";"i";"j"] in
  let result = Lists.slice  list_to_slice start_ind end_ind in
  assert_equal expected_result result
    
let test_ListsSlice_GettingASliceOutOfBoundYieldsEmpty ctx =
  let list_to_slice = ["a";"b";"c"] and
  first_start_ind = -6 and first_end_ind = -5 and
  first_expected_result = [] and
  second_start_ind = 4 and second_end_ind = 7 and
  second_expected_result = [] in
  let first_result = Lists.slice list_to_slice first_start_ind first_end_ind 
  and second_result = Lists.slice list_to_slice second_start_ind second_end_ind
  in
  assert_equal first_expected_result first_result ;
  assert_equal second_expected_result second_result


let test_ListsRotate_RotatesCorrectly ctx =
  let list_to_rotate = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] and
  first_rot_ind = 3 and second_rot_ind = 7 and
  first_expected_result = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] and
  second_expected_result =  ["h"; "a"; "b"; "c"; "d"; "e"; "f"; "g"] in
  let first_result = Lists.rotate list_to_rotate first_rot_ind and
  second_result = Lists.rotate list_to_rotate second_rot_ind in
  assert_equal first_expected_result first_result ;
  assert_equal second_expected_result second_result

let test_ListsRotate_RotatesInOtherDirectionWhenNegIndex ctx =
  let list_to_rotate = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] and
  rot_ind = (-2) and big_rot_ind = (-10) and
  expected_result = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] in
  let result = Lists.rotate list_to_rotate rot_ind and
  second_result = Lists.rotate list_to_rotate big_rot_ind in
  assert_equal expected_result result ;
  assert_equal expected_result second_result

let test_ListsRotate_IfIndexZeroReturnsOriginal ctx =
  let list_to_rotate = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] and
  rot_ind = 0 and
  expected_result =  ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] in
  let result = Lists.rotate list_to_rotate rot_ind in
  assert_equal expected_result result


let test_ListsRemoveAt_RemovesCorrectly ctx =
  let list_to_alter = ["a";"b";"c";"d"] and
  ind = 1 in
  let expected_result = ["a";"c";"d"] and
  result = Lists.remove_at ind list_to_alter in
  assert_equal expected_result result

let test_ListsRemoveAt_CountsFromRearIfNegativePos ctx =
  let list_to_alter = ["a";"b";"c";"d"] and
  ind = -2 and
  expected_result = ["a";"b";"d"] in
  let result = Lists.remove_at ind list_to_alter in
  assert_equal expected_result result

let test_ListsRemoveAt_ReturnsOriginalIfIndexOutOfBound ctx =
  let list_to_alter = ["a";"b";"c";"d"] and
  neg_ind = -5 and pos_ind = 4 in
  let clos ind = Lists.remove_at ind list_to_alter in
  let first_result = clos neg_ind and
  second_result = clos pos_ind in
  assert_equal list_to_alter first_result ;
  assert_equal list_to_alter second_result


let test_ListsInsertAt_InsertsCorrectly ctx = 
  let list_to_alter = ["a";"b";"c";"d"] and
  ind = 2 and el = "x" and
  expected_result = ["a";"b";"x";"c";"d"] in
  let result = Lists.insert_at el ind list_to_alter in
  assert_equal expected_result result

let test_ListsInsertAt_LookForNegativeIndexFromRear ctx =
  let list_to_alter = ["a";"b";"c";"d"] and
  el = "x" and first_ind = -1 and 
  first_expected_result = ["a";"b";"c";"d";"x"] and 
  second_ind = -3 and second_expected_result = ["a";"b";"x";"c";"d"] and
  third_ind = -5 and third_expected_result =  ["x";"a";"b";"c";"d"] in
  let clos ind = Lists.insert_at el ind list_to_alter in
  let first_result = clos first_ind and
  second_result = clos second_ind and
  third_result = clos third_ind in 
  assert_equal first_expected_result first_result ;
  assert_equal second_expected_result second_result ;
  assert_equal third_expected_result third_result

let test_ListsInsertAt_ReturnOriginalIfIndexOutOfBound ctx =
  let list_to_alter = ["a";"b";"c";"d"] and
  el = "x" and first_ind = 5 and second_ind = -6 in
  let clos ind = Lists.insert_at el ind list_to_alter in
  let first_result = clos first_ind and
  second_result = clos second_ind in
  assert_equal list_to_alter first_result ;
  assert_equal list_to_alter second_result

let test_ListsInsertAt_InsertsAtTheEndOfTheList ctx =
  let list_to_alter = ["a";"b";"c";"d"] and
  ind = 4 and el = "x" and
  expected_result = ["a";"b";"c";"d";"x"] in
  let result = Lists.insert_at el ind list_to_alter in
  assert_equal expected_result result


let test_ListsRange_CreatesCorrectRange ctx =
  let first_start = 4 and first_end = 9 and second_start = -2 and 
  second_end = 2 and
  first_expected_result = [4;5;6;7;8;9] and
  second_expected_result = [-2;-1;0;1;2] in
  let first_result = Lists.range first_start first_end and 
  second_result = Lists.range second_start second_end in
  assert_equal first_expected_result first_result ;
  assert_equal second_expected_result second_result

let test_ListsRange_IfStartGreaterThanEndCreatesReversedList ctx =
  let first_start = 9 and first_end = 4 and second_start = 2 and 
  second_end = -2 and
  first_expected_result = [9;8;7;6;5;4] and
  second_expected_result = [2;1;0;-1;-2] in
  let first_result = Lists.range first_start first_end and 
  second_result = Lists.range second_start second_end in
  assert_equal first_expected_result first_result ;
  assert_equal second_expected_result second_result 

let test_ListsRange_IfStartEqualsEndReturnsListWithSingleElement ctx =
  let s = 4 and e = 4 and
  expected_result = [4] in
  let result = Lists.range s e in
  assert_equal expected_result result


let test_ListsRandSelect_SelectsCorrectNumberOfElements ctx = 
  let target_array = ["a";"b";"c";"d";"e";"f";"g";"h"] and 
  first_number_of_elements = 3 and second_number_of_elements = 8 in
  let clos num = Lists.rand_select target_array num in
  let first_result = clos first_number_of_elements and 
  second_result = clos second_number_of_elements in
  assert_equal first_number_of_elements (List.length first_result) ;
  assert_equal second_number_of_elements (List.length second_result)

let test_ListsRandSelect_IfNumberOfElementsIsZeroReturnsEmpty ctx =
  let target_array = ["a";"b"] and number_of_elements = 0 in
  let result = Lists.rand_select target_array number_of_elements in
  assert_equal number_of_elements (List.length result)

let test_ListsRandSelect_IfNumberOfElementsIsNegativeReturnsEmpty ctx =
  let target_array = ["a";"b"] and number_of_elements = -1 and
  expected_number_of_elements = 0 in
  let result = Lists.rand_select target_array number_of_elements in
  assert_equal expected_number_of_elements (List.length result)

let test_ListsRandSelect_IfNumberIsOfutOfBoundReturnsListOfOriginalLength ctx =
  let target_array = ["a";"b";"c"] and number_of_elements = 4 and
  expected_number_of_elements = 3 in
  let result = Lists.rand_select target_array number_of_elements in
  assert_equal expected_number_of_elements (List.length result)

let test_ListsRandSelect_IfLengthsEqualReturnsPermutation ctx =
  let target_array = ["a";"b";"c"] and number_of_elements = 3 in
  let result = Lists.rand_select target_array number_of_elements in
  let sorted_result = List.sort compare result in
  assert_equal sorted_result target_array 


let test_LottoSelect_SelectsCorrectNumberOfElements ctx =
  let target_number_of_els = 6 and boundary = 49 in
  let result = Lists.lotto_select target_number_of_els boundary in
  let result_len = List.length result in
  assert_equal result_len target_number_of_els

let test_LottoSelect_SelectedElementsAreGreaterThanZero ctx = 
  let target_number_of_els = 40 and boundary = 3 in
  let result = Lists.lotto_select target_number_of_els boundary in
  let rec is_all_gt_zero (l: int list) =
    match l with
      [] -> true
    | h::t -> 
      if h < 1 then false
      else is_all_gt_zero t
  in
  let is_gt_zero = is_all_gt_zero result in
  assert_bool "Some elements are smaller than zero" is_gt_zero

let test_LottoSelect_SelectedElementsAreLEThanBoundary ctx = 
  let target_number_of_els = 40 and boundary = 3 in
  let result = Lists.lotto_select target_number_of_els boundary in
  let rec is_all_le_boundary (l: int list) =
    match l with
      [] -> true
    | h::t ->
      if h > boundary then false
      else is_all_le_boundary t
  in
  let is_le_boundary = is_all_le_boundary result in
  assert_bool "Some elements are great than the boundary" is_le_boundary

let test_LottoSelect_IfNumberOfElementsIsLEZeroReturnsEmpty ctx =
  let first_target_number_of_els = 0 and second_target_number_of_els = -4 and
  boundary = 10 and expected_result = [] in
  let first_result = Lists.lotto_select first_target_number_of_els boundary and
    second_result = Lists.lotto_select second_target_number_of_els boundary in
  assert_equal expected_result first_result ;
  assert_equal expected_result second_result

let test_LottoSelect_IfBoundaryIsLEZeroReturnsEmpty ctx =
  let number_of_elements = 10 and first_boundary = 0 and 
  second_boundary = -4 and expected_result = [] in
  let first_result = Lists.lotto_select number_of_elements first_boundary and
    second_result = Lists.lotto_select number_of_elements second_boundary in
  assert_equal expected_result first_result ;
  assert_equal expected_result second_result


let test_Permutation_ReturnsListWithSameElements ctx =
  let target_list = ["a"; "b"; "c"; "d"; "e"; "f"] in
  let result = Lists.permutation target_list in
  let sorted_result = List.sort compare result in
  assert_equal target_list sorted_result


let suite = 
  "suite">:::
  ["Lists.last: Returns last element">:: test_ListsLast_ReturnsLastElement ;
    "Lists.last: Returns 'None' if list is empty">:: 
        test_ListsLast_ReturnsNoneIfListIsEmpty ;
    "Lists.last_two: Returns last two elements">:: 
        test_ListsLastTwo_ReturnsLastTwoElements ;
    "Lists.last_two: Returns None if list contains less than two elements">::
        test_ListsLastTwo_ReturnsNoneIfListContainsLessThanTwoElements ;
    "Lists.at: Return element at given position">::
        test_ListsAt_ReturnsElementAtGivenPosition ;
    "Lists.at: Returns None if given index is out of bound">::
        test_ListsAt_ReturnsNoneIfIndexIsOutOfBound ;
    "Lists.at: Returns None if list is empty">:: 
        test_ListsAt_ReturnsNoneIfListIsEmpty ;
    "Lists.length: Return number of elements in list">::
        test_ListsLength_ReturnsNumberOfElementsInList ;
    "Lists.rev: Returns reversed list">::
        test_ListsRev_ReturnsReversedList ;
    "Lists.rev: List that consist of a single element stays the same">::
        test_ListsRev_SingleElementListStaysSame ;
    "Lists.rev: Empty list stays the same">::
        test_ListsRev_EmptyListStaysSame ;
    "Lists.is_palindrome: Recognizes a palindrome">::
        test_ListsIsPalindrome_RecognizesPalindrome ;
    "Lists.is_palindrome: Empty list is not a palindrome">::
        test_ListsIsPalindrome_EmptyListIsNotPalindrome ;
    "Lists.flatten: Flattens list">::
        test_ListsFlatten_FlattensList ;
    "Lists.compress: Compresses list">::
        test_ListsCompress_CompressesList ;
    "Lists.pack: Packs list">::
        test_ListsPack_PacksList ;
    "Lists.encode: Performs a run-length encoding of a list">::
        test_ListsEncode_PerformsRLEncoding;
    "Lists.encode: Returns empty list if passed list is empty">::
        test_ListsEncode_ReturnsEmptyListIfPassedListEmpty;
    "Lists.decode: Performs run-length decoding of a list">::
        test_ListsDecode_PerformsRLDecoding;
    "Lists.duplicate: Duplicates list">::
        test_ListsDuplicate_DuplicatesCorrectly;
    "Lists.replicate: Replicates list">::
        test_ListsReplicate_ReplicatesCorrectly;
    "Lists.replicate: If list is replicated zero times, an empty list is" 
    ^ "returned">::
        test_ListsReplicate_ZeroTimesYieldsEmptyList;
    "Lists.drop: Drops correct element">::
        test_ListsDrop_DropsCorrectElement;
    "Lists.drop: If index is negative, returns a duplicated list">::
        test_ListsDrop_IfNegIndexReturnsSameList;
    "Lists.drop: If index is out of bound, returns the same list">::
        test_ListsDrop_IfIndexIsBiggerThanLengthReturnsSameList;
    "Lists.split: Splits normally">::
        test_ListsSplit_SplitsNormally;
    "Lists.split: If first part length is bigger than the length of list, "
    ^ "returns the same and an empty list">::
        test_ListsSplit_IfLengthBiggerReturnsFullEmpty;
    "Lists.split: If first part length is negative, returns an empty and the "
    ^ "same list">::
        test_ListsSplit_IfLengthNegativeReturnsEmptyFull;
    "Lists.split: If first part length is zero, returns an empty and the "
    ^ "same list">::
        test_ListsSplit_IfLengthZeroReturnsEmptyFull;
    "Lists.slice: Slices normally">::
        test_ListsSlice_SlicesNormally;
    "Lists.slice: If indices are equal returns a single element list with the "
    ^ "single element A[start_ind]">::
        test_ListsSlice_IfStartEndEqualReturnsSingleEl;
    "Lists.slice: If start is bigger than end swaps indices and splices "
    ^ "normally">::
        test_ListsSlice_IfStartGreaterEndSwaps;
    "Lists.slice: If an index is negative, it is considered to be equal to "
    ^ "the difference between the list length and the absolute value of the "
    ^ "index">::
        test_ListsSlice_IfNegIndexIsDifferenceFromRear;
    "Lists.slice: If an end index is out of bound, returns "
    ^ "A[start_ind]..A[length - 1]">::
        test_ListsSlice_IfEndOutOfBoundReturnsEndOfList;
    "Lists.slice: If indices are out of bound, returs empty list">::
        test_ListsSlice_GettingASliceOutOfBoundYieldsEmpty;
    "Lists.rotate: Rotate normally">::
        test_ListsRotate_RotatesCorrectly;
    "Lists.rotate: If rotation index is negative, rotates in a different "
    ^ "direction">::
        test_ListsRotate_RotatesInOtherDirectionWhenNegIndex;
    "Lists.rotate: If rotation index is zero, returns the original list">::
        test_ListsRotate_IfIndexZeroReturnsOriginal;
    "Lists.remove_at: Removes correctly">::
        test_ListsRemoveAt_RemovesCorrectly;
    "Lists.remove_at: If negative position is passed, element is searched "
    ^ "from rear">::
        test_ListsRemoveAt_CountsFromRearIfNegativePos;
    "Lists.remove_at: If index is out of bound, returns original list">::
        test_ListsRemoveAt_ReturnsOriginalIfIndexOutOfBound;
    "Lists.insert_at: Inserts correctly">::
        test_ListsInsertAt_InsertsCorrectly ;
    "Lists.insert_at: If index is negative, counts index from the rear of the "
    ^ "list">::
        test_ListsInsertAt_LookForNegativeIndexFromRear ;
    "Lists.insert_at: If index is out of bound, returns a new list indentical "
    ^ "to the original">::
        test_ListsInsertAt_ReturnOriginalIfIndexOutOfBound ;
    "Lists.insert_at: If index is equal to (-1), appends the element to the "
    ^ "rear of the list">::
        test_ListsInsertAt_InsertsAtTheEndOfTheList ;
    "Lists.range: Creates range normally">::
        test_ListsRange_CreatesCorrectRange ;
    "Lists.range: If start index is greater than the end index, creates a "
    ^ "reversed list">::
        test_ListsRange_IfStartGreaterThanEndCreatesReversedList ;
    "Lists.range: If start is equal to end creates a list with a single "
    ^ "element, equal to start">::
        test_ListsRange_IfStartEqualsEndReturnsListWithSingleElement ;
    "Lists.rand_select: Select a correct number of elements">::
        test_ListsRandSelect_SelectsCorrectNumberOfElements ;
    "Lists.rand_select: If number of elements to select is 0, returns "
    ^ "an empty array">::
        test_ListsRandSelect_IfNumberOfElementsIsZeroReturnsEmpty ;
    "Lists.rand_select: If number of elements to select is negative, returns "
    ^ "an empty array">::
        test_ListsRandSelect_IfNumberOfElementsIsNegativeReturnsEmpty ;
    "Lists.rand_select: If number of elements to select is bigger than the "
    ^ "length of the array, returns an array of the same length as the "
    ^ "original one">::
        test_ListsRandSelect_IfNumberIsOfutOfBoundReturnsListOfOriginalLength ;
    "Lists.rand_select: If number of elements to select is equal to the "
    ^ "length of the original array, returns a permutation">::
        test_ListsRandSelect_IfLengthsEqualReturnsPermutation ;
    "Lists.lotto_select: Selects correct number of elements">::
        test_LottoSelect_SelectsCorrectNumberOfElements ;
    "Lists.lotto: all selected elements are greater than zero">::
        test_LottoSelect_SelectedElementsAreGreaterThanZero ;
    "Lists.lotto_select: all selected elements are less than the boundary">:: 
        test_LottoSelect_SelectedElementsAreLEThanBoundary ;
    "Lists.lotto_select: If number of elements to select is less than one, "
    ^ "returns an empty list">::
        test_LottoSelect_IfNumberOfElementsIsLEZeroReturnsEmpty ;
    "Lists.lotto_select: If boundary is less than one, returns an empty" 
    ^ "list">::
        test_LottoSelect_IfBoundaryIsLEZeroReturnsEmpty ;
    "Lists.permutation: Permutates list correctly">::
        test_Permutation_ReturnsListWithSameElements ;
    ]
;;

let () =
  run_test_tt_main suite
;;
