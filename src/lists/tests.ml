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
  assert_bool ((list_to_string palindrome_to_check) ^ " is not palindrome") 
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
  ]
;;

let () =
  run_test_tt_main suite
;;
