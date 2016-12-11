open OUnit2;;

let test_ListsLast_ReturnsLastElement ctx = 
  assert_equal (Some "d") (Lists.last [ "a" ; "b" ; "c" ; "d"])
;;

let test_ListsLast_ReturnsNoneIfListIsEmpty ctx =
  assert_equal None (Lists.last[])
;;

let test_ListsLastTwo_ReturnsLastTwoElements ctx =
  assert_equal (Some ("c", "d")) (Lists.last_two [ "a" ; "b" ; "c" ; "d"]);
;;

let test_ListsLastTwo_ReturnsNoneIfListContainsLessThanTwoElements ctx =
  assert_equal None (Lists.last_two ["a"]);
  assert_equal None (Lists.last_two []);
;;

let test_ListsAt_ReturnsElementAtGivenPosition ctx =
  assert_equal (Some "c") (Lists.at 3 [ "a" ; "b"; "c"; "d"; "e" ])
;;

let test_ListsAt_ReturnsNoneIfIndexIsOutOfBound ctx =
  assert_equal None (Lists.at 3 ["a"]);
  assert_equal None (Lists.at 0 ["a"]);
  assert_equal None (Lists.at (-1) ["a"]);
;;

let test_ListsAt_ReturnsNoneIfListIsEmpty ctx =
  assert_equal None (Lists.at 1 []);
;;

let test_ListsLength_ReturnsNumberOfElementsInList ctx =
  assert_equal 3 (Lists.length [ "a" ; "b" ; "c"]);
  assert_equal 0 (Lists.length []);
;;

let test_ListsRev_ReturnsReversedList ctx =
  assert_equal [ "a" ; "b" ; "c" ] (Lists.rev [ "c" ; "b" ; "a" ])
;;

let test_ListsRev_SingleElementListStaysSame ctx =
  assert_equal [ "a" ] (Lists.rev [ "a" ])
;;

let test_ListsRev_EmptyListStaysSame ctx =
  assert_equal [] (Lists.rev [])
;;


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
  ]
;;

let () =
  run_test_tt_main suite
;;
