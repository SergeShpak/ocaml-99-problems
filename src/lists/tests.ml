open OUnit2;;

let test_ListsLast_ReturnsLastElement ctx = 
  assert_equal (Some "d") (Lists.last[ "a" ; "b" ; "c" ; "d"])
;;

let test_ListsLast_ReturnsNoneIfListIsEmpty ctx =
  assert_equal (None) (Lists.last[])
;;

let suite = 
  "suite">:::
  ["Lists.last: Returns last element">:: test_ListsLast_ReturnsLastElement ;
   "Lists.last: Returns 'None' if list is empty">:: 
   test_ListsLast_ReturnsNoneIfListIsEmpty]
;;

let () =
  run_test_tt_main suite
;;
