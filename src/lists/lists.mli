type 'a node =
  | One of 'a
  | Many of 'a node list
;;

type 'a rle =
  | RLEOne of 'a
  | RLEMany of (int * 'a)
;;

val last : 'a list -> 'a option;;
val last_two : 'a list -> ('a * 'a) option;;
val at : int -> 'a list -> 'a option;;
val length : 'a list -> int;;
val rev : 'a list -> 'a list;;
val is_palindrome : 'a list -> bool;;
val flatten : 'a node list -> 'a list;;
val compress : 'a list -> 'a list;;
val pack : 'a list -> 'a list list;;
val encode : 'a list -> 'a rle list;;
