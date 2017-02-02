type 'a node =
  | One of 'a
  | Many of 'a node list

type 'a rle =
  | RLEOne of 'a
  | RLEMany of (int * 'a)

val last : 'a list -> 'a option
val last_two : 'a list -> ('a * 'a) option
val at : int -> 'a list -> 'a option
val length : 'a list -> int
val rev : 'a list -> 'a list
val is_palindrome : 'a list -> bool
val flatten : 'a node list -> 'a list
val compress : 'a list -> 'a list
val pack : 'a list -> 'a list list
val encode : 'a list -> 'a rle list
val decode : 'a rle list -> 'a list
val duplicate : 'a list -> 'a list
val replicate : 'a list -> int -> 'a list
val drop : 'a list -> int -> 'a list
val split : 'a list -> int -> ('a list * 'a list)
val slice : 'a list -> int -> int -> 'a list
val rotate : 'a list -> int -> 'a list
val remove_at : int -> 'a list -> 'a list
val insert_at : 'a -> int -> 'a list -> 'a list
