type 'a node =
    | One of 'a
    | Many of 'a node list

let list_to_string f (l: 'a list) =
  let rec elems_to_string (l: 'a list) (acc: string list) =
    match l with
      [] -> String.concat "; " acc
    | el::r -> elems_to_string r ([f el] @ acc)
  in
  "[" ^ elems_to_string l [] ^ "]"
;;

let rec last (l: 'a list) = match l with
    [] -> None
  | [el] -> Some el
  | h :: t -> last t
;;

let rec last_two (l: 'a list) = match l with
    [] -> None
  | [el] -> None
  | first :: second :: [] -> Some (first, second)
  | head :: tail -> last_two tail
;;

let rec at (i: int) (l: 'a list) =
  if i < 1 then None
  else 
    match l with 
      [] -> None
    | h :: t -> match i with
        1 -> Some h
      | _ -> at (i - 1) t 
;;

let length (l: 'a list) =
    let rec aux (l: 'a list) (acc: int) = match l with
        [] -> acc
      | [el] -> acc + 1
      | h :: t -> aux t (acc + 1) 
    in
    aux l 0
;;

let rev (l: 'a list) =
  let rec aux (l: 'a list) (acc: 'a list) = match l with
      [] -> acc
    | h :: t -> aux t ([h] @ acc)
  in
  aux l []
;;

let is_palindrome (l: 'a list) = match l with
    [] -> false
  | _ -> l = rev l
;;

let rec flatten (nodes: 'a node list) =
  let rec aux (nodes: 'a node list) (acc: 'a list) =
    match nodes with
      [] -> acc
    | h :: t -> match h with
        One(el) -> aux t (acc @ [el])
      | Many(elements) -> aux t (acc @ (flatten elements))
  in
  aux nodes []
;;

let compress (l: 'a list) =
  let rec aux (l: 'a list) (acc: 'a list) = match l with
      [] -> acc
    | h :: t -> 
      match acc with
        [] -> aux t [h]
      | acc_h :: acc_t -> if h = acc_h then aux t acc else aux t [h] @ acc
  in
  List.rev (aux l [])
;;

let pack (l: 'a list) =
  let rec aux (l: 'a list) (acc: 'a list list) (curr_sublist: 'a list) = 
    match l with
      [] -> acc @ [curr_sublist]
    | h :: t -> match curr_sublist with
        [] -> aux t acc [h]
      | sublist_h :: sublist_t -> 
        if sublist_h = h then aux t acc (h :: curr_sublist) 
                            else aux (h::t) (acc @ [curr_sublist]) []
  in
  aux l [] []
;;

let main () =
    () 
;; 

main();;
