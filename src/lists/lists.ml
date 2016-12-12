type 'a node =
    | One of 'a
    | Many of 'a node list
;;

let rec print_list (l: 'a list) = match l with
    [] -> print_endline
  | [el] -> print_string el ; print_endline
  | h :: t -> print_string h ; print_string " "; print_list t
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
