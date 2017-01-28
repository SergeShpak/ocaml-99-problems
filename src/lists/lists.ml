type 'a node =
    | One of 'a
    | Many of 'a node list
;;

type 'a rle =
  | RLEOne of 'a
  | RLEMany of (int * 'a)
;;

let list_to_string f (l: 'a list) =
  let rec elems_to_string (l: 'a list) (acc: string list) =
    match l with
      [] -> String.concat "; " (List.rev acc)
    | el::r -> elems_to_string r ((f el) :: acc)
  in
  "[" ^ elems_to_string l [] ^ "]"
;;

let int_str_tuple_to_string (t: (int * string)) =
  let (f, s) = t in
  let string_parts = ["("; string_of_int f; ", "; s; ")"] in
  String.concat "" string_parts
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

let encode (l: 'a list) =
  let rec aux (l: 'a list) (acc: 'a rle list) (cur: 'a rle option) =
    match l with 
      [] -> begin
                match cur with 
              Some c -> c :: acc 
            | None -> acc
      end
    | el :: rest -> 
      begin
        match cur with 
          None -> aux rest acc (Some (RLEOne el))
        | Some (RLEOne e) -> 
          if e = el then
            aux rest acc (Some (RLEMany (2, e)))
          else
            let new_el = RLEOne e in
            aux rest (new_el :: acc) (Some (RLEOne el)) 
        | Some (RLEMany (count, e)) ->
          if e = el then
            aux rest acc (Some (RLEMany (count + 1, e)))
          else
            let new_el = RLEMany (count, e) in
            aux rest (new_el :: acc) (Some (RLEOne el))
      end 
  in
  let rec encoding_packed (l: 'a list list) (acc: 'a rle list) =
    match l with
      li :: rest -> let len = List.length li in
      let head = List.hd li in
      if len = 1 then
        encoding_packed rest ((RLEOne head) :: acc)
      else
        encoding_packed rest ((RLEMany (len, head)) :: acc)
    | [] -> rev acc 
  in
  List.rev (aux l [] None)  
;;

let decode(l: 'a rle list) =
  ()
;;

let main () =
  ()
;; 

main();;
