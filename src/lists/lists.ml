type 'a node =
    | One of 'a
    | Many of 'a node list


type 'a rle =
  | RLEOne of 'a
  | RLEMany of (int * 'a)


let list_to_string f (l: 'a list) =
  let rec elems_to_string (l: 'a list) (acc: string list) =
    match l with
      [] -> String.concat "; " (List.rev acc)
    | el::r -> elems_to_string r ((f el) :: acc)
  in
  "[" ^ elems_to_string l [] ^ "]"


let id (el: 'a) =
  el


let int_str_tuple_to_string (t: (int * string)) =
  let (f, s) = t in
  let string_parts = ["("; string_of_int f; ", "; s; ")"] in
  String.concat "" string_parts


let rec last (l: 'a list) = match l with
    [] -> None
  | [el] -> Some el
  | h :: t -> last t


let rec last_two (l: 'a list) = match l with
    [] -> None
  | [el] -> None
  | first :: second :: [] -> Some (first, second)
  | head :: tail -> last_two tail


let rec at (i: int) (l: 'a list) =
  if i < 1 then None
  else 
    match l with 
      [] -> None
    | h :: t -> match i with
        1 -> Some h
      | _ -> at (i - 1) t 


let length (l: 'a list) =
    let rec aux (l: 'a list) (acc: int) = match l with
        [] -> acc
      | [el] -> acc + 1
      | h :: t -> aux t (acc + 1) 
    in
    aux l 0


let rev (l: 'a list) =
  let rec aux (l: 'a list) (acc: 'a list) = match l with
      [] -> acc
    | h :: t -> aux t ([h] @ acc)
  in
  aux l []


let is_palindrome (l: 'a list) = match l with
    [] -> false
  | _ -> l = rev l


let rec flatten (nodes: 'a node list) =
  let rec aux (nodes: 'a node list) (acc: 'a list) =
    match nodes with
      [] -> acc
    | h :: t -> match h with
        One(el) -> aux t (acc @ [el])
      | Many(elements) -> aux t (acc @ (flatten elements))
  in
  aux nodes []


let compress (l: 'a list) =
  let rec aux (l: 'a list) (acc: 'a list) = match l with
      [] -> acc
    | h :: t -> 
      match acc with
        [] -> aux t [h]
      | acc_h :: acc_t -> if h = acc_h then aux t acc else aux t [h] @ acc
  in
  List.rev (aux l [])


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
  rev (aux l [] None)  


let decode(l: 'a rle list) =

  let rec clone_el (el: 'a) (times_to_clone: int) (acc: 'a list) : 'a list =
    if times_to_clone > 0 then
      clone_el el (times_to_clone - 1) (el :: acc)
    else
      acc
  in

  let rec aux (l: 'a rle list) (acc: 'a list) =
    match l with
      [] -> acc
    | h :: rest ->
      match h with
        RLEOne el -> aux rest (el :: acc) 
      | RLEMany (count, el) ->
        begin
          let sublist = clone_el el count [] in
          aux rest (sublist @ acc)
        end
  in

    rev (aux l [])


let duplicate (l: 'a list) =

  let rec aux (l: 'a list) (acc: 'a list) =
    match l with
      [] -> rev acc
    | el::rest -> aux rest (el::el::acc)
  in

  aux l []


let replicate (l: 'a list) (times: int) =

  let rec aux (l: 'a list) (acc: 'a list) (counter: int) =
    match l with
      [] -> rev acc
    | el::rest ->
      if counter > 0 then
        aux (l: 'a list) (el :: acc) (counter - 1)
      else
        aux rest acc times
  in

  aux l [] times


let drop (l: 'a list) (index: int) =

  let rec aux (l: 'a list) (acc: 'a list) (counter: int) =
    if index <= 0 then
      replicate l 1
    else
    match l with
      [] -> rev acc
    | el::rest ->
      match counter with
        i when i < index -> aux rest (el :: acc) (counter + 1)
      | i when i = index -> aux rest acc 1
      | _ -> rev acc
  in

  aux l [] 1


let split (l: 'a list) (first_part_length: int) = 

  let rec aux (l: 'a list) (acc: 'a list) (counter: int) =
    if counter <= 0 then
      ((rev acc), l)
    else
      match l with
        [] -> ((rev acc), [])
      | [el] -> ((rev (el::acc)), [])
      | h :: el -> aux el (h :: acc) (counter - 1)
  in
  if (first_part_length <= 0) then
    ([], l)
  else
    aux l [] first_part_length


let slice (l: 'a list) (start_ind: int) (end_ind: int) =

  let rec skip_n (l: 'a list) (n: int) =
    if n <= 0 then l else
    match l with
      [] -> []
    | h :: t -> skip_n t (n - 1)
  in

  let rec get_n (l: 'a list) (n: int) (acc: 'a list) =
    if n < 0 then rev acc else
    match l with
      [] -> rev acc
    | h :: t -> get_n t (n - 1) (h :: acc)
  in

  let rec get_indices_pair (start_ind: int) (end_ind: int) (len: int) =
    let neg_normalize start_ind end_ind =
      let norm_neg ind = if ind < 0 then len + ind else ind in
      let norm_start = norm_neg start_ind and norm_end = norm_neg end_ind in
      (norm_start, norm_end)
    in
    let swap_if_needed (first, second) =
      if (first > second && second >= 0) then (second, first)
      else (first, second)
    in
    swap_if_needed (neg_normalize start_ind end_ind)
  in

  let indices = get_indices_pair start_ind end_ind (List.length l) in
  let start_ind = fst indices and end_ind = snd indices in
  if end_ind < 0 then [] else
  if start_ind < 0 then get_n l end_ind [] else
  get_n (skip_n l start_ind) (end_ind - start_ind) []


let rec rotate (l: 'a list) (rot_ind: int) =
  let rec left_rot (l: 'a list) (rot_ind: int) (acc: 'a list) =
    match l with
      [] -> rev acc
    | h :: t -> if rot_ind = 0 then (l @ (rev acc))
      else left_rot t (rot_ind - 1) (h :: acc)
  in
    
  let rec right_rot (l: 'a list) (rot_ind: int) (acc: 'a list) =
    rev (left_rot (rev l) rot_ind acc)
  in 

  let list_len = List.length l in
  let norm_rot_ind = rot_ind mod list_len in
  if norm_rot_ind = 0 then l else 
  if norm_rot_ind < 0 then rotate l (norm_rot_ind + list_len) else
  if norm_rot_ind > list_len / 2 then right_rot l (list_len - norm_rot_ind) [] 
  else
  left_rot l norm_rot_ind []


let remove_at (pos: int) (l: 'a list) =
  let rec aux (l: 'a list) (count: int) (acc: 'a list) =
    match l with
      [] -> rev acc
    | h :: t -> if count = 0 then (rev acc) @ t
      else aux t (count - 1) (h :: acc)
  in

  let list_len = List.length l in
  let pos = if pos < 0 then list_len + pos else pos in
  if pos < 0 || pos > (list_len - 1) then l
  else aux l pos []


let insert_at (el: 'a) (pos: int) (l: 'a list) =
  let rec aux (count: int) (l: 'a list) (acc: 'a list) =
    match l with 
      [] -> if count = 0 then rev (el :: acc) else rev acc
    | h :: t -> 
      if count = 0 then aux (count - 1) t (h :: (el :: acc)) else
      if count < 0 then aux count t (h :: acc) 
      else aux (count - 1) t (h :: acc)
  in
  let pos = if pos >= 0 then pos
    else let list_len = List.length l in (list_len + 1) + pos in
  aux pos l [] 


let range (start_number : int) (end_number : int) =

  let decrement el = el - 1 and
  increment el = el + 1 and
  
  aux (curr_number : int) (end_number : int) mod_func (acc: int list) =
    let rec inner (curr_number : int) (acc : int list) =
    if curr_number = end_number then (curr_number :: acc)
    else inner (mod_func curr_number) (curr_number :: acc) in
    inner curr_number acc in

  if start_number > end_number then 
    let mod_func = increment in aux end_number start_number mod_func []
  else
    let mod_func = decrement in aux end_number start_number mod_func []


let main () =
  ()
;; 

main();;
