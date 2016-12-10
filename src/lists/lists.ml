let rec last (l: 'a list) = match l with
    [] -> None
  | [el] -> Some el
  | h :: t -> last t
;;

(* if !Sys.interactive then () else main ();; *)
