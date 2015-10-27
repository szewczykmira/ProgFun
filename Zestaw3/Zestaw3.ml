(* Zadanie 1 *)
(* Wersja uzywajaca funkcji modulu List - uzywa fold_left poniewaz fold_right nie uzywa rekursji ogonowej*)
let horner elems x = List.fold_left (fun a b -> a *. x +. b) 0. elems;;
(* wersja z rekursja ogonowa *)

(* Zadanie 2 *)
let horner_2 elems x = List.fold_left (fun a b -> a *. x +. b) 0. (List.rev elems);;

(* Zadanie 4*)
(* 1*)
let validate_matrix matrix = let rec validate acc mat = 
  match mat with
    [] -> true
    | x::xs -> let y = List.length x in if y==acc then validate y xs else false in validate (List.length matrix) matrix;;
(* 2*)
let rec get_heads matrix =
 match matrix with
  [] -> []
  | x::xs -> (List.hd x) :: (get_heads xs);; 
let rec get_tails matrix =
  match matrix with
  [] -> []
  | x::xs -> (List.tl x) :: (get_tails xs);;

let rec get_row matrix n =
  match n with
  1 -> get_heads matrix
  | _ -> get_row (get_tails matrix) (n-1);;
(*3*)
let transposition matrix = let rec tran len acc matrix = 
  match len with
  0 -> acc
  | _ -> let l = (len -1) in tran l ((get_row matrix len)::acc ) matrix in tran (List.length matrix) [] matrix;;
