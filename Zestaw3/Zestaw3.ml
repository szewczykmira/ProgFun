(* Zadanie 1 *)
(* Wersja uzywajaca funkcji modulu List - uzywa fold_left poniewaz fold_right nie uzywa rekursji ogonowej*)
let horner elems x = List.fold_left (fun a b -> a *. x +. b) 0. elems;;
(* wersja z rekursja ogonowa *)
let horner_tail elems x = let rec h_acc acc elems x =
  match elems with
  [] -> acc
  |y::ys -> h_acc ((fun a b -> a *. x +. b) acc y) ys x in h_acc 0. elems x;;
(* Zadanie 2 *)
let horner_2 elems x = List.fold_left (fun a b -> a *. x +. b) 0. (List.rev elems);;
let h_tail_2 elems x = let rec h_acc_2 (acc, pot) elems x =
  match elems with
    [] -> acc
  | y::ys -> h_acc_2 (acc +. (pot *. y), pot*.x) ys x in h_acc_2 (0., 1.) elems x;;
(*Zadanie 3*)
let pochodna lista = let rec pochodna_acc acc num  lista =
  match lista with
  [] -> List.tl (List.rev acc)
  | x::xs -> pochodna_acc ((x*.num)::acc) (num+.1.) xs in pochodna_acc [] 0. lista;;
let poch lista = List.rev (fst (List.fold_left (fun (x,y) a -> ((a*.(y+.1.))::x, y+.1.)) ([],0.) (List.tl lista)));;
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
(*4*)
let rec zip list_a list_b =
  match list_a with
    [] -> []
  | x::xs -> (x, (List.hd list_b)) :: zip xs (List.tl list_b);;
(*5*)
let rec zipf f list_a list_b =
  match list_a with
    [] -> []
  | x::xs -> (f x (List.hd list_b)) :: zipf f xs (List.tl list_b);;
(*6*)
let mult i lista = let rec mult_acc acc i lista =
  match lista with
  [] -> acc
  | x::xs -> mult_acc ((x*.i)::acc) i xs in mult_acc [] i lista;;
let list_sum lista = let rec list_acc acc lista =
  match lista with
  [] -> acc
  | x::xs -> list_acc (acc +. x) xs in list_acc 0. lista;;
let mult_vec vex  mat = List.rev (List.map list_sum (transposition (zipf mult vex mat)));;
(*7*)
let mult_mat mat1 mat2 = List.map (fun x -> mult_vec x mat2) mat1;;


let data = [1.;0.;-1.;2.];;
let matrix = [[1;2;3];[4;5;6];[7;8;9]];;
let b_matrix = [1;2];;
let vec = [1.;2.];;
let m = [[2.;0.];[4.;5.]];;
let iden = [[1.;0.];[0.;1.]];;
