(* Zadanie 1 *)
let palindrom  lista = let rec part_acc acc counter l = 
  match counter with
    [] -> acc = l
  | [x] -> acc = (List.tl l)
  | a::b::c -> part_acc ((List.hd l)::acc) c (List.tl l) in part_acc [] lista lista;;

(* Zadanie 2 *)
type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;
(*Czesc pierwsza - sprawdzanie czy drzewo jest zbalansowane*)
let rec balance = function
    Leaf -> (0, true)
  | Node(left, root, right) -> let l = balance left and r = balance right 
in ((max (fst l) (fst r)) + 1, if (abs ((fst l) - (fst r))) > 1 then false else (snd l) && (snd r));;
let balanced = fun x -> snd (balance x);;

