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

(*tworzenie zbalansowanego drzewa z listy*)
let take n lista = let rec t acc n l = 
  match n with
    0 -> List.rev acc
  | _ -> t ((List.hd l)::acc) (n-1) (List.tl l) in t [] n lista;; 
let rec drop n lista = if n = 0 then lista else drop (n-1) (List.tl lista);;
let rec make_tree = function
    [] -> Leaf
  | x::xs -> let len = List.length xs 
  in Node(make_tree (take (len/2) xs),x,make_tree (drop (len/2) xs));;
(* Zadanie 3*)
type 'a mtree = MNode of 'a * 'a forest and 'a forest = EmptyForest | Forest of 'a mtree * 'a forest;;
(* przechodzenie w glab *)
let rec dfs_forest = function
    EmptyForest -> []
  | Forest(MNode(m,f), forest) -> (m)::(dfs_forest (f))@(dfs_forest forest);;
let rec dfs_mtree = function
    MNode(root, EmptyForest) -> [root]
  | MNode(root, forest) -> root::(dfs_forest forest);;
(* przechodzenie wszerz *)
let bfs_forest t = let rec forest_aux = function
    [] -> []
  | EmptyForest::t -> forest_aux t
  | Forest(MNode(m,f),forest)::t -> m::forest_aux (forest::t@[f]) in forest_aux [t];;
let bfs_mtree = function
    MNode(root, EmptyForest) -> [root]
  | MNode(root, forest) -> root::(bfs_forest forest);;

(* Druga definicja drzewa wielokierunkowego *)
type 'a mtree_lst = MTree of 'a * ('a mtree_lst) list;;
(* dfs *)
let dfs t = let rec dfs2 = function
    [] -> []
  | MTree(m, [])::t -> m::dfs2 t
  | MTree(m, l)::t -> m::dfs2 (l@t) in dfs2 [t];;
(* bfs *)
let bfs t = let rec bfs2 = function
    [] -> []
  | MTree(m,[])::t -> m::bfs2 t
  | MTree(m,lista)::t -> m::bfs2 (t@lista) in bfs2 [t];;

(* Zadanie 4 *)
type typdanych = Var of string | Not of typdanych | Con of typdanych * typdanych | Alt of typdanych * typdanych;;

type wartosciowanie = string -> bool

let magic f x v = function y -> if x = y then v else f y;;
let generowanie:string list -> wartosciowanie list = let rec cos = function
    [] -> []
  | y::ys -> let rys = cos ys in let pos f = magic f y true in let neg f = magic f y false in List.map pos rys @ List.map neg rys in cos;;

let rec evaluacja f = function
    Var(x) -> f x
  | Not(x) -> not (evaluacja f x)
  | Con(x,y) -> (evaluacja f x) && (evaluacja f y)
  | Alt(x,y) -> (evaluacja f x ) || (evaluacja f y);;

let is_taut formula val_list = List.find (fun x -> not (evaluacja x formula)) val_list;;

let good = [1;2;3;2;1];;
let bad = [1;2;3;4];;
let good_tree = Node(Node(Leaf,2,Leaf),5,Node(Leaf,7,Node(Leaf,4,Leaf)));;
let bad_tree = Node(Leaf, 2,Node(Leaf,7,Node(Leaf,6,Leaf)));;
let lista = [2;5;6;7;8];;
let mtr = MNode(6,Forest(MNode(6,Forest(MNode(7,EmptyForest),EmptyForest)), Forest(MNode(9,EmptyForest),EmptyForest)));;
