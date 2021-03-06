(* Zadanie 1 *)
type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree

let tree1 = Node(Node(Leaf 1, Leaf 2), Leaf 3)
let tree2 = Node(Leaf 1, Node(Leaf 2, Leaf 3))
let tree3 = Node(Leaf 3, Node(Leaf 1, Leaf 2))

let fringe tree =
  let rec aux acc  = function
      [] -> List.rev acc
    | (Leaf(x)::xs) -> aux (x::acc) xs
    | (Node(left, right)::xs) -> aux acc (left::right::xs)
  in aux [] [tree]

let compare_fringe = fun t1 -> fun t2 -> (fringe t1) = (fringe t2)

open Lazy
type 'a stream = Nil | Cons of 'a * 'a stream Lazy.t

let fringe2 tree =
  let rec aux = function
      [] -> Nil
    | (Leaf(x)::xs) -> Cons(x, lazy (aux xs))
    | (Node(left, rigth)::xs) -> aux (left::rigth::xs)
  in aux [tree]

let same_fringe tree1 tree2 =
  let rec aux = function
      (Nil, Nil) -> true
    | (Nil, _) -> false
    | (_, Nil) -> false
    | (Cons(x,xs), Cons(y, ys)) -> if x=y then aux (force xs, force ys) else false
  in aux ((fringe2 tree1),(fringe2 tree2))

(* Zadanie 2 *)
type 'a tree = TLeaf of 'a | TNode of 'a tree * 'a * 'a tree

let ftree = TNode (TNode(TLeaf 'a', 'b', TLeaf 'c'), 'd', TLeaf 'e')
let inttree = TNode( TNode (TLeaf 4, 5, TLeaf 8), 10, TLeaf 23)

type info_tree = N | L

let dfs_forest tree = 
  let rec aux acc= function
      [] -> []
    | (TLeaf(x)::xs) -> (L,acc) :: aux (acc+1) xs
    | (TNode(left, root, right)::xs) -> (N, acc) :: aux (acc+1) (left::right::xs)
  in aux 1 [tree]


let dfs_remake_tree = 
  let rec aux = function
      [] -> failwith "error"
    | (L,x)::xs -> (TLeaf(x), xs)
    | (N,x)::xs -> let (left, lp) = aux xs
    in let (right, rp) = aux lp in (TNode(left, x, right), rp)
  in fun tree -> match aux tree with
      (x, []) -> x
    | _ -> failwith "error"

let dfs tree = dfs_remake_tree (dfs_forest tree)

let bfs_forest tree = 
  let rec aux acc = function
      [] -> []
    | ((TLeaf(x), par)::xs) -> (L, acc, par) :: aux (acc+1) xs
    | ((TNode(left, root, right), par)::xs) -> (N, acc, par) :: aux (acc+1) (xs@[(left,acc)]@[(right,acc)])
  in aux 1 [(tree, 0)] 

let find_elem p = List.filter (fun (_, y, _) -> y = p)
let find_sons p = List.filter (fun (_, _, z) -> z = p)
 
let rebuild t =
  let rec aux par = match find_elem par t with
    | [(L, n, _)] -> TLeaf(n)
    | [(N, n, _)] -> (match find_sons par t with
      | [(_, nl, _); (_, nr, _)] -> (match List.map aux [nl; nr] with
        | [lt; rt] -> TNode(lt, n, rt)
        | _ -> failwith "error")
      | _ -> failwith "error")
    | _ -> failwith "error"
  in aux 1

let bfs tree = rebuild (bfs_forest tree)

(* Zadanie 3 *)

type 'a btree = BLeaf | BNode of 'a btree * 'a * 'a btree
type 'a barray = BArray of 'a btree * int

let some_tree = BNode(BNode(BNode(BLeaf, 4, BLeaf), 2, BNode(BLeaf, 5, BLeaf)), 1, BNode(BNode(BLeaf, 6, BLeaf), 3, BNode(BLeaf, 7, BLeaf)))
let some_array = BArray(some_tree, 7)

let aempty = BArray(BLeaf, 0)
let asub index tree = 
  let rec aux = function
      (x, BNode(left, root, right)) -> if x = 1 then root else 
        let dir = if x mod 2 = 0 then left else right 
        in aux (x/2, dir)
    | (_, BLeaf) -> failwith "list index out of range"
  in match tree with
    BArray(x, y) -> if y < index then failwith "list index out of range" else aux (index, x)

let aupdate tree index ninf =
  let rec aux n = function
      (1, BNode(left, root, right)) -> BNode(left, n, right)
    | (x, BNode(left, root, right)) -> if x mod 2 = 0 then BNode(aux n (x/2, left), root, right) else BNode(left, root, aux n (x/2, right))
    | (x, BLeaf) -> failwith "list index out of range"
  in match tree with
    BArray(x, y) -> if y < index then failwith "list index out of range" 
        else aux ninf (index, x)

let ahiext tree ninf =
  let rec aux ninf = function
      (_, BLeaf) -> BNode(BLeaf, ninf, BLeaf)
    | (1, BNode(_,_,_)) -> failwith "error"
    | (x, BNode(left, root, right)) -> if x mod 2 = 0 
      then BNode( aux ninf ((x/2), left), root, right) 
      else BNode(left, root, aux ninf ((x/2), right))
  in match tree with
    BArray(x, y) -> BArray(aux ninf ((y+1), x), y+1)

let ahirem tree =
  let rec aux = function
      (_, BLeaf) -> failwith "list index out of range"
    | (1, BNode(_,_,_)) -> BLeaf
    | (x, BNode(left, root, right)) -> if x mod 2 = 0
      then BNode(aux ((x/2, left)), root, right)
      else BNode(left, root, aux ((x/2), right))
  in match tree with
    BArray(x,y) -> BArray(aux (y, x), y-1)
