(* definicja typu leniwej listy nieskończonej *)
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec ltake : int * 'b llist -> 'b list = function
    (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x, xf)) -> x:: ltake (n-1, xf());;

let ltl : 'a llist -> 'a llist = function
    LNil -> failwith "ltl"
  | LCons(_, x) -> x();;

(* Podpunkt 1 *)
let fl = float_of_int;;
let leibniz =
  let rec aux = let sign x = if x mod 2=0 then 1. else -1. 
    in let leb n = (sign n)*.(1./.(2. *. (fl n) +. 1.))  
      in fun x -> LCons (leb x, function () -> aux (x+1))
  in aux 0;;

let get_round = fun x -> (List.fold_left (fun acc y -> acc +. y) 0. (ltake (x, leibniz)));;

let get_pi_round = fun x -> 4. *. get_round x;;

(* Podpunkt 2*)
let rec zipf : ('a list -> 'b) -> 'a llist -> 'b llist =
  fun f stream -> LCons(f(ltake (3, stream)), fun () -> zipf f (ltl stream));;

(* Podpunkt 3 *)
let f x y z = z -. (((y-.z)**2.)/.(x-.(2.*.y)+.z));;

let leib_stream = 
  let rec aux = 
    fun x -> LCons(get_round x, fun () -> aux (x+1)) 
  in aux 0;;

let euler = zipf (fun arr -> let x = List.nth arr 0 
  in let y = List.nth arr 1 
    in let z = List.nth arr 2 in f x y z) leib_stream;;

let get_euler_round = fun n -> List.map (fun a -> a *. 4.) (ltake (n, euler));;

(* Wersja z uzyciem modulu Lazy *)
type 'a slist = Nil | Cons of 'a * 'a slist Lazy.t;;

let stl = function
    Nil -> failwith "stl"
  | Cons(_, x) -> Lazy.force x;;

let rec stake = function
    (0, _) -> []
  | (_, Nil) -> []
  | (n, Cons(y, ys)) -> y::stake ((n-1), Lazy.force ys);;

(* Podpunkt 1 *)

let sleibniz = 
  let rec aux = let sign x = if x mod 2 = 0 then 1. else -1. 
    in let leb n = (sign n)*.(1./.(2. *. (fl n) +. 1.))
      in fun x -> Cons(leb x, lazy (aux (x+1)))
  in aux 0;;

let get_sround = fun x -> List.fold_left (fun a b -> a+.b) 0. (stake (x, sleibniz));;
let get_pi_sround x = 4.*. (get_sround x);;

(* Podpunkt 2 *)

let rec szipf : ('a list -> 'b) -> 'a slist -> 'b slist = fun g stream ->
  Cons(g (stake (3, stream)),lazy (szipf g (stl stream )));;

(* Podpunkt 3 *)

let sleib_stream = 
  let rec aux = 
    fun x -> Cons(get_sround x, lazy (aux (x+1))) 
  in aux 0;;

let seuler = szipf (fun arr -> let x = List.nth arr 0 
  in let y = List.nth arr 1 
    in let z = List.nth arr 2 in f x y z) sleib_stream;;

let get_euler_sround = fun n -> List.map (fun a -> a *. 4.) (stake (n, seuler));;

(* Zadanie 2 *)

let rec lfilter pred = function
    LNil -> LNil
  | LCons(x,xf) -> if pred x then LCons(x, function () -> lfilter pred (xf()) ) else lfilter pred (xf());;

let breadthFirst next x =
  let rec bfs = function
    [] -> LNil
  | (h::t) -> LCons(h, function() -> bfs (t@next h))
  in bfs [x];;

let saveQueen oldq newq =
  let rec nodiag = function
    (i, []) -> true
  | (i, q::qt) -> abs (newq-q) <> i && nodiag (i+1, qt)
  in not (List.mem newq oldq) && nodiag (1, oldq);;

let rec fromTo a b =
  if a>b then []
    else a::(fromTo (a+1) b);;

let nextQueen n qs =
  List.map (fun h -> h::qs) (List.filter (saveQueen qs) (fromTo 1 n) );;

let solution n qs = List.length qs = n;;
let breadthQueen n = lfilter (solution n) (breadthFirst (nextQueen n) []);;


(* Zadanie 3 *)

let bfsQueen n =
  let rec aux acc = function
    [] -> acc
  | (h::t) -> let (sol, pos) = if solution n h then ([h], []) else ([], nextQueen n h) 
  in aux (sol@acc) (t@pos) 
in aux [] [[]];;

(* Zadanie 4 *)

type 'a tree = Leaf of ('a * int ) | Tree of ('a tree * int * 'a tree);;

let value = function
    Leaf(_, f) -> f
  | Tree(_,f,_) -> f;;

let compare a b = if a = b then 0 else if a < b then -1 else 1;;

let sort x = List.sort (fun x y -> compare (value x) (value y)) x;;

let build_tree x =
  let rec build = function
      [] -> failwith "empty"
    | [x] -> x
    | (x::y::ys) -> build ( (Tree(y, (value x) + (value y), x) ):: ys)
  in build (sort x)

let rec findInTree sym = function
    Leaf(s, _) -> if s = sym then true else false
  | Tree(left, _, right) -> (findInTree sym left) || (findInTree sym right)

let encode sym tree =
  let aux sym route = function
    Leaf(s, _) -> if s = sym then true else false
  | Tree(left, _, right) -> let aux 
