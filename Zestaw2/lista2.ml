(* Zadanie 1 *)
let rec sublist l = 
  match l with
  [] -> [[]]
  | x::xs -> let ys = sublist xs in  (List.map (fun y -> x::y) ys) @ ys

(* Zadanie 2 *)
let rec f x = 
  match x with
  0 -> 1
  | 1 -> 2
  | _ -> 2*(f (x-2)) - (f (x-1)) +1

let f_tail n =
  let rec f_acc (n1, n2) n = 
    match n with
    0 -> n2
  | 1 -> n1
  | _ -> f_acc ((2*(n2) - n1 + 1), n1) (n-1)
  in f_acc (2, 1) n
 
(* Zadanie 3 *)
let z3_tail f l =
 let rec z3_acc f l acc =
   match l with
   [] -> acc
  | _ -> (z3_acc f (List.tl l) (f (List.hd l)::acc ))
 in z3_acc f l []

let rec z31 f l = if l = [] then [] else (z31 f (List.tl l))@[f (List.hd l)] 

(* Zadanie 4 *)
let rec merge cmp l1 l2 =
  match l1, l2 with
  [], _ -> l2
  | _, [] -> l1
  | h1::t1, h2::t2 -> (match cmp h1 h2 with
    | true -> h1 :: merge cmp t1 l2
    | _ -> h2 :: merge cmp l1 t2)

let tail_merge cmp l1 l2 = 
  let rec tail_acc cmp l1 l2 acc = 
    match l1, l2 with
    [], _ -> (List.rev acc)@l2
    | _, [] -> (List.rev acc)@l1
    | h1::t1, h2::t2 -> (match cmp h1 h2 with
      | true -> tail_acc cmp t1 l2 (h1::acc)
      | _ -> tail_acc cmp l1 t2 (h2::acc))
  in tail_acc cmp l1 l2 []

let rec take n l = if n = 0 then [] else (List.hd l)::(take (n-1) (List.tl l))
let rec drop n l = if n = 0 then l else drop (n-1) (List.tl l)

let rec mergesort l =
  match l with
  [] -> []
  | [h] -> [h]
  | _ -> let left = take (List.length l/2) l in
        let right = drop (List.length l/2) l in
        tail_merge (<=) (mergesort left) (mergesort right) 

(* Zadanie 5 *)
let rec insert x l = 
  match l with
    [] -> [[x]]
    | y::ys -> [x::l] @ (List.map (fun a -> y::a) (insert x ys))

let rec perms l = 
  match l with
    [] ->  [[]]
    | x::xs -> let ys = perms xs in List.concat (List.map (fun y -> insert x y) ys) 

(* Zadanie 6 *)
let rec suffixes l =
  match l with 
  [] -> []
  | _ -> l::(suffixes (List.tl l)) 

let rec tails l = List.rev (suffixes (List.rev l))
let rec prefix l = 
  match l with 
    [] -> [[]]
  | x::xs -> List.map (fun y -> x::y) (prefix xs) @ [[]]


let some_list = [1;2;3;4;5;6;7;8];;
let another_list = [2;3;6;8;12];;
let unsorted_list = [5;4;7;812;2;1;8;0;9];;

