(* Zadanie 2 *)
let rec f x = 
  match x with
  0 -> 1
  | 1 -> 2
  | _ -> 2*(f (x-2)) - (f (x-1)) +1
 
(* Zadanie 3 *)
let z3 f l =
 let rec z33 f l acc =
   match l with
   [] -> acc
  | _ -> (z33 f (List.tl l) (f (List.hd l)::acc ))
 in z33 f l []

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
    [], _ -> acc@l2
    | _, [] -> acc@l1
    | h1::t1, h2::t2 -> (match cmp h1 h2 with
      | true -> tail_acc cmp t1 l2 (acc@[h1])
      | _ -> tail_acc cmp l1 t2 (acc@[h2]))
  in tail_acc cmp l1 l2 []