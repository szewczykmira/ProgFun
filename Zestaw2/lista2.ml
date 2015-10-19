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
