(* Zadanie 1 *)

let rec fix funkcior = fun elem ->
  funkcior (fix funkcior) elem

let s = fun f -> fun n -> if n = 0 then 1 else n * (f (n-1))
let s_fix = fix s

let silnia a = let x = ref 1 in for i=1 to a do x := !x * i done; !x

let fix2 funkcior elem = 
  let refun = ref (function f -> List.hd [])
  in let aux f e = f (!refun) e
  in refun := (aux funkcior);
  (!refun) elem

let s_fix2 = fix2 s

(* Zadanie 2 *)

type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref

let rec con = function
    ([], x) -> x
  | (x::xs, ys) -> x :: con (xs, ys)

let a = LMcons(1, ref (LMcons(3, ref (LMcons(4, ref LMnil)))))
let b = LMcons(2, ref (LMcons(3, ref (LMcons(7, ref LMnil)))))

let concat_copy a b =
  let rec aux = function
    (LMnil, x) -> x
  | (LMcons(x, xs), ys) -> LMcons(x, ref (aux (!xs, ys))) 
  in aux (a,b)

let concat_share a b =
  let rec aux = function
    (LMcons(x, y), b) -> if !y = LMnil 
    then y := b 
    else aux (!y, b)
  | (LMnil, b) -> failwith "Fail"
  in aux (a,b)

(* Zadanie 3 *)

type 'a dict = End | Dic of ('a * 'a) * 'a dict

let dicty = Dic((3,4), Dic((5,6),Dic((7,8),End)))

let create = End
let find elem tree =
  let rec aux elem = function
      End -> None
    | Dic((x,y), z) -> if x = elem then Some(y) else aux elem z
  in aux elem tree

let update key value tree = 
  let x = (key, value) 
  in Dic(x, tree)

let fib x =
  let rec aux n ac f2 f1 = if n = ac then f2 else aux n (ac+1) f1 (f1+f2)
  in aux x 0 0 1

let fib_memo x =
  let dicty = ref create 
  in let rec aux x = 
    match (find x !dicty) with
      None ->
        let an = 
        if x = 0 
        then 0 
        else if x = 1 
          then 1 
          else (aux (x-1)) + (aux (x-2))
        in dicty := (update x an !dicty); an
    | Some(x) -> x
  in aux x

(* Zadanie 4 *)

type ('a, 'b) either = Int of 'a | String of 'b

let overlap = 
  let keep = ref 0 
  in let aux = function
      String(value) -> keep := !keep + 1; value^string_of_int(!keep)
    | Int(value) -> keep := value; ""
  in aux 

let fresh x = overlap @@ String(x)
let reset x = overlap @@ Int(x)
