(* Zadanie 1 *)

module type PQUEUE =
sig
 type priority
 type 'a t

 exception EmptyPQueue

 val empty : 'a t
 val insert : 'a t -> priority -> 'a -> 'a t
 val remove : 'a t -> priority * 'a * 'a t
 val priority_of_int  : int -> priority
end

module PQueue : PQUEUE =
  struct
    type priority = int
    type 'a t = Empty | Queue of (priority * 'a) * 'a t

    exception EmptyPQueue

    let empty = Empty
    let insert queue pr elem =
      let rec aux pr elem = function
        Empty -> Queue((pr, elem), Empty)
      | Queue((p,x), y) ->
          if pr < p then Queue((pr, elem), Queue((p,x), y))
          else Queue((p,x), aux pr elem y)
      in aux pr elem queue
    let remove = function
      Queue((p,x),y) -> (p, x, y)
    | Empty -> raise EmptyPQueue
    let priority_of_int = function x -> x
  end

let sort lista =
  let open PQueue in
	let queue = empty
	in let aux queue = function x -> List.fold_left (fun x y -> insert x (priority_of_int y) y) queue x
	in let rec rem = function a ->
    try
      let (x,y,z) = remove a in y :: (rem z)
    with _ -> []
  in rem (aux queue lista)

module type ORDTYPE =
  sig
    type t
    val compare : t -> t -> int
    val t_of_int : int -> t
  end

module Ordnung : ORDTYPE =
  struct
    type t = int
    let compare x y = if x < y then -1 
          else if x = y then 0 else 1
    let t_of_int = fun x -> x
  end

module PQ (Ord:ORDTYPE) : PQUEUE =
  struct
    type priority = Ord.t
    type 'a t = Empty | Queue of (priority * 'a) * 'a t

    exception EmptyPQueue

    let empty = Empty
    let insert queue pr elem =
      let rec aux pr elem = function
          Empty -> Queue((pr, elem), Empty)
        | Queue((x,y), z) ->
            let a = Ord.compare pr x
            in if a < 0 then Queue((pr,elem), Queue((x,y),z))
              else Queue((x,y), aux pr elem z)
      in aux pr elem queue

    let remove = function
        Empty -> raise EmptyPQueue
      | Queue((x,y), z) -> (x,y,z)

    let priority_of_int x = Ord.t_of_int x
  end

module PQOrd = PQ(Ordnung)
let sort2 lista =
  let open PQOrd in
	let queue = empty
	in let aux queue = function x -> List.fold_left (fun x y -> insert x (priority_of_int y) y) queue x
	in let rec rem = function a ->
    try
      let (x,y,z) = remove a in y :: (rem z)
    with _ -> []
  in rem (aux queue lista)
