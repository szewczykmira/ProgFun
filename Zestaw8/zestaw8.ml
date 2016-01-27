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

(* Zadanie 2 *)

module type VERTEX =
  sig
    type t
    type label

    val equal : t -> t -> bool
    val create : label -> t
    val label : t -> label
    val str_to_label : string -> label
  end

module type EDGE =
  sig
    type t
    type label
    type vertex

    val equal : t * t -> bool
    val create : vertex -> vertex -> label -> t  
    val label : t -> label
    val start : t -> vertex
    val finish : t -> vertex
    val str_to_label : string -> label
  end

module Vertex : VERTEX with type label = string =
  struct
    type label = string
    type t = label 

    let equal v1 v2 = v1 = v2
    let str_to_label s = s
    let create lab = str_to_label lab
    let label st = st
  end

module Edge : EDGE with type vertex = Vertex.t
                        and type label = string =
  struct
    type label = string
    type vertex = Vertex.t
    type t = label * vertex * vertex

    let equal = function 
      ((x,y,z), (a,b,c)) -> 
        if x=a || y = b || z = c then true else false
    let str_to_label s = s
    let create st en lab = (str_to_label lab, str_to_label st, str_to_label en)
    let label = function
        (x,y,z) -> x
    let start = function
        (x,y,z) -> y
    let finish = function
        (x,y,z) -> z
  end

module type GRAPH =
  sig
    (* typ prezentacji grafu *)
    type t
    module V : VERTEX

    type vertex = V.t

    module E : EDGE with type vertex = vertex

    type edge = E.t

    (* funkcje wyszukiwania *)
    val mem_v : t -> vertex -> bool
    val mem_e : t -> edge -> bool
    val mem_e_v : t -> vertex -> vertex -> bool
    val find_e : t -> vertex -> vertex -> edge
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list

    (* funkcje modyfikacji *)
    val empty : t
    val add_e : t -> edge -> t
    val add_v : t -> vertex -> t
    val rem_e : t -> edge -> t
    val rem_v : t -> vertex -> t

    (* Iteratory *)
    val fold_v : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
  end

module Graph : GRAPH with type E.label = string and type V.label = string =
  struct
    (* typ prezentacji grafu *)

    module E = Edge
    module V = Vertex

    type vertex = V.t
    type edge = E.t

    type t = Empty | Graph of vertex list * edge list

    (* Funkcje pomocnicze *)
    let edges_list = function
        Empty -> []
      | Graph(_,y) -> y
    let vertex_list = function
        Empty -> []
      | Graph(x,_) -> x

    (* funkcje wyszukiwania *)
    let mem_v graph vert = 
      let aux vert = function
          Empty -> false
        | Graph(x,y) -> List.mem vert x
      in aux vert graph

    let mem_e graph ed =
      let aux ed = function
          Empty -> false
        | Graph(x,y) -> List.mem ed y
      in aux ed graph

    let mem_e_v graph st en = 
      let rec aux st en = function
          [] -> false
        | x::xs -> if (E.start x) = st && (E.finish x) = en then true else aux st en xs
      in aux st en (edges_list graph)

    let find_e graph st en =
      let aux st en = function
          Empty -> failwith "Not found"
        | Graph(x,y) -> List.find (function ed -> (E.start ed) = st && (E.finish ed) = en) y
      in aux st en graph

    let succ graph vert = 
      let ed = List.filter (function x -> (E.start x) = vert ) (edges_list graph) 
      in List.map (fun x -> E.finish x) ed

    let pred graph vert =  
      let edg = List.filter (function x -> (E.finish x) = vert) (edges_list graph) 
      in List.map (fun x -> E.start x) edg

    let succ_e graph vert = List.filter (function x -> vert = (E.start x)) (edges_list graph)

    let pred_e graph vert = List.filter (function x -> vert = (E.finish x)) (edges_list graph)

  
    (* funkcje modyfikacji *)
    let empty = Empty
    let add_e graph e = 
      let verts = vertex_list graph 
      in let edgs = edges_list graph 
      in Graph(verts, e::edgs) 

    let add_v graph v = 
      let verts = vertex_list graph
      in let edgs = edges_list graph
      in Graph(v::verts, edgs)

    let rem_e graph e = 
      let rec aux e = function
          [] -> []
        | x::xs -> if E.equal (x, e) then xs else x :: (aux e xs)
      in let edgs = edges_list graph 
      in Graph(vertex_list graph, aux e edgs)
    
    let rem_v graph v =
      let rec aux v = function
          [] -> []
        | x::xs -> if V.equal x v then xs else x :: (aux v xs)
      in let vrt = vertex_list graph
      in Graph(aux v vrt, edges_list graph)

    let fold_v f graph a = List.fold_right f (vertex_list graph) a
    let fold_e f graph a = List.fold_right f (edges_list graph) a
  end

module GR (Ver:VERTEX) (Ed:EDGE with type vertex = Ver.t) : GRAPH =
  struct
    (* typ prezentacji grafu *)

    module E = Ed
    module V = Ver

    type vertex = V.t
    type edge = E.t

    type t = Empty | Graph of vertex list * edge list

    (* Funkcje pomocnicze *)
    let edges_list = function
        Empty -> []
      | Graph(_,y) -> y
    let vertex_list = function
        Empty -> []
      | Graph(x,_) -> x

    (* funkcje wyszukiwania *)
    let mem_v graph vert = 
      let aux vert = function
          Empty -> false
        | Graph(x,y) -> List.mem vert x
      in aux vert graph

    let mem_e graph ed =
      let aux ed = function
          Empty -> false
        | Graph(x,y) -> List.mem ed y
      in aux ed graph

    let mem_e_v graph st en = 
      let rec aux st en = function
          [] -> false
        | x::xs -> if (E.start x) = st && (E.finish x) = en then true else aux st en xs
      in aux st en (edges_list graph)

    let find_e graph st en =
      let aux st en = function
          Empty -> failwith "Not found"
        | Graph(x,y) -> List.find (function ed -> (E.start ed) = st && (E.finish ed) = en) y
      in aux st en graph

    let succ graph vert = 
      let ed = List.filter (function x -> (E.start x) = vert ) (edges_list graph) 
      in List.map (fun x -> E.finish x) ed

    let pred graph vert =  
      let edg = List.filter (function x -> (E.finish x) = vert) (edges_list graph) 
      in List.map (fun x -> E.start x) edg

    let succ_e graph vert = List.filter (function x -> vert = (E.start x)) (edges_list graph)

    let pred_e graph vert = List.filter (function x -> vert = (E.finish x)) (edges_list graph)

    (* funkcje modyfikacji *)
    let empty = Empty
    let add_e graph e = 
      let verts = vertex_list graph 
      in let edgs = edges_list graph 
      in Graph(verts, e::edgs) 

    let add_v graph v = 
      let verts = vertex_list graph
      in let edgs = edges_list graph
      in Graph(v::verts, edgs)

    let rem_e graph e = 
      let rec aux e = function
          [] -> []
        | x::xs -> if E.equal (x, e) then xs else x :: (aux e xs)
      in let edgs = edges_list graph 
      in Graph(vertex_list graph, aux e edgs)
    
    let rem_v graph v =
      let rec aux v = function
          [] -> []
        | x::xs -> if V.equal x v then xs else x :: (aux v xs)
      in let vrt = vertex_list graph
      in Graph(aux v vrt, edges_list graph)

    let fold_v f graph a = List.fold_right f (vertex_list graph) a
    let fold_e f graph a = List.fold_right f (edges_list graph) a
  end

module FS (Gr : GRAPH) =
  struct
    let bfs graph beg = 
      let rec aux graph = function
          [] -> []
        | (x::xs) -> let nex = Gr.succ graph x 
      in Gr.V.label x :: aux graph (xs@nex)
      in aux graph (Gr.succ graph beg)

    let dfs graph beg =
      let rec aux graph = function
          [] -> []
        | (x::xs) -> let nex = Gr.succ graph x
        in Gr.V.label x :: aux graph (nex@xs)
      in aux graph (Gr.succ graph beg)
  end

(* dane pomocnicze do przetestowania *)

module GFS = FS(Graph)

open Graph
open GFS

let v = ["1";"2";"3";"4";"5";"6"]
let ver = List.map (fun x -> Graph.V.create (Graph.V.str_to_label x)) v
let e = [("a", "1", "2"); ("b", "1","5"); ("c", "2","3"); ("d", "2","5"); ("e", "3","4"); ("f", "5","4"); ("g", "4","6")]
let ed = 
  let rec aux = function
      [] -> []
    | ((a,b,c)::xs) -> Graph.E.create (Graph.V.create (Graph.V.str_to_label b)) (Graph.V.create (Graph.V.str_to_label c)) (Graph.E.str_to_label a) :: aux xs
  in aux e

let create x y = 
  let gr = List.fold_left (fun x y -> add_v x y) empty x 
  in List.fold_left (fun x y -> add_e x y) gr y

let graph = create ver ed
let beg = Graph.V.create (Graph.V.str_to_label "1")
