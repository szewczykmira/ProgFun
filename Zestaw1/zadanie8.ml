type 'a stream = int -> 'a;;
let hd : 'a stream -> 'a = fun st -> st 0;;
let tl : 'a stream -> 'a stream = fun st x -> st (x+1);;
let add: int stream -> int -> int stream = fun st i -> (fun x -> (st x) + 1);;
let map : 'a stream -> ('a -> 'b) -> 'b stream = fun st f -> (fun x -> f (st x));;
let map2 : 'a stream -> 'b stream -> ('a -> 'b -> 'c) -> 'c stream = fun sta stb f -> (fun x -> f (sta x) (stb x));;
let reduce : 'a stream -> int -> 'a -> 'a stream = fun st i a -> (fun x -> if x mod i == 0 then a else st x)';;
let take : 'a stream -> int -> 'a stream = fun st i -> (fun x -> st (x*i));;
let rec fold : ('a -> 'b -> 'a) -> 'a -> 'b stream -> 'a stream = fun f a stb -> (fun x -> if x=0 then stb x else fold f (f a (stb 0)) (tl stb) (x-1));;
