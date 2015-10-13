let zlozenie f g = function x -> f (g x);;
let rec iteracja num f = if num > 0 then zlozenie (iteracja (num-1) f) f else (fun z -> z);;
let m a b = iter (a-1) ((+) b) b;;
let mult a b = a*b;;
let pow a b = iter a (mult b) 1;;
