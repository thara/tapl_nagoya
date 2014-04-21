type info = string

type term =
    TmX of info
  | TmLA of info * info
  | TmFA of info * info

(* Church ブール値 *)
let tru = fun t -> fun f -> t
let fls = fun t -> fun f -> f

let test = fun l -> fun m -> fun n -> l m n

let and' = fun b -> fun c -> b c fls
let or' = fun b -> fun c -> b tru c tru fls
let not' = fun b -> b fls tru

(* 二つ組 *)
let pair = fun f -> fun s -> fun b -> b f s
let fst p = p tru
let snd = fun p -> p fls

(* Church数 *)
let c0 = fun s -> fun z -> z
let c1 = fun s -> fun z -> s z
let c2 = fun s -> fun z -> s (s z)
let c3 = fun s -> fun z -> s (s (s z))

let scc = fun n -> fun s -> fun z -> s (n s z)
let plus = fun m -> fun n -> fun s -> fun z -> m s (n s z)
let times = fun m -> fun n -> m (plus n) c0

let iszro = fun m -> m (fun x -> fls) tru

let zz = fun m -> (pair c0 c0) m
let ss = fun p -> pair (snd p) (plus c1 (snd p))
let prd m = fst (m ss zz)

let subtract = fun m -> fun n -> n prd m

let equal m n = and' (iszro (m prd n)) (iszro (n prd m))
