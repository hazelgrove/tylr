[@deriving sexp]
type t;

let max: t;
let fact: t;
let ap: t;
let mult: t;
let plus: t;
let prod: t;
let cond: t;
let let_: t;
let min: t;

let compare: (t, t) => int;

let associativity: t => Util.Direction.t;
