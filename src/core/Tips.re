open Nib;

type t = {l: Nib.t, r: Nib.t};

let mk = (l, r) => {l, r};

let op = mk(Convex, Convex);
let pre = mk(Convex, Concave);
let post = mk(Concave, Convex);
let bin = mk(Concave, Concave);
