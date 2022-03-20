[@deriving show]
type t = Id.t;

let init = 0;
let next = id_gen => (id_gen, id_gen + 1);
