[@deriving show]
type t = Id.t;

let next = (id_gen: t) => (id_gen, id_gen + 1);
