include Map.Make(Var);
type nonrec t = t(Type.t);

// TODO
let sexp_of_t = _ => Sexplib.Sexp.Atom("ctx");
let t_of_sexp = _ => failwith("Ctx.t_of_sexp");

// favors second arg when there are overlapping bindings
let union: (t, t) => t = union((_, _, ty) => Some(ty));
