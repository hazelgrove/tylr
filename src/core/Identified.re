[@deriving sexp]
type t('a) = (Id.t, 'a);

let get = (f: 'a => 'b, (_, a): t('a)): 'b => f(a);
