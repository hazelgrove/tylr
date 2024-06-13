[@deriving (show({with_path: false}), sexp, yojson)]
type t('eq, 'neq) =
  | Eq('eq)
  | Neq('neq);

let eq = eq => Eq(eq);
let neq = neq => Neq(neq);

let is_eq =
  fun
  | Eq(eq) => Some(eq)
  | Neq(_) => None;
let is_neq =
  fun
  | Eq(_) => None
  | Neq(neq) => Some(neq);

let map = (~eq, ~neq) =>
  fun
  | Eq(x) => Eq(eq(x))
  | Neq(x) => Neq(neq(x));
