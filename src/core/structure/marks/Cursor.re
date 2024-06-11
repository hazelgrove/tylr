[@deriving (show({with_path: false}), sexp, yojson, hash)]
type t('point, 'select) =
  | Point('point)
  | Select('select);

let point = p => Point(p);
let select = s => Select(s);

let get = (f_p, f_s) =>
  fun
  | Point(p) => f_p(p)
  | Select(s) => f_s(s);
let map = (f_p, f_s) =>
  fun
  | Point(p) => Point(f_p(p))
  | Select(s) => Select(f_s(s));

let is_point =
  fun
  | Point(_) => true
  | Select(_) => false;
let get_point =
  fun
  | Point(p) => Some(p)
  | Select(_) => None;
