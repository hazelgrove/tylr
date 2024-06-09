[@deriving (sexp, yojson, ord)]
type t('a) =
  | Root
  | Node('a);

let pp = (pp_a, out) =>
  fun
  | Root => Fmt.pf(out, "ROOT")
  | Node(a) => Fmt.pf(out, "%a", pp_a, a);
let show = pp_a => Fmt.to_to_string(pp(pp_a));

let get = (~root) =>
  fun
  | Root => root
  | Node(a) => a;

let map = f =>
  fun
  | Root => Root
  | Node(a) => Node(f(a));

let to_opt =
  fun
  | Root => None
  | Node(a) => Some(a);

let split =
  fun
  | Root => (Root, Root)
  | Node((l, r)) => (Node(l), Node(r));
