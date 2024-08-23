[@deriving (sexp, yojson, ord)]
type t('a) =
  | Root
  | Node('a);

let node = a => Node(a);

let pp = (pp_a, out) =>
  fun
  | Root => Fmt.pf(out, "ROOT")
  | Node(a) => Fmt.pf(out, "%a", pp_a, a);
let show = pp_a => Fmt.to_to_string(pp(pp_a));

let get = (~root) =>
  fun
  | Root => root
  | Node(a) => a;
let get_exn =
  fun
  | Root => raise(Invalid_argument("Bound.get_exn"))
  | Node(a) => a;

let map = f =>
  fun
  | Root => Root
  | Node(a) => Node(f(a));

let to_opt =
  fun
  | Root => None
  | Node(a) => Some(a);

let to_list =
  fun
  | Root => []
  | Node(a) => [a];

let split =
  fun
  | Root => (Root, Root)
  | Node((l, r)) => (Node(l), Node(r));
