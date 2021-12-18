// invariant: affixes are non-empty
// invariant: shards in each pair of affixes assemble to a tile
type t = list((Tile.Frame.t, Tile.Frame.s));

let get = (get_pat, get_exp) =>
  fun
  | Pat(frame) => get_pat(frame)
  | Exp(frame) => get_exp(frame);

let get_pat =
  fun
  | Pat(frame) => Some(frame)
  | Exp(_) => None;
let get_exp =
  fun
  | Exp(frame) => Some(frame)
  | Pat(_) => None;

let sort =
  fun
  | Pat(_) => Sort.Pat
  | Exp(_) => Exp;

let is_root =
  fun
  | Exp((_, Root)) => true
  | _ => false;
