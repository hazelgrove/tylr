open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

exception Invalid;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = list(Step.t);
  let compare = List.compare(Step.compare);
};
include Base;

module Map = {
  include Maps.Make(Base);
  let to_list = bindings;
  let of_list = bindings => of_seq(List.to_seq(bindings));
};

module Head = {
  type t('err) = Result.t(Step.t, 'err);
  let map = f => Result.map_error(~f);
  let get = f =>
    fun
    | Ok(step) => step
    | Error(err) => f(err);
};

let empty = [];
let of_step = n => [n];
let cons = List.cons;
let peel = n =>
  fun
  | [hd, ...tl] when n == hd => Some(tl)
  | _ => None;
let hd: t => Head.t(unit) =
  fun
  | [hd, ..._] => Ok(hd)
  | [] => Error();

module Range = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = (Base.t, Base.t);
  let is_empty = ((l, r): t) => compare(l, r) == 0;
  let map = Tuples.map2;
  let cons = n => map(cons(n));
  let peel = (n, (l, r): t) => {
    open Options.Syntax;
    let+ l = peel(n, l)
    and+ r = peel(n, r);
    (l, r);
  };
  let hd =
    fun
    | ([hd_l, ..._], [hd_r, ..._]) when hd_l == hd_r => Ok(hd_l)
    | sel => Error(sel);
};
