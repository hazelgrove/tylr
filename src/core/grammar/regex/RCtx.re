open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('a) = list(RFrame.t('a));

let empty: t(_) = [];

// assumes s is already oriented so that nearest element is head
let push_s = (~onto: Dir.t, s: Regex.s(_), ctx: t(_)): t(_) =>
  switch (s) {
  | [] => ctx
  | [_, ..._] as s =>
    switch (onto, ctx) {
    | (L, [Seq_(ls, rs), ...ctx]) => [Seq_(s @ ls, rs), ...ctx]
    | (R, [Seq_(ls, rs), ...ctx]) => [Seq_(ls, s @ rs), ...ctx]
    | (L, _) => [Seq_(s, []), ...ctx]
    | (R, _) => [Seq_([], s), ...ctx]
    }
  };
// note this push call does not bother orienting flattened regex as expected
// by push_s. seems ok for the moment based on current usage, but may need fix.
let push = (~onto, r) => push_s(~onto, Regex.flatten(r));

let is_null = (~atom, ~side: Dir.t) =>
  List.for_all(RFrame.is_null(~atom, ~side));
let nullable = (~atom, ~side: Dir.t) =>
  List.for_all(RFrame.nullable(~atom, ~side));
