open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

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
let push = (~onto, r) =>
  Regex.flatten(r) |> Dir.pick(onto, (List.rev, Fun.id)) |> push_s(~onto);

let is_null = (~atom, ~side: Dir.t) =>
  List.for_all(RFrame.is_null(~atom, ~side));
let nullable = (~atom, ~side: Dir.t) =>
  List.for_all(RFrame.nullable(~atom, ~side));
