open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = option('a);

let of_bool = b => b ? Some() : None;

let get = (if_none, o) =>
  switch (o) {
  | None => if_none()
  | Some(a) => a
  };
let get_fail = s => get(() => failwith(s));
let get_exn = e => get(() => raise(e));

let map2 = (f, o1, o2) =>
  Base.Option.both(o1, o2) |> Option.map(((v1, v2)) => f(v1, v2));

let for_all = (l: list(option('a))): option(list('a)) =>
  List.fold_right(map2((x, xs) => [x, ...xs]), l, Some([]));

let merge = Base.Option.merge;

let bind = Base.Option.bind;

module Syntax = {
  let ( let* ) = Option.bind;
  let (let+) = (o, f) => Option.map(f, o);
  let (and+) = Base.Option.both;
  let return = Option.some;
  let (let/) = (o, f) =>
    switch (o) {
    | Some(_) => o
    | None => f()
    };
  let (let-) = (o, f) =>
    switch (o) {
    | Some(a) => a
    | None => f()
    };
};
