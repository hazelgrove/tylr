open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list((int, int));

let incr = (~side: option(Dir.t)=?, n: int, eqs: t) =>
  eqs
  |> List.map(((l, r)) =>
       switch (side) {
       | None => (l + n, r + n)
       | Some(L) => (l + n, r)
       | Some(R) => (l, r + n)
       }
     );
