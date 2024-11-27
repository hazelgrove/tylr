open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
// open Stds;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Tab(Dir.t)
  | Move(Move.t)
  | Select(Select.t)
  | Insert(string)
  | Delete(Dir.t);

let perform = (a: t, z: Zipper.t): option(Zipper.t) => {
  Effects.reset();
  switch (a) {
  | Tab(d) => Tab.perform(d, z)
  | Move(a) => Move.perform(a, z)
  | Select(a) => Select.perform(a, z)
  | Insert(s) =>
    s
    |> Labeler.label
    |> List.map((x: Token.Unmolded.t) => x.text)
    |> List.fold_left((x, y) => Modify.insert(y, x), z)
    |> Option.some
  | Delete(d) => Modify.delete(d, z)
  };
};
