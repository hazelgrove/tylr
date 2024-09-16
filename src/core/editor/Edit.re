open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
// open Stds;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Move(Move.t)
  | Select(Select.t)
  | Insert(string)
  | Delete(Dir.t);

let perform = (a: t, z: Zipper.t): option(Zipper.t) => {
  Effects.reset();
  switch (a) {
  | Move(a) => Move.perform(a, z)
  | Select(a) => Select.perform(a, z)
  | Insert(s) => Some(Modify.insert(s, z))
  | Delete(d) => Modify.delete(d, z)
  };
};
