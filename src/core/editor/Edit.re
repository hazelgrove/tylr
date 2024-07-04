open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Move.Action.t)
    | Select(Select.Action.t)
    | Insert(string)
    | Delete(Dir.t);
};

let perform = (a: Action.t, z: Zipper.t): option(Zipper.t) => {
  Effects.reset();
  switch (a) {
  | Move(a) => Move.perform(a, z)
  | Select(a) => Select.perform(a, z)
  | Insert(s) => Some(Insert.perform(s, z))
  | Delete(d) =>
    open Options.Syntax;
    let+ z = Cursor.is_point(z.cur) ? Select.select(d, z) : return(z);
    Insert.perform("", z);
  };
};
