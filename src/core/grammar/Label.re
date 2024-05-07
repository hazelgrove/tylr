open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

// token classes determined by lexer.mll
// todo: add operator class
[@deriving (sexp, yojson, ord)]
type t =
  | Const(Padding.t, string)
  | Id_lower
  | Id_upper
  | Int_lit
  | Float_lit;

let pp = out =>
  fun
  | Id_lower => Fmt.pf(out, "Id_lower")
  | Id_upper => Fmt.pf(out, "Id_upper")
  | Int_lit => Fmt.pf(out, "Int_lit")
  | Float_lit => Fmt.pf(out, "Float_lit")
  | Const(_, s) => Fmt.pf(out, "\'%s\'", s);
let show = Fmt.to_to_string(pp);

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Ord);
module Set = Set.Make(Ord);

let const = (~padding=Padding.none, text) => Const(padding, text);

let padding =
  fun
  | Const(padding, _) => padding
  | _ => Padding.none;

let is_empty =
  fun
  | Const(_, "") => true
  | _ => false;

let is_const =
  fun
  | Const(_) => true
  | _ => false;

let is_complete = text =>
  fun
  | Id_lower
  | Id_upper
  | Int_lit
  | Float_lit =>
    // assuming text is consistent with lbl
    true
  | Const(_, c) => String.equal(c, text);
