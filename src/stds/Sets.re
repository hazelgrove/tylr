module type OrderedShowType = {
  include Set.OrderedType;

  let pp: (Format.formatter, t) => unit;
};

module type OrderedSexpType = {
  include Set.OrderedType;

  let sexp_of_t: t => Sexplib.Sexp.t;
  let t_of_sexp: Sexplib.Sexp.t => t;
};

module type OrderedYojsonType = {
  include Set.OrderedType;

  let yojson_of_t: t => Yojson.Safe.t;
  let t_of_yojson: Yojson.Safe.t => t;
};

module type OrderedType = {
  include OrderedShowType;
  include OrderedSexpType with type t := t;
  include OrderedYojsonType with type t := t;
};

module type ShowS = {
  include Set.S;

  let pp: (Format.formatter, t) => unit;
};

module type SexpS = {
  include Set.S;

  let sexp_of_t: t => Sexplib.Sexp.t;
  let t_of_sexp: Sexplib.Sexp.t => t;
};

module type YojsonS = {
  include Set.S;

  let yojson_of_t: t => Yojson.Safe.t;
  let t_of_yojson: Yojson.Safe.t => t;
};

module type S = {
  include ShowS;
  include SexpS with type t := t and type elt := elt;
  include YojsonS with type t := t and type elt := elt;
};

module MakeShowFor = (O: OrderedShowType, S: Set.S with type elt = O.t) => {
  let pp = (f, set) =>
    S.iter(k => Format.fprintf(f, "%a@\n", O.pp, k), set);
};

module MakeShow = (O: OrderedShowType) : (ShowS with type elt = O.t) => {
  module M = Set.Make(O);

  include M;
  include MakeShowFor(O, M);
};

module MakeSexpFor = (O: OrderedSexpType, S: Set.S with type elt = O.t) => {
  open Sexplib.Std;

  [@deriving sexp]
  type elt = O.t;

  let sexp_of_t = set => set |> S.elements |> sexp_of_list(sexp_of_elt);
  let t_of_sexp = sexp =>
    sexp |> list_of_sexp(elt_of_sexp) |> List.to_seq |> S.of_seq;
};

module MakeSexp = (O: OrderedSexpType) : (SexpS with type elt = O.t) => {
  module M = Set.Make(O);

  include M;
  include MakeSexpFor(O, M);
};

module MakeYojsonFor = (O: OrderedYojsonType, S: Set.S with type elt = O.t) => {
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

  [@deriving yojson]
  type elt = O.t;

  let yojson_of_t = set => set |> S.elements |> yojson_of_list(yojson_of_elt);
  let t_of_yojson = yojson =>
    yojson |> list_of_yojson(elt_of_yojson) |> List.to_seq |> S.of_seq;
};

module MakeYojson = (O: OrderedYojsonType) : (YojsonS with type elt = O.t) => {
  module M = Set.Make(O);

  include M;
  include MakeYojsonFor(O, M);
};

module MakeFor = (O: OrderedType, M: Set.S with type elt = O.t) => {
  include MakeShowFor(O, M);
  include MakeSexpFor(O, M);
  include MakeYojsonFor(O, M);
};

module Make = (O: OrderedType) : (S with type elt = O.t) => {
  module M = Set.Make(O);

  include M;
  include MakeFor(O, M);
};
