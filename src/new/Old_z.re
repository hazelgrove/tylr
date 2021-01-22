open Sexplib.Std;

/*
let enter =
    (d: Direction.t, unzipped)
    : option((int, zipped => ancestor)) => {
  let+ ytile = unzipped;
  ytile
  |> Tile.get(
    fun
    | Paren_body({prefix, suffix, z}) => {
      let zipper = body => L((prefix @ [Op(Paren(body)), ...suffix], z));
      (List.length(prefix) + 1, zipper);
    },
    fun
    | Let_def(p, {prefix, suffix, z}) => {
      let zipper = def => L((prefix @ [Pre(Let(p, def)), ...suffix], z));
      (List.length(prefix) + 1, zipper);
    },
    fun
    | Ap_arg(status, {prefix, suffix, z}) => {
      let zipper = arg => L((prefix @ [Post(Ap(status, arg)), ...suffix], z));
      (List.length(prefix) + 1, zipper);
    },
    () => raise(Void_ybin),
  );
};

let select =
    (d: Direction.t, unzipped)
    : option(((Tessera.t, Tessera.t), ytiles)) => {
  open Tessera;
  let+ ytile = unzipped;
  ytile
  |> Tile.get(
    fun
    | Paren_body(ytiles) => ((Paren_l, Paren_r), ytiles),
    fun
    | Let_def(p, ytiles) => ((Let_eq(p), Let_in), ytiles),
    fun
    | Ap_arg(_) => failwith("todo"),
    () => raise(Void_ybin),
  );
};
*/

module type I = {
  module T: Tile.S;
  module Y: Y.S with module T := T;

  type descendant;
  type ancestor;

  let mk_descendant: T.s => descendant;
  let mk_ancestor: T.s => ancestor;

  let move_in: (Direction.t, T.t, Y.t) => option((int, descendant));
  let move_out: (Direction.t, T.s, Y.tile) => option((int, ancestor));
}

module type S = {
  module T: Tile.S;
  module Y: Y.S with module T := T;

  type unzipped = option(Y.tile);
  type zipped = T.s;
  type t = (zipped, unzipped);

  type descendant;
  type ancestor;

  let move_in: (Direction.t, int, t) => option((int, descendant));
  let move_out: (Direction.t, t) => option((int, ancestor));

  let select
};

module Make = (
  T: Tile.S,
  Y: Y.S with module T := T,
  I: I,
) => {
  type unzipped = option(Y.tile);
  type zipped = T.s;
  type t = (zipped, unzipped);

  let move_in =
      (d: Direction.t, j, (zipped, unzipped) as zipper)
      : option((int, descendant)) => {
    let+ (prefix, tile, suffix) =
      ListUtil.split_nth_opt(d == Left ? j - 1 : j, zipped);
    let moved_past = (d == Left ? j - 1 : j + 1, I.mk_descendant(zipper));
    let entered_j = (ts: list('t)) =>
      d == Left ? List.length(ts) : 0;
    let ye = ZList.{prefix, z: unzipped, suffix};
    switch (I.move_in(d, tile, ye)) {
    | None => moved_past
    | Some(r) => r
    };
  };

  let move_out =
      (d: Direction.t, (zipped, unzipped)): option((int, ancestor)) => {
    let+ ytile = unzipped;
    I.move_out(d, zipped, ytile);
    ytile
    |> Tile.get(
      fun
      | Paren_body({prefix, suffix, z}) =>
        f((prefix, Tile.Op(Paren(zipped)), suffix), z),
      fun
      | Let_def(p, {prefix, suffix, z}) =>
        f((prefix, Tile.Pre(Let(p, def)), suffix), z),
      fun
      | Ap_arg(status, {prefix, suffix, z}) =>
        f((prefix, Tile.Post(Ap(status, arg)), suffix), z),
      () => raise(YExp.Void_bin),
    );
  };
}

module rec Exp: (
  S with
    module T := HExp.T and
    module Y := YExp and
    type descendant = [ | `Exp(Exp.t) | `Pat(Pat.t) ] and
    type ancestor = [ | `Exp(Exp.t) ]
) = {
  type descendant = [ | `Exp(Exp.t) | `Pat(Pat.t) ];
  type ancestor = [ | `Exp(Exp.t) ];

  let move_in = (d: Direction.t, tile: HExp.tile, ye: YExp.t): option((int, descendant)) =>
    tile
    |> Tile.get(
      fun
      | OpHole
      | Num(_)
      | Var(_) => None
      | Paren(body) => Some((entered_j(body), `Exp((body, Paren_body(ye))))),
      fun
      | Lam(p) => Some((entered_j(p), `Pat((p, Lam_pat(ye)))))
      | Let(p, def) =>
        switch (d) {
        | Left => Some((List.length(def), `Exp((def, Let_def(p, ye)))))
        | Right => Some((0, `Pat((p, Let_pat(ye, def)))))
        },
      fun
      | Ap(arg) => Some((entered_j(arg), `Exp((arg, Ap_arg(ye))))),
      fun
      | BinHole
      | Plus(_) => None,
    );

  let move_out = (d: Direction.t, zipped, ytile) => {
    let f = ((prefix, tile, suffix), unzipped) => {
      let j =
        d == Left ? List.length(prefix) : List.length(prefix) + 1;
      let zipped = prefix @ [tile, ...suffix];
      (j, (zipped, unzipped));
    };
    ytile
    |> Tile.get(
      fun
      | Paren_body({prefix, suffix, z}) =>
        f((prefix, Tile.Op(Paren(zipped)), suffix), z),
      fun
      | Let_def(p, {prefix, suffix, z}) =>
        f((prefix, Tile.Pre(Let(p, def)), suffix), z),
      fun
      | Ap_arg(status, {prefix, suffix, z}) =>
        f((prefix, Tile.Post(Ap(status, arg)), suffix), z),
      () => raise(YExp.Void_bin),
    );
  };

  include Make(HExp.T, YExp, Exp);
}

