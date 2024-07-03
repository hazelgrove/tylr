open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Move.Action.t)
    | Select(Select.Action.t)
    | Delete(Dir.t)
    | Insert(string);
};

let pull_neighbors = ctx => {
  let pull = (from: Dir.t, ctx) =>
    switch (Ctx.pull(~from, ctx)) {
    | None => ("", ctx)
    | Some((tok, ctx)) =>
      Effects.remove(tok);
      (tok.text, ctx);
    };
  let (l, ctx) = pull(L, ctx);
  let (r, ctx) = pull(R, ctx);
  ((l, r), ctx);
};

let insert = (s: string, z: Zipper.t) => {
  List.iter(Effects.remove, Zipper.Cursor.flatten(z.cur));
  let ((l, r), ctx) = pull_neighbors(z.ctx);
  let (cell, ctx) =
    Labeler.label(l ++ s ++ r)
    |> List.fold_left((ctx, tok) => Ctx.mold(ctx, tok), ctx)
    |> Ctx.remold(~fill=Cell.point(Focus));
  Zipper.unzip(cell, ~ctx)
  |> Option.map(Move.move_n(- Utf8.length(r)))
  |> Option.value(~default=Zipper.mk_unroll(R, cell, ~ctx));
};

let delete = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let+ z = Cursor.is_point(z.cur) ? Select.select(d, z) : return(z);
  insert("", z);
};

let perform = (a: Action.t, z: Zipper.t): option(Zipper.t) => {
  Effects.reset();
  switch (a) {
  | Move(a) => Move.perform(a, z)
  | Select(a) => Select.perform(a, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
};
