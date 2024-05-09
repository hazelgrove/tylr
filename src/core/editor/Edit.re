open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Move.Action.t)
    | Select(Select.Action.t)
    | Delete(Dir.t)
    | Insert(string);
};

// d is side of cleared focus contents the cursor should end up
let clear_focus = (d: Dir.t, z: Zipper.t) =>
  switch (z.cur) {
  | Point () => z.ctx
  | Select((_, zigg)) =>
    let onto = Dir.toggle(d);
    Melder.Ctx.push_zigg(~onto, zigg, z.ctx);
  };

let pull_neighbors = ctx => {
  let pull = (from: Dir.t, ctx) =>
    switch (Melder.Ctx.pull(~from, ctx)) {
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
  let ctx = clear_focus(L, z);
  let ((l, r), ctx) = pull_neighbors(ctx);
  Labeler.label(l ++ s ++ r)
  |> List.fold_left((ctx, tok) => Molder.mold(ctx, tok), ctx)
  |> Molder.remold(~fill=Fill.unit(Cell.point()))
  |> Zipper.mk
  |> Zipper.move_to_cursor;
};

let delete = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let+ z = Cursor.is_point(z.cur) ? Select.select(d, z) : return(z);
  insert("", z);
};

let perform = (a: Action.t, z: Zipper.t): option(Zipper.t) =>
  switch (a) {
  | Move(a) => Move.perform(a, z)
  | Select(a) => Select.perform(a, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
