type direction =
  | Left
  | Right;

module TileShape = {
  type t =
    | Num(int)
    | Var(Var.t)
    | Paren
    | Lam
    | Ap
    | Ann
    | Plus
    | Arrow;

  module type S = {
    type tile;
    let tile_of_shape: t => option(tile);
  };
};

type t =
  | Mark
  | Move(direction)
  | Edit(edit)
and edit =
  | Delete
  | Construct(TileShape.t);

module Util =
       (
         T: Tiles.TILE,
         Z: ZTiles.ZTILE with type tile = T.t,
         S: TileShape.S with type tile = T.t,
       )
       : {
         type edit_state =
           | Normal({focus: Z.s})
           | Selecting({
               anchor: Z.s,
               focus: Z.s,
             })
           | Restructuring({
               selection: (Z.s, Z.s),
               focus: Z.s,
             });

         let get_focus: edit_state => Z.s;
         let put_focus: (Z.s, edit_state) => edit_state;

         let perform: (t, edit_state) => option(edit_state);
         let perform_edit: (edit, Z.s) => option(Z.s);
       } => {
  module TUtil = Tiles.Util(T);
  module ZUtil = ZTiles.Util(T, Z);

  type edit_state =
    | Normal({focus: Z.s})
    | Selecting({
        anchor: Z.s,
        focus: Z.s,
      })
    | Restructuring({
        selection: (Z.s, Z.s),
        focus: Z.s,
      });

  let get_focus =
    fun
    | Normal({focus})
    | Selecting({focus, _})
    | Restructuring({focus, _}) => focus;

  let put_focus = (focus: Z.s, edit_state) =>
    switch (edit_state) {
    | Normal(_) => Normal({focus: focus})
    | Selecting({anchor, _}) => Selecting({anchor, focus})
    | Restructuring({selection, _}) => Restructuring({selection, focus})
    };

  let opt_map_focus = (f: Z.s => option(Z.s), edit_state) =>
    switch (edit_state) {
    | Normal({focus}) =>
      f(focus) |> Option.map(focus => Normal({focus: focus}))
    | Selecting({anchor, focus}) =>
      f(focus) |> Option.map(focus => Selecting({anchor, focus}))
    | Restructuring({selection, focus}) =>
      f(focus) |> Option.map(focus => Restructuring({selection, focus}))
    };

  let move =
    fun
    | Left => ZUtil.move_left
    | Right => ZUtil.move_right;

  let perform_edit = (edit: edit, ztiles: Z.s): option(Z.s) => {
    let perform =
      switch (edit) {
      | Delete =>
        ZUtil.opt_map((prefix, suffix) =>
          ListUtil.split_last_opt(prefix)
          |> Option.map(((prefix, tile)) => {
               let open_children = List.flatten(T.get_open_children(tile));
               let (prefix, suffix) =
                 TUtil.fix_empty_holes(prefix @ open_children, suffix);
               ZUtil.mk(~prefix, ~suffix, ());
             })
        )
      | Construct(shape) =>
        ZUtil.opt_map((prefix, suffix) =>
          S.tile_of_shape(shape)
          |> Option.map(tile => {
               let (prefix, suffix) =
                 TUtil.fix_empty_holes(prefix @ [tile], suffix);
               ZUtil.mk(~prefix, ~suffix, ());
             })
        )
      };
    perform(ztiles);
  };

  let perform = (a: t, edit_state: edit_state): option(edit_state) =>
    switch (a) {
    | Mark =>
      switch (edit_state) {
      | Normal({focus}) => Some(Selecting({anchor: focus, focus}))
      | Selecting({anchor, focus}) =>
        Some(Restructuring({selection: (anchor, focus), focus}))
      | Restructuring({selection: (l, r), focus}) =>
        ZUtil.restructure(l, r, focus)
        |> Option.map(restructured => Normal({focus: restructured}))
      }
    | Move(direction) =>
      edit_state |> opt_map_focus(ZUtil.opt_map(move(direction)))
    | Edit(edit) =>
      switch (edit_state) {
      | Normal({focus}) =>
        perform_edit(edit, focus)
        |> Option.map(result => Normal({focus: result}))
      | Selecting(_)
      | Restructuring(_) => None
      }
    };
};

module Typ = {
  module TileShape = {
    type tile = HTyp.Tile.t;
    let tile_of_shape: TileShape.t => option(tile) =
      fun
      | Num(_)
      | Lam
      | Ap
      | Plus
      | Var(_)
      | Ann => None
      | Paren => Some(Operand(Paren(HTyp.mk_hole())))
      | Arrow => Some(BinOp(Arrow));
  };

  include Util(HTyp.Tile, ZTyp.ZTile, TileShape);
};

module Pat = {
  module TileShape = {
    type tile = HPat.Tile.t;
    let tile_of_shape: TileShape.t => option(tile) =
      fun
      | Num(_)
      | Lam
      | Ap
      | Plus
      | Arrow => None
      | Var(x) => Some(Operand(Var(x)))
      | Paren => Some(Operand(Paren(HPat.mk_hole())))
      | Ann => Some(PostOp(Ann(NotInHole, HTyp.mk_hole())));
  };

  include Util(HPat.Tile, ZPat.ZTile, TileShape);

  let syn_perform =
      (ctx: Ctx.t, a: t, edit_state): option((edit_state, Type.t, Ctx.t)) =>
    perform(a, edit_state)
    |> Option.map(result => {
         let (zp, ty, ctx) =
           Statics.Pat.syn_fix_holes_z(ctx, get_focus(result));
         (put_focus(zp, edit_state), ty, ctx);
       });
  let ana_perform =
      (ctx: Ctx.t, a: t, edit_state, ty: Type.t)
      : option((edit_state, Ctx.t)) =>
    perform(a, edit_state)
    |> Option.map(result => {
         let (zp, ctx) =
           Statics.Pat.ana_fix_holes_z(ctx, get_focus(result), ty);
         (put_focus(zp, edit_state), ctx);
       });
};

module Exp = {
  module TileShape = {
    type tile = HExp.Tile.t;
    let tile_of_shape: TileShape.t => option(tile) =
      fun
      | Ann
      | Arrow => None
      | Num(n) => Some(Operand(Num(NotInHole, n)))
      | Var(x) => Some(Operand(Var(NotInHole, x)))
      | Paren => Some(Operand(Paren(HExp.mk_hole())))
      | Lam => Some(PreOp(Lam(NotInHole, HPat.mk_hole())))
      | Ap => Some(PostOp(Ap(NotInHole, HExp.mk_hole())))
      | Plus => Some(BinOp(Plus(NotInHole)));
  };

  include Util(HExp.Tile, ZExp.ZTile, TileShape);

  let syn_perform =
      (ctx: Ctx.t, a: t, edit_state): option((edit_state, Type.t)) =>
    perform(a, edit_state)
    |> Option.map(result => {
         let (ze, ty) = Statics.Exp.syn_fix_holes_z(ctx, get_focus(result));
         (put_focus(ze, edit_state), ty);
       });
  let ana_perform =
      (ctx: Ctx.t, a: t, edit_state, ty: Type.t): option(edit_state) =>
    perform(a, edit_state)
    |> Option.map(result => {
         let ze = Statics.Exp.ana_fix_holes_z(ctx, get_focus(result), ty);
         put_focus(ze, edit_state);
       });
};
