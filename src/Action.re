type tile_shape =
  | Num(int)
  | Var(Var.t)
  | Paren
  | If
  | Let
  | Ann
  | Plus
  | Times
  | Eq;

type t =
  | Delete
  | Construct(tile_shape);

module Exp = {
  let tile_of_shape: tile_shape => HExp.Tile.t =
    fun
    | Num(n) => Num(n)
    | Var(x) => Var(x)
    | Paren => Paren(HExp.Term.EHole)
    | If => If(HExp.Term.EHole, HExp.Term.EHole)
    | Let => Let(HPat.Term.EHole, HExp.Term.EHole)
    | Ann => Ann(HTyp.Term.EHole)
    | Plus => Plus
    | Times => Times
    | Eq => Eq;

  let rec syn_perform =
          (ctx: Ctx.t, a: t, ze: ZExp.t): option((ZExp.t, HTyp.Term.t)) =>
    switch (ze) {
    | Z(prefix, suffix) =>
      switch (a) {
      | Delete =>
        switch (prefix) {
        | [] => None
        | [t, ...prefix] =>
          let open_children_tiles =
            t
            |> HExp.Tile.get_open_children
            |> List.map(HExp.unparse)
            |> List.flatten;
          let (tiles, n) = {
            let (fixed_prefix, fixed_suffix) =
              HExp.fix_empty_holes(prefix @ open_children_tiles, suffix);
            (fixed_prefix @ fixed_suffix, List.length(fixed_prefix));
          };
          let ((new_prefix, new_suffix), ty) = {
            let (new_e, ty) =
              Statics.Exp.syn_fix_holes(ctx, HExp.parse(tiles));
            (ListUtil.split_n(n, HExp.unparse(new_e)), ty);
          };
          Some((Z(new_prefix, new_suffix), ty));
        }
      | Construct(s) =>
        let (tiles, n) = {
          let (fixed_prefix, fixed_suffix) =
            HExp.fix_empty_holes([tile_of_shape(s), ...prefix], suffix);
          (fixed_prefix @ fixed_suffix, List.length(fixed_prefix));
        };
        let ((new_prefix, new_suffix), ty) = {
          let (new_e, ty) =
            Statics.Exp.syn_fix_holes(ctx, HExp.parse(tiles));
          (ListUtil.split_n(n, HExp.unparse(new_e)), ty);
        };
        Some((Z(new_prefix, new_suffix), ty));
      }
    | ParenZ(zbody) =>
      syn_perform(ctx, a, zbody)
      |> Option.map(((zbody, ty)) => (ParenZ(zbody), ty))
    };
};
