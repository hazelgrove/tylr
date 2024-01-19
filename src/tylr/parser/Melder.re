open Util;

module Melded = {
  type t =
    | Eq(Wald.t)
    | Neq(Slope.t);

  let mk_eq = (src: Wald.t, bake: Bake.t, dst: Wald.t) =>
    failwith("todo");
  let mk_neq = (bake: Bake.t, dst: Wald.t) =>
    failwith("todo");
  let mk = (src: Wald.t, bake: Bake.t, dst: Wald.t) =>
    Bake.is_eq(bake) ? mk_eq(src, bake, dst) : mk_neq(bake, dst);
};

module Wald = {
  let rec meld =
          (~from: Dir.t, l: Wald.t, ~fill=[], r: Wald.t)
          : option(Melded.t) => {
    let (src, dst) = Dir.choose(from, l, r);
    let fill = Dir.choose(from, fill, List.rev(fill));
    let rec go = (src, fill) => {
      open OptUtil.Syntax;
      let/ () =
        // try removing ghost
        switch (Wald.unlink(src)) {
        | Ok((hd, cell, tl)) when Token.is_ghost(hd) =>
          let fill = Option.to_list(cell.content) @ fill;
          switch (go(tl, fill)) {
          // require eq match further in to accept removing hd
          | Some(Eq(_)) as r =>
            Effect.perform(Remove(hd));
            r;
          | _ => None
          };
        | _ => None
        };
      let+ bake =
        walk(~from, Wald.face(src).mtrl, Wald.face(dst).mtrl)
        |> Oblig.Delta.minimize(Baker.bake(~from, ~fill));
      Melded.mk(src, bake, dst);
    };
    go(src, fill);
  };

  let meld_eq = (~from: Dir.t, l: Wald.t, ~fill=[], r: Wald.t) =>
    switch (meld(~from, l, ~fill, r)) {
    | Some(Eq(w)) => Some(w)
    | _ => None
    };
};

module Terr = {
  module R = {
    include Terr.R;

    let connect = (terr: Terr.R.t, bake) =>
      bake
      |> Chain.fold_left(
           cell => Meld.M(terr.cell, terr.wald, cell),
           (meld, tok, cell) => Meld.link(cell, tok, meld),
         )
      |> Meld.rev;

    let round = (~fill=[], terr: Terr.R.t) => {
      let bake = Baker.bake(~from=L);
      let exited = Walker.exit(R, Terr.face(terr));
      switch (Oblig.Delta.minimize(bake(~fill), exited)) {
      | Some(baked) => [connect(terr, baked)]
      | None =>
        let exited =
          ListUtil.hd_opt(exited)
          |> OptUtil.get_or_fail("bug: expected at least one exit");
        let baked =
          bake(exited)
          |> OptUtil.get_or_fail(
               "bug: bake expected to succeed if no fill required",
             );
        [connect(terr, baked), ...fill];
      };
    };
  };
};

module Slope = {
  module Dn = {
    let rec meld =
            (~top=Bound.Root, dn: Slope.Dn.t, ~fill=[], w: Wald.t)
            : Result.t(Slope.Dn.t, list(Meld.t)) =>
      switch (dn) {
      | [] =>
        open Result.Syntax;
        let* walked =
          Walker.walk_neq(~from=L, Root, Wald.face(w).mtrl)
          |> Result.of_option(~error=fill);
        let+ baked = bake(~fill, walked) |> Result.of_option(~error=fill);
        connect([], baked, w);
      | [hd, ...tl] =>
        switch (Wald.meld(~from=L, hd.wald, ~fill, w)) {
        | None => meld(~top, tl, ~fill=Terr.R.round(hd, ~fill), w)
        | Some(Eq(wald)) => Ok([{...hd, wald}, ...tl])
        | Some(Neq(dn')) => Slope.cat(dn', dn)
        }
      };
  };
};

module Ctx = {
  open OptUtil.Syntax;

  let push =
      (~onto: Dir.t, w: Wald.t, ~fill=[], ctx: Ctx.t): option(Ctx.t) =>
    switch (onto, Ctx.unlink(ctx)) {
    | (L, Error((dn, up))) =>
      let+ dn = Slope.Dn.push(dn, ~fill, w);
      Ctx.unit((dn, up));
    | (R, Error((dn, up))) =>
      let+ up = Slope.Up.push(w, ~fill, dn);
      Ctx.unit((dn, up));
    | (L, Ok(((dn, up), (l, r), ctx))) =>
      switch (Slope.Dn.push(~top=Node(l.wald), dn, ~fill, w)) {
      | Ok(dn) => Some(Ctx.link((dn, up), (l, r), ctx))
      | Error(fill) =>
        let+ wald = Wald.meld_eq(l.wald, ~fill, w);
        let (dn, up) = ([{...l, wald}], up @ [r]);
        Ctx.map_fst(Frame.Open.cat((dn, up)), ctx);
      }
    | (R, Ok(((dn, up), (l, r), ctx))) =>
      switch (Slope.Up.push(w, ~fill, up, ~top=Node(r.wald))) {
      | Ok(up) => Some(Ctx.link((dn, up), (l, r), ctx))
      | Error(fill) =>
        let+ wald = Wald.meld_eq(w, ~fill, r.wald);
        let (dn, up) = (dn @ [l], [{...r, wald}]);
        Ctx.map_fst(Frame.Open.cat((dn, up)), ctx);
      }
    };

  let bridge = (~sel=Ziggurat.empty, well: Stepwell.t): Stepwell.t => {
    print_endline("Stepwell.assemble");
    let (pre, suf) = get_slopes(well);
    // separate siblings that belong to the selection
    let (pre_lt_sel, pre_geq_sel) = Ziggurat.split_lt(pre, sel);
    let (sel_leq_suf, sel_gt_suf) = Ziggurat.split_gt(sel, suf);
    well
    |> Stepwell.put_slopes(Slopes.empty)
    |> bridge_slopes((pre_lt_sel, sel_gt_suf))
    |> push_slopes((pre_geq_sel, sel_leq_suf));
  };

  let pull_lexeme = (~char=false, ~from: Dir.t, well: Stepwell.t) =>
    switch (Slopes.pull_lexeme(~char, ~from, Stepwell.get_slopes(well))) {
    | Some((a, sib)) => Some((a, Stepwell.put_slopes(sib, well)))
    | None =>
      open OptUtil.Syntax;
      let+ (slopes, bridge, well) = Chain.unlink(well);
      let (lx, slopes') = Bridge.pull_lexeme(~char, ~from, bridge);
      let well =
        well |> push_slopes(Slopes.cat(slopes, slopes')) |> assemble;
      (lx, well);
    };
};