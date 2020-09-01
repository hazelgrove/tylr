type type_mode =
  | Syn
  | Ana(HTyp.t);

module Pat = {
  let rec syn =
          (ctx: Ctx.t, (skel, tiles) as p: HPat.t): option((Typ.t, Ctx.t)) =>
    switch (skel) {
    | Operand(n) =>
      switch (Tiles.get_operand(n, tiles)) {
      | OperandHole => Some((Typ.Hole, ctx))
      | Var(x) => Some((Typ.Hole, Ctx.add(x, Typ.Hole, ctx)))
      | Paren(body) => syn(ctx, body)
      }
    | PreOp(_) => raise(HPat.Void_PreOp)
    | PostOp(skel, n) =>
      switch (Tiles.get_postop(n, tiles)) {
      | Ann(status, ann) =>
        let subj = (skel, tiles);
        switch (status) {
        | NotInHole =>
          let ty = HTyp.contract(ann);
          ana(ctx, subj, ty) |> Option.map(ctx => (ty, ctx));
        | InHole =>
          syn(ctx, HPat.set_hole_status(NotInHole, p))
          |> Option.map(((_, ctx)) => (Typ.Hole, ctx))
        };
      }
    | BinOp(skel1, n, skel2) =>
      switch (Tiles.get_binop(n, tiles)) {
      | OperatorHole =>
        let p1 = (skel1, tiles);
        let p2 = (skel2, tiles);
        switch (syn(ctx, p1)) {
        | None => None
        | Some((_, ctx)) =>
          syn(ctx, p2) |> Option.map(((_, ctx)) => (Typ.Hole, ctx))
        };
      }
    }
  and ana =
      (ctx: Ctx.t, (skel, tiles) as p: HPat.t, ty: Typ.t): option(Ctx.t) => {
    let subsume = () =>
      switch (syn(ctx, p)) {
      | None => None
      | Some((ty', ctx)) => Typ.consistent(ty, ty') ? Some(ctx) : None
      };
    switch (skel) {
    | Operand(n) =>
      switch (Tiles.get_operand(n, tiles)) {
      | OperandHole => subsume()
      | Var(x) => Some(Ctx.add(x, ty, ctx))
      | Paren(body) => ana(ctx, body, ty)
      }
    | PreOp(_) => raise(HPat.Void_PreOp)
    | PostOp(_, n) =>
      switch (Tiles.get_postop(n, tiles)) {
      | Ann(_) => subsume()
      }
    | BinOp(_, n, _) =>
      switch (Tiles.get_binop(n, tiles)) {
      | OperatorHole => subsume()
      }
    };
  };

  let rec syn_fix_holes =
          (ctx: Ctx.t, (skel, tiles): HPat.t): (HPat.t, Typ.t, Ctx.t) => {
    let (tiles, ty, ctx) =
      switch (skel) {
      | Operand(n) =>
        switch (Tiles.get_operand(n, tiles)) {
        | OperandHole => (tiles, Typ.Hole, ctx)
        | Var(x) =>
          let ctx = Ctx.add(x, Typ.Hole, ctx);
          (tiles, Hole, ctx);
        | Paren(body) =>
          let (body, ty, ctx) = syn_fix_holes(ctx, body);
          let tiles =
            Tiles.put_nth(n, Tile.Operand(HPat.Paren(body)), tiles);
          (tiles, ty, ctx);
        }
      | PreOp(_) => raise(HPat.Void_PreOp)
      | PostOp(skel, n) =>
        switch (Tiles.get_postop(n, tiles)) {
        | Ann(_, ann) =>
          let ty = HTyp.contract(ann);
          let ((_, tiles), ctx) = {
            let subj = (skel, tiles);
            ana_fix_holes(ctx, subj, ty);
          };
          let tiles =
            Tiles.put_nth(n, Tile.PostOp(HPat.Ann(NotInHole, ann)), tiles);
          (tiles, ty, ctx);
        }
      | BinOp(skel1, n, skel2) =>
        switch (Tiles.get_binop(n, tiles)) {
        | OperatorHole =>
          let ((_, tiles), _, ctx) = syn_fix_holes(ctx, (skel1, tiles));
          let ((_, tiles), _, ctx) = syn_fix_holes(ctx, (skel2, tiles));
          (tiles, Typ.Hole, ctx);
        }
      };
    ((skel, tiles), ty, ctx);
  }
  and ana_fix_holes =
      (ctx: Ctx.t, (skel, tiles) as p: HPat.t, ty: Typ.t): (HPat.t, Ctx.t) => {
    let subsume = () => {
      let (p, ty', ctx) = syn_fix_holes(ctx, p);
      let (_, tiles) =
        Typ.consistent(ty, ty') ? p : HPat.set_hole_status(InHole, p);
      (tiles, ctx);
    };
    let (tiles, ctx) =
      switch (skel) {
      | Operand(n) =>
        switch (Tiles.get_operand(n, tiles)) {
        | OperandHole => subsume()
        | Var(x) => (tiles, Ctx.add(x, ty, ctx))
        | Paren(body) =>
          let (body, ctx) = ana_fix_holes(ctx, body, ty);
          let tiles =
            Tiles.put_nth(n, Tile.Operand(HPat.Paren(body)), tiles);
          (tiles, ctx);
        }
      | PreOp(_) => raise(HPat.Void_PreOp)
      | PostOp(_, n) =>
        switch (Tiles.get_postop(n, tiles)) {
        | Ann(_) => subsume()
        }
      | BinOp(_, n, _) =>
        switch (Tiles.get_binop(n, tiles)) {
        | OperatorHole => subsume()
        }
      };
    ((skel, tiles), ctx);
  };
  /*
   let rec syn_nth_type_mode =
           (ctx: Ctx.t, n: int, p: HPat.t): option(type_mode) =>
     switch (p) {
     | OperandHole
     | Var(_)
     | Paren(_) => n == 0 ? Some(Syn) : None
     | Ann(_, subj, ann) =>
       let m = List.length(HPat.unparse(subj));
       n < m ? ana_nth_type_mode(ctx, n, subj, ann) : Some(Syn);
     | OperatorHole(l, r) =>
       let m = List.length(HPat.unparse(l));
       if (n < m) {
         syn_nth_type_mode(ctx, n, l);
       } else if (n > m) {
         syn_nth_type_mode(ctx, n - (m + 1), r);
       } else {
         Some(Syn);
       };
     }
   and ana_nth_type_mode =
       (ctx: Ctx.t, n: int, p: HPat.t, ty: HTyp.t): option(type_mode) =>
     switch (p) {
     | OperandHole
     | Var(_)
     | Paren(_) => n == 0 ? Some(Ana(ty)) : None
     | Ann(_)
     | OperatorHole(_) =>
       // subsumption
       syn_nth_type_mode(ctx, n, p)
     };
   */
};

module Exp = {
  /*
   let rec syn = (ctx: Ctx.t, e: HExp.t): option(HTyp.t) =>
     switch (e) {
     | OperandHole
     | Var(InHole, _)
     | Num(InHole, _) => Some(OperandHole)
     | Var(NotInHole, x) => Ctx.find_opt(x, ctx)
     | Num(NotInHole, _) => Some(Num)
     | Paren(body) => syn(ctx, body)
     | Lam(status, p, body) =>
       switch (status) {
       | InHole =>
         syn(ctx, Lam(NotInHole, p, body))
         |> Option.map(_ => HTyp.OperandHole)
       | NotInHole =>
         switch (Pat.syn(ctx, p)) {
         | None => None
         | Some((ty1, ctx)) =>
           syn(ctx, body) |> Option.map(ty2 => HTyp.Arrow(ty1, ty2))
         }
       }
     | Plus(status, e1, e2) =>
       switch (status) {
       | InHole =>
         syn(ctx, Plus(NotInHole, e1, e2))
         |> Option.map(_ => HTyp.OperandHole)
       | NotInHole =>
         ana(ctx, e1, HTyp.Num) && ana(ctx, e2, HTyp.Num)
           ? Some(HTyp.Num) : None
       }
     | Ap(status, fn, arg) =>
       switch (status) {
       | InHole =>
         syn(ctx, Ap(NotInHole, fn, arg)) |> Option.map(_ => HTyp.OperandHole)
       | NotInHole =>
         switch (syn(ctx, fn)) {
         | None => None
         | Some(fn_ty) =>
           switch (HTyp.matched_arrow(fn_ty)) {
           | None => None
           | Some((ty1, ty2)) => ana(ctx, arg, ty1) ? Some(ty2) : None
           }
         }
       }
     | OperatorHole(e1, e2) =>
       OptUtil.map2((_, _) => HTyp.OperandHole, syn(ctx, e1), syn(ctx, e2))
     }
   and ana = (ctx: Ctx.t, e: HExp.t, ty: HTyp.t): bool =>
     switch (e) {
     | OperandHole
     | Num(_)
     | Var(_)
     | Plus(_)
     | Ap(_)
     | OperatorHole(_) =>
       switch (syn(ctx, e)) {
       | None => false
       | Some(ty') => HTyp.consistent(ty, ty')
       }
     | Paren(e) => ana(ctx, e, ty)
     | Lam(InHole, p, body) =>
       Option.is_some(syn(ctx, Lam(NotInHole, p, body)))
     | Lam(NotInHole, p, body) =>
       switch (HTyp.matched_arrow(ty)) {
       | None => false
       | Some((ty1, ty2)) =>
         switch (Pat.ana(ctx, p, ty1)) {
         | None => false
         | Some(ctx) => ana(ctx, body, ty2)
         }
       }
     };
   */

  let rec syn_fix_holes =
          (ctx: Ctx.t, (skel, tiles): HExp.t): (HExp.t, Typ.t) => {
    let (tiles, ty) =
      switch (skel) {
      | Operand(n) =>
        switch (Tiles.get_operand(n, tiles)) {
        | OperandHole => (tiles, Typ.Hole)
        | Num(_, m) =>
          let tiles =
            Tiles.put_nth(n, Tile.Operand(HExp.Num(NotInHole, m)), tiles);
          (tiles, Typ.Num);
        | Var(_, x) =>
          let (tile, ty) =
            switch (Ctx.find_opt(x, ctx)) {
            | None => (Tile.Operand(HExp.Var(InHole, x)), Typ.Hole)
            | Some(ty) => (Tile.Operand(HExp.Var(NotInHole, x)), ty)
            };
          let tiles = Tiles.put_nth(n, tile, tiles);
          (tiles, ty);
        | Paren(body) =>
          let (body, ty) = syn_fix_holes(ctx, body);
          let tiles =
            Tiles.put_nth(n, Tile.Operand(HExp.Paren(body)), tiles);
          (tiles, ty);
        }
      | PreOp(n, skel) =>
        switch (Tiles.get_preop(n, tiles)) {
        | Lam(_, p) =>
          let (p, ty1, ctx) = Pat.syn_fix_holes(ctx, p);
          let ((_, tiles), ty2) = {
            let body = (skel, tiles);
            syn_fix_holes(ctx, body);
          };
          let tiles =
            Tiles.put_nth(n, Tile.PreOp(HExp.Lam(NotInHole, p)), tiles);
          (tiles, Arrow(ty1, ty2));
        }
      | PostOp(skel, n) =>
        switch (Tiles.get_postop(n, tiles)) {
        | Ap(_, arg) =>
          let ((_, tiles) as fn, fn_ty) = {
            let fn = (skel, tiles);
            syn_fix_holes(ctx, fn);
          };
          switch (Typ.matched_arrow(fn_ty)) {
          | None =>
            let (_, tiles) = HExp.set_hole_status(InHole, fn);
            let arg = ana_fix_holes(ctx, arg, Typ.Hole);
            let tiles =
              Tiles.put_nth(n, Tile.PostOp(HExp.Ap(NotInHole, arg)), tiles);
            (tiles, Typ.Hole);
          | Some((ty1, ty2)) =>
            let arg = ana_fix_holes(ctx, arg, ty1);
            let tiles =
              Tiles.put_nth(n, Tile.PostOp(HExp.Ap(NotInHole, arg)), tiles);
            (tiles, ty2);
          };
        }
      | BinOp(skel1, n, skel2) =>
        switch (Tiles.get_binop(n, tiles)) {
        | Plus(_) =>
          let (_, tiles) = ana_fix_holes(ctx, (skel1, tiles), Typ.Num);
          let (_, tiles) = ana_fix_holes(ctx, (skel2, tiles), Typ.Num);
          let tiles =
            Tiles.put_nth(n, Tile.BinOp(HExp.Plus(NotInHole)), tiles);
          (tiles, Typ.Num);
        | OperatorHole =>
          let ((_, tiles), _) = syn_fix_holes(ctx, (skel1, tiles));
          let ((_, tiles), _) = syn_fix_holes(ctx, (skel2, tiles));
          (tiles, Typ.Hole);
        }
      };
    ((skel, tiles), ty);
  }
  and ana_fix_holes =
      (ctx: Ctx.t, (skel, tiles) as e: HExp.t, ty: Typ.t): HExp.t => {
    let subsume = () => {
      let (e, ty') = syn_fix_holes(ctx, e);
      let (_, tiles) =
        Typ.consistent(ty, ty') ? e : HExp.set_hole_status(InHole, e);
      tiles;
    };
    let tiles =
      switch (skel) {
      | Operand(n) =>
        switch (Tiles.get_operand(n, tiles)) {
        | OperandHole
        | Num(_)
        | Var(_) => subsume()
        | Paren(body) =>
          let body = ana_fix_holes(ctx, body, ty);
          Tiles.put_nth(n, Tile.Operand(HExp.Paren(body)), tiles);
        }
      | PreOp(n, skel) =>
        switch (Tiles.get_preop(n, tiles)) {
        | Lam(_, p) =>
          switch (Typ.matched_arrow(ty)) {
          | None =>
            let (p, _, ctx) = Pat.syn_fix_holes(ctx, p);
            let ((_, tiles), _) = {
              let body = (skel, tiles);
              syn_fix_holes(ctx, body);
            };
            Tiles.put_nth(n, Tile.PreOp(HExp.Lam(InHole, p)), tiles);
          | Some((ty1, ty2)) =>
            let (p, ctx) = Pat.ana_fix_holes(ctx, p, ty1);
            let (_, tiles) = {
              let body = (skel, tiles);
              ana_fix_holes(ctx, body, ty2);
            };
            Tiles.put_nth(n, Tile.PreOp(HExp.Lam(NotInHole, p)), tiles);
          }
        }
      | PostOp(_, n) =>
        switch (Tiles.get_postop(n, tiles)) {
        | Ap(_) => subsume()
        }
      | BinOp(_, n, _) =>
        switch (Tiles.get_binop(n, tiles)) {
        | OperatorHole
        | Plus(_) => subsume()
        }
      };
    (skel, tiles);
  };

  /**
   * Given an expression `e` in synthetic position under context
   * `ctx`, `syn_nth_type_mode(ctx, n, e)` returns the type mode of
   * the subexpression rooted at the `n`th tile in the unparsed
   * representation of `e`
   */;
  /*
   let rec syn_nth_type_mode =
           (ctx: Ctx.t, n: int, e: HExp.t): option(type_mode) =>
     switch (e) {
     | OperandHole
     | Num(_)
     | Var(_)
     | Paren(_) => n == 0 ? Some(Syn) : None
     | Lam(_, p, body) =>
       n == 0
         ? Some(Syn)
         : Option.bind(Pat.syn(ctx, p), ((_, ctx)) =>
             syn_nth_type_mode(ctx, n - 1, body)
           )
     | Ap(_, fn, _) =>
       let m = List.length(HExp.unparse(fn));
       n < m ? syn_nth_type_mode(ctx, n, fn) : Some(Syn);
     | Plus(_, l, r) =>
       let m = List.length(HExp.unparse(l));
       if (n < m) {
         ana_nth_type_mode(ctx, n, l, HTyp.Num);
       } else if (n > m) {
         ana_nth_type_mode(ctx, n - (m + 1), r, Num);
       } else {
         Some(Syn);
       };
     | OperatorHole(l, r) =>
       let m = List.length(HExp.unparse(l));
       if (n < m) {
         syn_nth_type_mode(ctx, n, l);
       } else if (n > m) {
         syn_nth_type_mode(ctx, n - (m + 1), r);
       } else {
         Some(Syn);
       };
     }
   */
  /**
   * Given an expression `e` in analytic position against type
   * `ty` under context `ctx`, `ana_nth_type_mode(ctx, n, e, ty)`
   * returns the type mode of the subexpression rooted at the
   * `n`th tile in the unparsed representation of `e`
   */;
  /*
   and ana_nth_type_mode =
       (ctx: Ctx.t, n: int, e: HExp.t, ty: HTyp.t): option(type_mode) =>
     switch (e) {
     | OperandHole
     | Num(_)
     | Var(_)
     | Paren(_) => n == 0 ? Some(Ana(ty)) : None
     | Lam(_, p, body) =>
       n == 0
         ? Some(Ana(ty))
         : Option.bind(HTyp.matched_arrow(ty), ((ty1, ty2)) =>
             Option.bind(Pat.ana(ctx, p, ty1), ctx =>
               ana_nth_type_mode(ctx, n - 1, body, ty2)
             )
           )
     | Ap(_)
     | Plus(_)
     | OperatorHole(_) => syn_nth_type_mode(ctx, n, e)
     };
   */
};
