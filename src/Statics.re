type type_mode =
  | Syn
  | Ana(Type.t);

module Pat = {
  let rec syn =
          (ctx: Ctx.t, (skel, tiles) as p: HPat.t): option((Type.t, Ctx.t)) =>
    switch (HTerm.get_nth_root(Skel.root_index(skel), p)) {
    | Operand(operand) =>
      switch (operand) {
      | OperandHole => Some((Type.Hole, ctx))
      | Var(x) => Some((Type.Hole, Ctx.add(x, Type.Hole, ctx)))
      | Paren(body) => syn(ctx, body)
      }
    | PreOp(_) => raise(HPat.Void_PreOp)
    | PostOp(skel, postop) =>
      switch (postop) {
      | Ann(status, ann) =>
        let subj = (skel, tiles);
        switch (status) {
        | NotInHole =>
          let ty = HTyp.contract(ann);
          ana(ctx, subj, ty) |> Option.map(ctx => (ty, ctx));
        | InHole =>
          syn(ctx, HPat.set_hole_status(NotInHole, p))
          |> Option.map(((_, ctx)) => (Type.Hole, ctx))
        };
      }
    | BinOp(skel1, binop, skel2) =>
      switch (binop) {
      | OperatorHole =>
        let p1 = (skel1, tiles);
        let p2 = (skel2, tiles);
        switch (syn(ctx, p1)) {
        | None => None
        | Some((_, ctx)) =>
          syn(ctx, p2) |> Option.map(((_, ctx)) => (Type.Hole, ctx))
        };
      }
    }
  and ana = (ctx: Ctx.t, (skel, _) as p: HPat.t, ty: Type.t): option(Ctx.t) => {
    let subsume = () =>
      switch (syn(ctx, p)) {
      | None => None
      | Some((ty', ctx)) => Type.consistent(ty, ty') ? Some(ctx) : None
      };
    switch (HTerm.get_nth_root(Skel.root_index(skel), p)) {
    | Operand(operand) =>
      switch (operand) {
      | OperandHole => subsume()
      | Var(x) => Some(Ctx.add(x, ty, ctx))
      | Paren(body) => ana(ctx, body, ty)
      }
    | PreOp(_) => raise(HPat.Void_PreOp)
    | PostOp(_, postop) =>
      switch (postop) {
      | Ann(_) => subsume()
      }
    | BinOp(_, binop, _) =>
      switch (binop) {
      | OperatorHole => subsume()
      }
    };
  };

  let rec syn_fix_holes =
          (ctx: Ctx.t, (skel, tiles) as p: HPat.t): (HPat.t, Type.t, Ctx.t) => {
    let n = Skel.root_index(skel);
    let (tiles, ty, ctx) =
      switch (HTerm.get_nth_root(n, p)) {
      | Operand(operand) =>
        switch (operand) {
        | OperandHole => (tiles, Type.Hole, ctx)
        | Var(x) =>
          let ctx = Ctx.add(x, Type.Hole, ctx);
          (tiles, Hole, ctx);
        | Paren(body) =>
          let (body, ty, ctx) = syn_fix_holes(ctx, body);
          let tiles =
            ListUtil.put_nth(n, Tile.Operand(HPat.Paren(body)), tiles);
          (tiles, ty, ctx);
        }
      | PreOp(_) => raise(HPat.Void_PreOp)
      | PostOp(skel, postop) =>
        switch (postop) {
        | Ann(_, ann) =>
          let ty = HTyp.contract(ann);
          let ((_, tiles), ctx) = {
            let subj = (skel, tiles);
            ana_fix_holes(ctx, subj, ty);
          };
          let tiles =
            ListUtil.put_nth(
              n,
              Tile.PostOp(HPat.Ann(NotInHole, ann)),
              tiles,
            );
          (tiles, ty, ctx);
        }
      | BinOp(skel1, binop, skel2) =>
        switch (binop) {
        | OperatorHole =>
          let ((_, tiles), _, ctx) = syn_fix_holes(ctx, (skel1, tiles));
          let ((_, tiles), _, ctx) = syn_fix_holes(ctx, (skel2, tiles));
          (tiles, Type.Hole, ctx);
        }
      };
    ((skel, tiles), ty, ctx);
  }
  and ana_fix_holes =
      (ctx: Ctx.t, (skel, tiles) as p: HPat.t, ty: Type.t): (HPat.t, Ctx.t) => {
    let n = Skel.root_index(skel);
    let subsume = () => {
      let (p, ty', ctx) = syn_fix_holes(ctx, p);
      let (_, tiles) =
        Type.consistent(ty, ty') ? p : HPat.set_hole_status(InHole, p);
      (tiles, ctx);
    };
    let (tiles, ctx) =
      switch (HTerm.get_nth_root(n, p)) {
      | Operand(operand) =>
        switch (operand) {
        | OperandHole => subsume()
        | Var(x) => (tiles, Ctx.add(x, ty, ctx))
        | Paren(body) =>
          let (body, ctx) = ana_fix_holes(ctx, body, ty);
          let tiles =
            ListUtil.put_nth(n, Tile.Operand(HPat.Paren(body)), tiles);
          (tiles, ctx);
        }
      | PreOp(_) => raise(HPat.Void_PreOp)
      | PostOp(_, postop) =>
        switch (postop) {
        | Ann(_) => subsume()
        }
      | BinOp(_, binop, _) =>
        switch (binop) {
        | OperatorHole => subsume()
        }
      };
    ((skel, tiles), ctx);
  };

  let rec syn_nth_type_mode = (n: int, (skel, tiles) as p: HPat.t): type_mode => {
    let m = Skel.root_index(skel);
    switch (HTerm.get_nth_root(m, p)) {
    | Operand(_) =>
      assert(n == m);
      Syn;
    | PreOp(_) => raise(HPat.Void_PreOp)
    | PostOp(skel, postop) =>
      assert(n <= m);
      switch (postop) {
      | Ann(_, ann) =>
        n == m
          ? Syn : ana_nth_type_mode(n, (skel, tiles), HTyp.contract(ann))
      };
    | BinOp(skel1, binop, skel2) =>
      switch (binop) {
      | OperatorHole =>
        if (n < m) {
          syn_nth_type_mode(n, (skel1, tiles));
        } else if (n > m) {
          syn_nth_type_mode(n, (skel2, tiles));
        } else {
          Syn;
        }
      }
    };
  }
  and ana_nth_type_mode =
      (n: int, (skel, _) as p: HPat.t, ty: Type.t): type_mode => {
    let m = Skel.root_index(skel);
    let subsume = () => syn_nth_type_mode(n, p);
    switch (HTerm.get_nth_root(m, p)) {
    | Operand(_) =>
      assert(n == m);
      Ana(ty);
    | PreOp(_) => raise(HPat.Void_PreOp)
    | PostOp(_, postop) =>
      assert(n <= m);
      switch (postop) {
      | Ann(_) => n == m ? Ana(ty) : subsume()
      };
    | BinOp(_, binop, _) =>
      switch (binop) {
      | OperatorHole => n == m ? Ana(ty) : subsume()
      }
    };
  };
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
          (ctx: Ctx.t, (skel, tiles) as e: HExp.t): (HExp.t, Type.t) => {
    let n = Skel.root_index(skel);
    let (tiles, ty) =
      switch (HTerm.get_nth_root(n, e)) {
      | Operand(operand) =>
        switch (operand) {
        | OperandHole => (tiles, Type.Hole)
        | Num(_, m) =>
          let tiles =
            ListUtil.put_nth(
              n,
              Tile.Operand(HExp.Num(NotInHole, m)),
              tiles,
            );
          (tiles, Type.Num);
        | Var(_, x) =>
          let (tile, ty) =
            switch (Ctx.find_opt(x, ctx)) {
            | None => (Tile.Operand(HExp.Var(InHole, x)), Type.Hole)
            | Some(ty) => (Tile.Operand(HExp.Var(NotInHole, x)), ty)
            };
          let tiles = ListUtil.put_nth(n, tile, tiles);
          (tiles, ty);
        | Paren(body) =>
          let (body, ty) = syn_fix_holes(ctx, body);
          let tiles =
            ListUtil.put_nth(n, Tile.Operand(HExp.Paren(body)), tiles);
          (tiles, ty);
        }
      | PreOp(preop, skel) =>
        switch (preop) {
        | Lam(_, p) =>
          let (p, ty1, ctx) = Pat.syn_fix_holes(ctx, p);
          let ((_, tiles), ty2) = {
            let body = (skel, tiles);
            syn_fix_holes(ctx, body);
          };
          let tiles =
            ListUtil.put_nth(n, Tile.PreOp(HExp.Lam(NotInHole, p)), tiles);
          (tiles, Arrow(ty1, ty2));
        }
      | PostOp(skel, postop) =>
        switch (postop) {
        | Ap(_, arg) =>
          let ((_, tiles) as fn, fn_ty) = {
            let fn = (skel, tiles);
            syn_fix_holes(ctx, fn);
          };
          switch (Type.matched_arrow(fn_ty)) {
          | None =>
            let (_, tiles) = HExp.set_hole_status(InHole, fn);
            let arg = ana_fix_holes(ctx, arg, Type.Hole);
            let tiles =
              ListUtil.put_nth(
                n,
                Tile.PostOp(HExp.Ap(NotInHole, arg)),
                tiles,
              );
            (tiles, Type.Hole);
          | Some((ty1, ty2)) =>
            let arg = ana_fix_holes(ctx, arg, ty1);
            let tiles =
              ListUtil.put_nth(
                n,
                Tile.PostOp(HExp.Ap(NotInHole, arg)),
                tiles,
              );
            (tiles, ty2);
          };
        }
      | BinOp(skel1, binop, skel2) =>
        switch (binop) {
        | Plus(_) =>
          let (_, tiles) = ana_fix_holes(ctx, (skel1, tiles), Type.Num);
          let (_, tiles) = ana_fix_holes(ctx, (skel2, tiles), Type.Num);
          let tiles =
            ListUtil.put_nth(n, Tile.BinOp(HExp.Plus(NotInHole)), tiles);
          (tiles, Type.Num);
        | OperatorHole =>
          let ((_, tiles), _) = syn_fix_holes(ctx, (skel1, tiles));
          let ((_, tiles), _) = syn_fix_holes(ctx, (skel2, tiles));
          (tiles, Type.Hole);
        }
      };
    ((skel, tiles), ty);
  }
  and ana_fix_holes =
      (ctx: Ctx.t, (skel, tiles) as e: HExp.t, ty: Type.t): HExp.t => {
    let n = Skel.root_index(skel);
    let subsume = () => {
      let (e, ty') = syn_fix_holes(ctx, e);
      let (_, tiles) =
        Type.consistent(ty, ty') ? e : HExp.set_hole_status(InHole, e);
      tiles;
    };
    let tiles =
      switch (HTerm.get_nth_root(n, e)) {
      | Operand(operand) =>
        switch (operand) {
        | OperandHole
        | Num(_)
        | Var(_) => subsume()
        | Paren(body) =>
          let body = ana_fix_holes(ctx, body, ty);
          ListUtil.put_nth(n, Tile.Operand(HExp.Paren(body)), tiles);
        }
      | PreOp(preop, skel) =>
        switch (preop) {
        | Lam(_, p) =>
          switch (Type.matched_arrow(ty)) {
          | None =>
            let (p, _, ctx) = Pat.syn_fix_holes(ctx, p);
            let ((_, tiles), _) = {
              let body = (skel, tiles);
              syn_fix_holes(ctx, body);
            };
            ListUtil.put_nth(n, Tile.PreOp(HExp.Lam(InHole, p)), tiles);
          | Some((ty1, ty2)) =>
            let (p, ctx) = Pat.ana_fix_holes(ctx, p, ty1);
            let (_, tiles) = {
              let body = (skel, tiles);
              ana_fix_holes(ctx, body, ty2);
            };
            ListUtil.put_nth(n, Tile.PreOp(HExp.Lam(NotInHole, p)), tiles);
          }
        }
      | PostOp(_, postop) =>
        switch (postop) {
        | Ap(_) => subsume()
        }
      | BinOp(_, binop, _) =>
        switch (binop) {
        | OperatorHole
        | Plus(_) => subsume()
        }
      };
    (skel, tiles);
  };

  /**
   * Given an expression `e` in synthetic position under context
   * `ctx`, `syn_nth_type_mode(ctx, n, e)` returns the type mode of
   * the subexpression rooted at the `n`th tile of `e`, returning
   * `None` if `e` does not synthesize a type
   */
  let rec syn_nth_type_mode =
          (ctx: Ctx.t, n: int, (skel, tiles) as e: HExp.t)
          : option(type_mode) => {
    let m = Skel.root_index(skel);
    switch (HTerm.get_nth_root(m, e)) {
    | Operand(_) =>
      assert(n == m);
      Some(Syn);
    | PreOp(preop, skel) =>
      switch (preop) {
      | Lam(_, p) =>
        n == m
          ? Some(Syn)
          : Option.bind(Pat.syn(ctx, p), ((_, ctx)) =>
              syn_nth_type_mode(ctx, n, (skel, tiles))
            )
      }
    | PostOp(skel, postop) =>
      switch (postop) {
      | Ap(_) =>
        n == m ? Some(Syn) : syn_nth_type_mode(ctx, n, (skel, tiles))
      }
    | BinOp(skel1, binop, skel2) =>
      if (n == m) {
        Some(Syn);
      } else {
        switch (binop) {
        | OperatorHole =>
          n < m
            ? syn_nth_type_mode(ctx, n, (skel1, tiles))
            : syn_nth_type_mode(ctx, n, (skel2, tiles))
        | Plus(_) =>
          n < m
            ? ana_nth_type_mode(ctx, n, (skel1, tiles), Type.Num)
            : ana_nth_type_mode(ctx, n, (skel2, tiles), Type.Num)
        };
      }
    };
  }
  /**
   * Given an expression `e` in analytic position against type
   * `ty` under context `ctx`, `ana_nth_type_mode(ctx, n, e, ty)`
   * returns the type mode of the subexpression rooted at the
   * `n`th tile of `e`, returning `None` if `e` fails to analyze
   */
  and ana_nth_type_mode =
      (ctx: Ctx.t, n: int, (skel, tiles) as e: HExp.t, ty: Type.t)
      : option(type_mode) => {
    let m = Skel.root_index(skel);
    let subsume = () =>
      n == m ? Some(Ana(ty)) : syn_nth_type_mode(ctx, n, e);
    switch (HTerm.get_nth_root(m, e)) {
    | Operand(_) =>
      assert(n == m);
      Some(Ana(ty));
    | PreOp(preop, skel) =>
      assert(n >= m);
      switch (preop) {
      | Lam(_, p) =>
        n == m
          ? Some(Ana(ty))
          : Option.bind(Type.matched_arrow(ty), ((ty1, ty2)) =>
              Option.bind(Pat.ana(ctx, p, ty1), ctx =>
                ana_nth_type_mode(ctx, n - 1, (skel, tiles), ty2)
              )
            )
      };
    | PostOp(_, postop) =>
      switch (postop) {
      | Ap(_) => subsume()
      }
    | BinOp(_, binop, _) =>
      switch (binop) {
      | OperatorHole
      | Plus(_) => subsume()
      }
    };
  };
};
