module Exp = {
  let rec type_info =
          (ztile: ZExp'.ztile, info: TypeInfo.t): option(TypeInfo.t) => {
    open OptUtil.Syntax;
    let Exp(e, rest) = ZExp'.zip_ztile(HExp.mk_hole(), ztile);
    let* info =
      switch (rest) {
      | None => Some(info)
      | Some(ztile) => type_info(ztile, info)
      };
    let* (e, info) =
      Statics_H.Exp.nth_type_info(ZExp'.index(ztile), e, info);
    switch (HExp.root(e)) {
    | Operand(_) =>
      switch (Tile.get_operand(ztile)) {
      | ParenZ_body(_) => Some(info)
      }
    | PreOp(_) => raise(ZExp'.Void_ZPreOp)
    | PostOp(e, _) =>
      switch (Tile.get_postop(ztile)) {
      | ApZ_arg(status, _) =>
        let* fn_ty = Statics_H.Exp.syn(info.ctx, e);
        switch (status) {
        | InHole => Some({...info, mode: Syn})
        | NotInHole =>
          let+ (ty_in, _) = Type.matched_arrow(fn_ty);
          {...info, mode: Ana(ty_in)};
        };
      }
    | BinOp(_) => raise(ZExp'.Void_ZBinOp)
    };
  };
};

module Pat = {
  let rec type_info =
          (ztile: ZPat'.ztile, info: TypeInfo.t): option(TypeInfo.t) => {
    OptUtil.Syntax.(
      switch (ZPat'.zip_ztile(HPat.mk_hole(), ztile)) {
      | Exp(e, rest) =>
        let* info =
          switch (rest) {
          | None => Some(info)
          | Some(ztile) => Exp.type_info(ztile, info)
          };
        let* (e, info) =
          Statics_H.Exp.nth_type_info(ZPat'.index(ztile), e, info);
        switch (HExp.root(e)) {
        | Operand(_) => failwith("no expression operand containing a pattern")
        | PreOp(_) =>
          switch (info.mode, Tile.get_preop(ztile)) {
          | (Syn, LamZ_pat(_))
          | (Ana(_), LamZ_pat(InHole, _)) => Some({...info, mode: Syn})
          | (Ana(ty), LamZ_pat(NotInHole, _)) =>
            let+ (ty_in, _) = Type.matched_arrow(ty);
            {...info, mode: Ana(ty_in)};
          }
        | PostOp(_) => raise(ZPat'.Void_ZPostOp)
        | BinOp(_) => raise(ZPat'.Void_ZBinOp)
        };
      | Pat(p, rest) =>
        let* info =
          switch (rest) {
          | None => Some(info)
          | Some(ztile) => type_info(ztile, info)
          };
        let* (p, info) =
          Statics_H.Pat.nth_type_info(ZPat'.index(ztile), p, info);
        switch (HPat.root(p)) {
        | Operand(_) =>
          switch (Tile.get_operand(ztile)) {
          | ParenZ_body(_) => Some(info)
          }
        | PreOp(_) => failwith("no pattern preop containing a pattern")
        | PostOp(_) => raise(ZPat'.Void_ZPostOp)
        | BinOp(_) => raise(ZPat'.Void_ZBinOp)
        };
      }
    );
  };
};
