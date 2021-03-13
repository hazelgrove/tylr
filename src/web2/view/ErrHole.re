let shift = n => List.map(PairUtil.map_fst((+)(n)));

module Tm = {
  module Exp = {
    let mk = (
      e: Term_exp.t,
      info: Frame_exp.TypeInfo.t('a),
    ) => {
      let rec go = (~expanded=false, e, info) => {
        let len = failwith("len todo");
        e
        |> Term.get(
          fun
          | OpHole => []
          | Num(_) =>
            switch (info.mode) {
            | Syn(_) => []
            | Ana(ty, _) when Type.consistent(ty, Num) => []
            | Ana(_) | Fn_pos(_) =>
              [(0, Decoration.ErrHole.{expanded, len})]
            }
          | Var(x) => {
            let profile = (0, Decoration.ErrHole.{expanded, len});
            switch (Ctx.find_opt(x, info.ctx)) {
            | None => [profile]
            | Some(ty) =>
              switch (info.mode) {
              | Syn(_) => []
              | Ana(ty', _) =>
                Type.consistent(ty, ty') ? [] : [profile]
              | Fn(_) =>
                Option.is_some(Type.matches_arrow(ty))
                ? [] : [profile]
              };
            };
          }
          | Paren(body) => mk(body, info),
          fun
          | (Lam(p), body) =>
            switch (info.mode) {
            | Syn(_) =>
              let p_holes =
                Pat.mk(p, Frame_pat.TypeInfo.{ctx, mode: Syn((_, _) => ())});
              let body_holes =
                mk(body,)
            | Ana(ty, _) =>
              switch (Type.matches_arrow(ty)) {
              | None => []
              | Some()
              }
            | Fn_pos(_) => []
            }
        );
      };
    };
  }
};

module Frm = {
  module
}