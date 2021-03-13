open Util;

let map_cons = step => List.map(List.cons(step));

let of_pat = (_, _) => failwith("todo");

let rec of_exp = (info: TypeInfo_exp.t, e: Term_exp.t) =>
  Term_exp.(
    e
    |> Term.get(
         fun
         | OpHole => []
         | Num(_) =>
           switch (info.mode) {
           | Ana(ty) when !Type.consistent(ty, Num) => [[]]
           | Fn_pos => [[]]
           | Ana(_)
           | Syn => []
           }
         | Var(x) =>
           switch (Ctx.find_opt(x, info.ctx)) {
           | None => [[]]
           | Some(ty) =>
             switch (info.mode) {
             | Ana(ty') when !Type.consistent(ty, ty') => [[]]
             | Fn_pos when Option.is_none(Type.matches_arrow(ty)) => [[]]
             | Syn
             | Ana(_)
             | Fn_pos => []
             }
           }
         | Paren(body) => map_cons(Step.paren_body, of_exp(info, body)),
         fun
         | (Lam(p), body) => {
             let child_holes =
                 (~p_mode=TypeInfo_pat.Syn, ~body_mode=TypeInfo_exp.Syn, ()) => {
               let p_info = TypeInfo_pat.{ctx: info.ctx, mode: p_mode};
               let p_holes = map_cons(Step.lam_pat, of_pat(p_info, p));
               let body_holes = {
                 let (_, body_ctx) = Statics_pat.syn(p_info, p);
                 let body_info =
                   TypeInfo_exp.{ctx: body_ctx, mode: body_mode};
                 map_cons(Step.lam_body, of_exp(body_info, body));
               };
               p_holes @ body_holes;
             };
             switch (info.mode) {
             | Syn
             | Fn_pos => child_holes()
             | Ana(ty) =>
               switch (Type.matches_arrow(ty)) {
               | None => [[], ...child_holes()]
               | Some((ty_in, ty_out)) =>
                 child_holes(~p_mode=Ana(ty_in), ~body_mode=Ana(ty_out), ())
               }
             };
           }
         | (Let(p, def), body) => {
             let p_info = TypeInfo_pat.{ctx: info.ctx, mode: Syn};
             let p_holes = map_cons(Step.let_pat, of_pat(p_info, p));
             let (p_ty, _) = Statics_pat.syn(p_info, p);
             let def_holes =
               def
               |> of_exp(TypeInfo_exp.{...info, mode: Ana(p_ty)})
               |> map_cons(Step.let_def);
             let body_ctx = Statics_exp.extend_let_body_ctx(info.ctx, p, def);
             let body_holes =
               body
               |> of_exp({...info, ctx: body_ctx})
               |> map_cons(Step.let_body);
             p_holes @ def_holes @ body_holes;
           },
         fun
         | (fn, Ap(arg)) => {
             let fn_info = {...info, mode: Fn_pos};
             let fn_holes = map_cons(Step.ap_fn, of_exp(fn_info, fn));
             let (ty_in, ty_out) =
               Statics_exp.syn(fn_info, fn)
               |> Type.matches_arrow
               |> OptUtil.get(() =>
                    failwith(
                      "synthesis of fn should produce matched arrow type",
                    )
                  );
             let arg_holes =
               map_cons(
                 Step.ap_arg,
                 of_exp({...info, mode: Ana(ty_in)}, arg),
               );
             let ap_hole =
               switch (info.mode) {
               | Ana(ty) when !Type.consistent(ty, ty_out) => [[]]
               | Fn_pos when Option.is_none(Type.matches_arrow(ty_out)) => [
                   [],
                 ]
               | Syn
               | Ana(_)
               | Fn_pos => []
               };
             ap_hole @ fn_holes @ arg_holes;
           },
         fun
         | (l, Plus, r) => {
             let arg_info = {...info, mode: Ana(Num)};
             let l_holes = map_cons(Step.plus_l, of_exp(arg_info, l));
             let r_holes = map_cons(Step.plus_r, of_exp(arg_info, r));
             let plus_hole =
               switch (info.mode) {
               | Ana(ty) when !Type.consistent(ty, Num) => [[]]
               | Fn_pos => [[]]
               | Syn
               | Ana(_) => []
               };
             plus_hole @ l_holes @ r_holes;
           }
         | (l, BinHole, r) => {
             let arg_info = {...info, mode: Syn};
             let l_holes = map_cons(Step.binhole_l, of_exp(arg_info, l));
             let r_holes = map_cons(Step.binhole_r, of_exp(arg_info, r));
             l_holes @ r_holes;
           },
       )
  );
