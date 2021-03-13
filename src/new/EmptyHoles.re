let map_cons = step => List.map(List.cons(step));

let rec of_typ = (ty: Term_typ.t) =>
  Term_typ.(
    ty
    |> Term.get(
         fun
         | OpHole => [[]]
         | Num
         | Bool => []
         | Paren(body) => map_cons(Step.paren_body, of_typ(body)),
         fun
         | ((), _) => raise(Term_typ.Void_pre),
         fun
         | (_, ()) => raise(Term_typ.Void_post),
         fun
         | (l, Arrow, r) =>
           map_cons(Step.arrow_l, of_typ(l))
           @ map_cons(Step.arrow_r, of_typ(r))
         | (l, BinHole, r) =>
           [[]]
           @ map_cons(Step.binhole_l, of_typ(l))
           @ map_cons(Step.binhole_r, of_typ(r)),
       )
  );

let rec of_pat = (p: Term_pat.t) =>
  Term_pat.(
    p
    |> Term.get(
         fun
         | OpHole => [[]]
         | Var(_) => []
         | Paren(body) => map_cons(Step.paren_body, of_pat(body)),
         fun
         | ((), _) => raise(Term_pat.Void_pre),
         fun
         | (subj, Ann(ann)) =>
           map_cons(Step.ann_subj, of_pat(subj))
           @ map_cons(Step.ann_ann, of_typ(ann)),
         fun
         | (l, BinHole, r) =>
           [[]]
           @ map_cons(Step.binhole_l, of_pat(l))
           @ map_cons(Step.binhole_r, of_pat(r)),
       )
  );

let rec of_exp = (e: Term_exp.t) =>
  Term_exp.(
    e
    |> Term.get(
         fun
         | OpHole => [[]]
         | Num(_)
         | Var(_) => []
         | Paren(body) => map_cons(Step.paren_body, of_exp(body)),
         fun
         | (Lam(p), body) =>
           map_cons(Step.lam_pat, of_pat(p))
           @ map_cons(Step.lam_body, of_exp(body))
         | (Let(p, def), body) =>
           map_cons(Step.let_pat, of_pat(p))
           @ map_cons(Step.let_def, of_exp(def))
           @ map_cons(Step.let_body, of_exp(body)),
         fun
         | (_, Ap(_)) => failwith("ap todo"),
         fun
         | (l, Plus, r) =>
           map_cons(Step.plus_l, of_exp(l))
           @ map_cons(Step.plus_r, of_exp(r))
         | (l, BinHole, r) =>
           [[]]
           @ map_cons(Step.binhole_l, of_exp(l))
           @ map_cons(Step.binhole_r, of_exp(r)),
       )
  );
