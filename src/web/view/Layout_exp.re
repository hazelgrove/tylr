let rec mk = (e: Term_exp.t): Layout.t => {
  open Layout;
  let l =
    e
    |> Term.get(
      fun
      | OpHole => Text(Unicode.nbsp)
      | Var(x) => Text(x)
      | Num(n) => Text(string_of_int(n))
      | Paren(body) =>
        mk_Paren(child(Step.paren_body, mk(body))),
      fun
      | (Lam(p), body) =>
        seps([
          Text(Unicode.lam),
          child(Step.lam_pat, Layout_pat.mk(p)),
          Text("."),
          child(Step.lam_body, mk(body))
        ])
      | Let(p, def) =>
        seps([
          Text("let"),
          child(Step.let_pat, Layout_pat.mk(p)),
          Text("="),
          child(Step.let_def, mk(def)),
          Text("in"),
          child(Step.let_body, mk(body)),
        ])
    );
  Annot(Term, l);
};