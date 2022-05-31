open Mold;

let forms_assoc: list((Label.t, list(Mold.t))) =
  List.fold_left(
    (acc, (_, {label, mold}: Form.t)) => {
      let molds =
        switch (List.assoc_opt(label, acc)) {
        | Some(old_molds) => old_molds @ [mold]
        | None => [mold]
        };
      List.cons((label, molds), List.remove_assoc(label, acc));
    },
    [],
    Form.forms,
  );

let get = (label: Label.t): list(Mold.t) =>
  switch (label, List.assoc_opt(label, forms_assoc)) {
  | ([t], _) when Token.is_num(t) => [mk_op(Exp, [])]
  | ([t], _) when Token.is_var(t) => [mk_op(Pat, []), mk_op(Exp, [])]
  | (_, Some(molds)) => molds
  | (_, None) => []
  };
