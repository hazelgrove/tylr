module Base = {
  type t('text, 'tag) =
    | Break
    | Text('text)
    | Tag('tag, t('text, 'tag))
    | Seq(list(t('text, 'tag)))
    | Nest(int, t('text, 'tag));
  let nil = Seq([]);
};
include Base;

module Dims = {
  type t = Base.t(int, Rel.t(unit, Dir.t));

  let rec of_cell = (~nest=0, ~tag=Rel.Eq(), c: Cell.t) =>
    switch (Cell.get(c)) {
    | None => (nil, 0)
    | Some(M(l, w, r)) =>
      let l = of_cell(~tag=Neq(Dir.R), l);
      let w = of_wald(w);
      let r = of_cell()




      let ((l, h_l), w, (r, h_r)) => of_meld(m);
      Seq([
        Nest(indent, l),
        Tag
        Nest(indent, r),
      ])
    }
  and of_meld = (M(l, w, r): Meld.t) => (
    of_cell(l),
    of_wald(w),
    of_cell(~delim=Node(Wald.ft(w)), r),
  )
  and of_wald = (w: Wald.t) => failwith("todo");
};

