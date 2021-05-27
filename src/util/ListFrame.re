open Sexplib.Std;

// heads of prefix and suffix neighbor the subject
[@deriving sexp]
type t('x) = (list('x), list('x));

let rec mk = (n: int, xs: list('x)): t('x) => {
  let invalid_arg = () => raise(Invalid_argument("ListUtil.mk_frame"));
  if (n < 0) {
    invalid_arg();
  } else if (n == 0) {
    ([], xs);
  } else {
    switch (xs) {
    | [] => invalid_arg()
    | [x, ...xs] =>
      let (prefix, suffix) = mk(n - 1, xs);
      (prefix @ [x], suffix);
    };
  };
};

let rec split_nth = (n: int, xs: list('x)): ('x, t('x)) =>
  switch (n, xs) {
  | (_, []) => failwith("list index out of bounds")
  | (0, [x, ...xs]) => (x, ([], xs))
  | (_, [x, ...xs]) =>
    let (subj, (prefix, suffix)) = split_nth(n - 1, xs);
    (subj, (prefix @ [x], suffix));
  };

let to_list = (~subject: list('x)=[], (prefix, suffix): t('x)) =>
  List.concat([List.rev(prefix), subject, suffix]);

let append = ((prefix, suffix): t('x), (prefix', suffix'): t('x)) => (
  prefix @ prefix',
  suffix @ suffix',
);
