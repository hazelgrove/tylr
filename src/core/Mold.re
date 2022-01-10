module Role: {
  type t =
    | Op
    | Pre(Precedence.t)
    | Post(Precedence.t)
    | Bin(Precedence.t);

  let precedence: t => Precedence.t;
};

module Sorts: {
  type t = {
    out: Sort.t,
    in_: list(Sort.t),
  };
};

type t = {
  role: Role.t,
  sorts: Sorts.t,
};

let nibs: t => Nibs.t;
