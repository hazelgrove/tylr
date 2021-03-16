open Sexplib.Std;

[@deriving sexp]
type t =
  | Op(int)
  | Pre(int, t)
  | Post(t, int)
  | Bin(t, int, t);

let rec size =
  fun
  | Op(_) => 1
  | Pre(_, r) => 1 + size(r)
  | Post(l, _) => size(l) + 1
  | Bin(l, _, r) => size(l) + 1 + size(r);

let root_index =
  fun
  | Op(n)
  | Pre(n, _)
  | Post(_, n)
  | Bin(_, n, _) => n;

// returns inclusive lower bound, exclusive upper bound
let rec range =
  fun
  | Op(n) => (n, n + 1)
  | Pre(n, r) => (n, snd(range(r)))
  | Post(l, n) => (fst(range(l)), n + 1)
  | Bin(l, _, r) => (fst(range(l)), snd(range(r)));

let rec skel_at = (n, skel) =>
  switch (skel) {
  | Op(m) => n == m ? skel : raise(Invalid_argument("Skel.skel_at"))
  | Pre(m, r) => n == m ? skel : skel_at(n, r)
  | Post(l, m) => n == m ? skel : skel_at(n, l)
  | Bin(l, m, r) =>
    if (n < m) {
      skel_at(n, l);
    } else if (n > m) {
      skel_at(n, r);
    } else {
      skel;
    }
  };
