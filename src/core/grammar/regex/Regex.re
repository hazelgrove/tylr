open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('a) =
  | Atom('a)
  | Star(t('a))
  | Seq(s('a))
  | Alt(s('a))
and s('a) = list(t('a));

let atom = a => Atom(a);
let star = r => Star(r);
let seq = rs => Seq(rs);
let alt = rs => Alt(rs);

let eps = Seq([]);
let opt = r => Alt([eps, r]);

let aseq = atoms => seq(List.map(atom, atoms));

let rec flatten =
  fun
  | Seq(rs) => List.concat_map(flatten, rs)
  | r => [r];

let push = (~from: Dir.t, r) =>
  fun
  | Seq(rs) => Seq(from == L ? [r, ...rs] : rs @ [r])
  | (Atom(_) | Star(_) | Alt(_)) as r' =>
    Seq(from == L ? [r, r'] : [r', r]);

let rec fold =
        (
          ~atom: 'a => 'acc,
          ~star: 'acc => 'acc,
          ~seq: list('acc) => 'acc,
          ~alt: list('acc) => 'acc,
          r: t('a),
        ) => {
  let fold = fold(~atom, ~star, ~seq, ~alt);
  switch (r) {
  | Atom(a) => atom(a)
  | Star(r) => star(fold(r))
  | Seq(rs) => seq(List.map(fold, rs))
  | Alt(rs) => alt(List.map(fold, rs))
  };
};

let atoms = r =>
  fold(~atom=a => [a], ~star=Fun.id, ~seq=List.concat, ~alt=List.concat, r);

let map = f => fold(~atom=a => atom(f(a)), ~star, ~seq, ~alt);

let nullable = r =>
  r
  |> fold(
       // assuming all atoms are non-nullable
       // but could change this in future
       ~atom=_ => false,
       ~star=_ => true,
       ~seq=List.for_all(Fun.id),
       ~alt=List.exists(Fun.id),
     );

// currently assuming:
// (1) no consecutive kids
// (2) no consecutive tokens
// (3) every sort is convex
// only (1) fundamentally necessary
exception Ill_typed;

// let tok = (lbl: Label.t) => Atom(Atom.Tok(lbl));
// let tokc = (t: Token.t) => tok(Const(t));
// let kid = s => Atom(Atom.Kid(s));
