open Sexplib.Std;

[@deriving sexp]
type t = Term.t(op, pre, post, bin)
and op =
  | OpHole
  | Num
  | Bool
  | Paren(t)
and pre = unit // empty
and post = unit // empty
and bin =
  | BinHole
  | Arrow;

exception Void_pre;
exception Void_post;

let mk_op_hole = () => OpHole;
let mk_bin_hole = () => BinHole;

let is_op_hole =
  fun
  | OpHole => true
  | _ => false;
let is_bin_hole =
  fun
  | BinHole => true
  | _ => false;

let rec to_type = ty =>
  ty
  |> Term.get(
       fun
       | OpHole => Type.Hole
       | Num => Num
       | Bool => Bool
       | Paren(body) => to_type(body),
       (((), _)) => raise(Void_pre),
       ((_, ())) => raise(Void_post),
       fun
       | (_, BinHole, _) => Type.Hole
       | (l, Arrow, r) => Arrow(to_type(l), to_type(r)),
     );

let of_type = (ty: Type.t) => {
  let rec go = (~parent_precedence=?, ~strict, ty: Type.t): t => {
    let p = Type.precedence(ty);
    let tm =
      switch (ty) {
      | Hole => Term.Op(OpHole)
      | Num => Op(Num)
      | Bool => Op(Bool)
      | Arrow(ty_in, ty_out) =>
        let go = go(~parent_precedence=p);
        Term.Bin(go(~strict=true, ty_in), Arrow, go(~strict=false, ty_out));
      };
    switch (parent_precedence) {
    | Some(pp) when p > pp || !strict && p >= pp => Op(Paren(tm))
    | _ => tm
    };
  };
  go(~parent_precedence=0, ~strict=false, ty);
};
