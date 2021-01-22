type op =
  | OpHole
  | Var(Var.t)
  | Paren(HTile.s);
type pre = unit; // empty
type post =
  | Ann(HTile.s);
type bin =
  | BinHole;
type tile = Tile.t(op, pre, post, bin);

exception Void_pre;

type t =
  | Op(op)
  | Pre(pre, t)
  | Post(t, post)
  | Bin(t, bin, t);

let precedence: tile => int =
  Tile.get(
    _ => 0,
    () => raise(Void_pre),
    fun
    | Ann(_) => 2,
    fun
    | BinHole => 1,
  );

let associativity =
  [(1, Associativity.Left)] |> List.to_seq |> IntMap.of_seq;

let mk = (ts: HTile.s): t => {
  if (ts == []) {
    failwith("expected ts to be nonempty");
  };
  let its = List.mapi((i, t) => (i, t), ts);
  let+ (max_p, max_p_ts) =
    List.fold_right(
      ((_, t) as it, max) => {
        let* (max_p, max_p_ts) = max;
        let* p = precedence(t);
        if (p > max_p) {
          Some((p, [it]));
        } else if (p == max_p) {
          Some((max_prec, [it, ...max_p_ts]));
        } else {
          max;
        };
      },
      its,
      Some((0, [])),
    );
  let a = IntMap.find(max_p, associativity);
  let n =
    if (List.length(max_p_ts) > 1 && a == Direction.Right) {
      // guaranteed to succeed if ts is nonempty
      let (_, (n, _)) = ListUtil.split_last(max_p_ts);
      n;
    } else {
      // guaranteed to succeed if ts is nonempty
      let (n, _) = List.hd(max_p_ts);
      n;
    };
  let {prefix, z, suffix} = ZList.split_at(n, ts);
  switch (z) {
  | Op(op) => Op(op)
  | Pre(pre) => Pre(pre, suffix)
  | Post(post) => Post(prefix, post)
  | Bin(bin) => Bin(prefix, bin, suffix)
  };
};

