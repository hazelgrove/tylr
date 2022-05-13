open Util;

[@deriving show]
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

let children =
  fun
  | Op(_) => []
  | Pre(_, skel) => [(Direction.Right, skel)]
  | Post(skel, _) => [(Left, skel)]
  | Bin(l, _, r) => [(Left, l), (Right, r)];

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

type ip = (int, Piece.t);
let mk = (seg: Segment.t): t => {
  if (!Segment.convex(seg)) {
    raise(Invalid_argument("Skel.mk"));
  };

  let push_output = ((i, p): ip, output_stack: list(t)): list(t) =>
    switch (Piece.shapes(p)) {
    | (Convex, Convex) => [Op(i), ...output_stack]
    | (Convex, Concave(_)) =>
      switch (output_stack) {
      | [] => failwith("impossible: pre encountered empty stack")
      | [skel, ...skels] => [Pre(i, skel), ...skels]
      }
    | (Concave(_), Convex) =>
      switch (output_stack) {
      | [] => failwith("impossible: post encountered empty stack")
      | [skel, ...skels] => [Post(skel, i), ...skels]
      }
    | (Concave(_), Concave(_)) =>
      switch (output_stack) {
      | []
      | [_] =>
        failwith("impossible: bin encountered empty or singleton stack")
      | [skel1, skel2, ...skels] => [Bin(skel2, i, skel1), ...skels]
      }
    };

  let process_op = (~output_stack, ~shunted_stack, iop) => (
    output_stack,
    [iop, ...shunted_stack],
  );

  let rec process_pre =
          (~output_stack: list(t), ~shunted_stack: list(ip), ipre: ip) => {
    switch (shunted_stack) {
    | [] => (output_stack, [ipre, ...shunted_stack])
    | [(_, p) as ip, ...ips] =>
      switch (Piece.shapes(p)) {
      | (_, Concave(_)) => (output_stack, [ipre, ...shunted_stack])
      | (_, Convex) =>
        process_pre(
          ~output_stack=push_output(ip, output_stack),
          ~shunted_stack=ips,
          ipre,
        )
      }
    };
  };
  // assumes postops lose ties with preops and binops
  let rec process_post =
          (
            ~output_stack: list(t),
            ~shunted_stack: list(ip),
            ~prec: Precedence.t,
            ipost: ip,
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [ipost, ...shunted_stack])
    | [(_, p) as ip, ...ips] =>
      switch (Piece.shapes(p)) {
      | (_, Convex) =>
        process_post(
          ~output_stack=push_output(ip, output_stack),
          ~shunted_stack=ips,
          ~prec,
          ipost,
        )
      | (_, Concave(prec_p)) =>
        prec_p < prec
        || prec_p == prec
        && Precedence.associativity(prec_p) == Left
          ? process_post(
              ~output_stack=push_output(ip, output_stack),
              ~shunted_stack=ips,
              ~prec,
              ipost,
            )
          : (output_stack, [ipost, ...shunted_stack])
      }
    };
  // currently assumes binops lose ties with preops
  let rec process_bin =
          (
            ~output_stack: list(t),
            ~shunted_stack: list(ip),
            ~prec: Precedence.t,
            ibin: ip,
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [ibin, ...shunted_stack])
    | [(_, p) as ip, ...ips] =>
      switch (Piece.shapes(p)) {
      | (_, Convex) =>
        process_bin(
          ~output_stack=push_output(ip, output_stack),
          ~shunted_stack=ips,
          ~prec,
          ibin,
        )
      | (_, Concave(prec_p)) =>
        prec_p < prec
        || prec_p == prec
        && Precedence.associativity(prec_p) == Left
          ? process_bin(
              ~output_stack=push_output(ip, output_stack),
              ~shunted_stack=ips,
              ~prec,
              ibin,
            )
          : (output_stack, [ibin, ...shunted_stack])
      }
    };
  let rec go =
          (
            ~output_stack: list(t)=[],
            ~shunted_stack: list(ip)=[],
            ips: list(ip),
          )
          : list(t) => {
    switch (ips) {
    | [] =>
      shunted_stack
      |> List.fold_left(
           (output_stack, t) => push_output(t, output_stack),
           output_stack,
         )
    | [(_, p) as ip, ...ips] =>
      let process =
        switch (Piece.shapes(p)) {
        | (Convex, Convex) => process_op
        | (Convex, Concave(_)) => process_pre
        | (Concave(prec), Convex) => process_post(~prec)
        | (Concave(prec), Concave(_)) => process_bin(~prec)
        };
      let (output_stack, shunted_stack) =
        process(~output_stack, ~shunted_stack, ip);
      go(~output_stack, ~shunted_stack, ips);
    };
  };

  seg |> List.mapi((i, p) => (i, p)) |> go |> List.hd;
};
