open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

module Base = {
  // invariant: List.length(loops) == List.length(links) + 1
  [@deriving (sexp, yojson, ord)]
  type t('loop, 'link) = (list('loop), list('link));
};
include Base;

let mk = (lps: list('lp), lks: list('lk)): t('lp, 'lk) => {
  if (List.length(lps) != List.length(lks) + 1) {
    raise(Invalid_argument("Chain.mk"));
  };
  (lps, lks);
};

let unit = (lp: 'lp): t('lp, _) => ([lp], []);
let loops: t('lp, _) => list('lp) = fst;
let links: t(_, 'lk) => list('lk) = snd;
let length = ((lps, _): t(_)) => List.length(lps) * 2 - 1;

let link = (a: 'lp, b: 'lk, (lps, lks): t('lp, 'lk)): t('lp, 'lk) => (
  [a, ...lps],
  [b, ...lks],
);
let unlink =
    ((lps, lks): t('lp, 'lk)): Result.t(('lp, 'lk, t('lp, 'lk)), 'lp) =>
  switch (lks) {
  | [] => Error(List.hd(lps))
  | [b, ...lks] => Ok((List.hd(lps), b, (List.tl(lps), lks)))
  };

module Affix = {
  type t('link, 'loop) = (list('link), list('loop));
  let empty = ([], []);
  let is_empty = ((lks, lps)) => lks == [] && lps == [];
  let cons = (lk, (lps, lks): Base.t(_)) => ([lk, ...lks], lps);
  let link = (lk, lp, (lks, lps): t(_)) => ([lk, ...lks], [lp, ...lps]);
  let unlink =
    fun
    | ([], _)
    | (_, []) => None
    | ([lk, ...lks], [lp, ...lps]) => Some((lk, lp, (lks, lps)));
  let knil = ((lks, lps): t(_), lk, lp) => (lks @ [lk], lps @ [lp]);

  let rev = ((lks, lps): t('lk, 'lp)): t('lp, 'lk) =
    (List.rev(lps), List.rev(lks));

  // todo: rename uncons
  let split_hd =
      ((lks, lps): t('lk, 'lp)): option(('lk, Base.t('lp, 'lk))) =>
    switch (lks) {
    | [] => None
    | [lk, ...lks] => Some((lk, (lps, lks)))
    };
  let fold_out = (~f, ~init, (lks, lps): t(_)) =>
    List.fold_right2(f, lks, lps, init);
};

let rec extend = (tl: Affix.t('lk, 'lp), c: t('lp, 'lk)) =>
  switch (tl) {
  | ([lk, ...lks], [lp, ...lps]) => extend((lks, lps), link(lp, lk, c))
  | _ => c
  };

let split_hd = ((lps, lks): t('lp, 'lk)): ('lp, Affix.t('lk, 'lp)) => {
  assert(lps != []);
  (List.hd(lps), (lks, List.tl(lps)));
};
let hd = (c: t('lp, _)): 'lp => fst(split_hd(c));
let map_hd = (f: 'lp => 'lp, c: t('lp, 'lk)): t('lp, 'lk) => {
  let (a, (lks, lps)) = split_hd(c);
  ([f(a), ...lps], lks);
};
let put_hd = lp => map_hd(_ => lp);

let cons = (hd: 'lp, (lks, lps): Affix.t('lk, 'lp)): t('lp, 'lk) => (
  [hd, ...lps],
  lks,
);
let snoc = ((lps, lks): Affix.t('lp, 'lk), lp: 'lp) => (lps @ [lp], lks);

let split_ft = ((lps, lks): t(_)): (Affix.t('lk, 'lp), 'lp) => {
  assert(lps != []);
  let (lps, lp) = Lists.Framed.ft_exn(lps);
  ((lks, lps), lp);
};
let ft = ((lps, _): t('lp, _)): 'lp => {
  assert(lps != []);
  Lists.ft_exn(lps);
};
let map_ft = (f: 'lp => 'lp, c: t('lp, 'lk)): t('lp, 'lk) => {
  let ((lks, lps), lp) = split_ft(c);
  (lps @ [f(lp)], lks);
};
let put_ft = lp => map_ft(_ => lp);

let rev =
    (~rev_loop=Fun.id, ~rev_link=Fun.id, (lps, lks): t('lp, 'lk))
    : t('lp, 'lk) => (
  List.rev_map(rev_loop, lps),
  List.rev_map(rev_link, lks),
);

let map_loop = (f_lp: 'lp1 => 'lp2, (lps, lks): t('lp1, 'lk)): t('lp2, 'lk) => (
  List.map(f_lp, lps),
  lks,
);
let map_link = (f_lk: 'lk1 => 'lk2, (lps, lks): t('lp, 'lk1)): t('lp, 'lk2) => (
  lps,
  List.map(f_lk, lks),
);
let map = (f_lp, f_lk, c) => c |> map_loop(f_lp) |> map_link(f_lk);

let mapi_loop = (f_lp, (lps, lks)) => (
  List.mapi(i => f_lp(2 * i), lps),
  lks,
);
let mapi_link = (f_lk, (lps, lks)) => (
  lps,
  List.mapi(i => f_lk(1 + 2 * i), lks),
);
let mapi = (f_lp, f_lk, c) => c |> mapi_loop(f_lp) |> mapi_link(f_lk);

let to_list = (f_lp: 'lp => 'x, f_lk: 'lk => 'x, c: t('lp, 'lk)): list('x) => {
  let (lps, lks) = c;
  let (lp, lps) = Lists.Framed.hd_exn(lps);
  List.fold_right2(
    (lk, lp, xs) => [f_lk(lk), f_lp(lp), ...xs],
    lks,
    lps,
    [],
  )
  |> List.cons(f_lp(lp));
};

let fold_left =
    (
      f_lp: 'lp => 'acc,
      f_lk: ('acc, 'lk, 'lp) => 'acc,
      (lps, lks): t('lp, 'lk),
    )
    : 'acc => {
  let (a, lps) = Lists.Framed.hd_exn(lps);
  List.fold_left2(f_lk, f_lp(a), lks, lps);
};
let fold_left_map =
    (
      f_lp: 'lp1 => ('acc, 'lp2),
      f_lk: ('acc, 'lk1, 'lp1) => ('acc, 'lk2, 'lp2),
      c: t('lp1, 'lk1),
    )
    : ('acc, t('lp2, 'lk2)) =>
  c
  |> fold_left(
       lp1 => {
         let (acc, lp2) = f_lp(lp1);
         (acc, unit(lp2));
       },
       ((acc, mapped), lk1, lp1) => {
         let (acc, lk2, lp2) = f_lk(acc, lk1, lp1);
         (acc, link(lp2, lk2, mapped));
       },
     )
  |> Tuples.map_snd(rev);

let fold_right =
    (f2: ('lp, 'lk, 'acc) => 'acc, f1: 'lp => 'acc, c: t('lp, 'lk)) => {
  let ((lks, lps), lp) = split_ft(c);
  List.fold_left2((acc, lk, lp) => f2(lp, lk, acc), f1(lp), lks, lps);
};

let cat =
    (cat: ('lp, 'lp) => 'lp, l: t('lp, 'lk), r: t('lp, 'lk)): t('lp, 'lk) =>
  l |> fold_right(link, lp => map_hd(cat(lp), r));

let append = (l: t('lp, 'lk), lk: 'lk, r: t('lp, 'lk)): t('lp, 'lk) =>
  l |> fold_right(link, lp => link(lp, lk, r));

let zip = (~pre=Affix.empty, ~suf=Affix.empty, foc) => {
  let (lks_pre, lps_pre) = pre;
  let (lks_suf, lps_suf) = suf;
  let lps = List.rev(lps_pre) @ [foc, ...lps_suf];
  let lks = List.rev(lks_pre) @ lks_suf;
  mk(lps, lks);
};

let unzip_loop = (n: int, c: t('lp, 'lk)): (Affix.t(_), 'lp, Affix.t(_)) => {
  let invalid = Invalid_argument("Chain.unzip_loop");
  if (n < 0 || n mod 2 != 0) {
    raise(invalid);
  };
  let rec go = (~pre=Affix.empty, n, c) =>
    if (n == 0) {
      let (lp, suf) = split_hd(c);
      (pre, lp, suf);
    } else {
      let (lp, lk, c) = unlink(c) |> Result.get_exn(invalid);
      let pre = Affix.link(lk, lp, pre);
      go(~pre, n - 2, c);
    };
  go(n, c);
};
let unzip_loops = (c: t('lp, 'lk)): list((Affix.t(_), 'lp, Affix.t(_))) => {
  let rec go = (~unzipped=[], ~pre=Affix.empty, c) =>
    switch (unlink(c)) {
    | Error(lp) => [(pre, lp, Affix.empty), ...unzipped]
    | Ok((lp, lk, c)) =>
      let unzipped = [(pre, lp, Affix.cons(lk, c)), ...unzipped];
      let pre = Affix.link(lk, lp, pre);
      go(~unzipped, ~pre, c);
    };
  go(c);
};

let unzip_link = (n: int, c: t('lp, 'lk)): (t(_), 'lk, t(_)) => {
  let invalid = Invalid_argument("Chain.unzip_link");
  if (n < 1 || n mod 2 == 0) {
    raise(invalid);
  };
  let rec go = (pre, n, suf) => {
    let (lk, lp, suf) = Options.get_exn(invalid, Affix.unlink(suf));
    n == 1 ? (pre, lk, cons(lp, suf)) : go(link(lp, lk, pre), n - 2, suf);
  };
  let (hd, tl) = split_hd(c);
  go(unit(hd), n, tl);
};

module Elem = {
  type t('lp, 'lk) =
    | Loop('lp)
    | Link('lk);
};

let unzip = (n: int, c: t('lp, 'lk)) =>
  Elem.(n mod 2 == 0 ? Loop(unzip_loop(n, c)) : Link(unzip_link(n, c)));

let pp = (pp_lp, pp_lk, out, c: t(_)) => {
  let (lp, (lks, lps)) = split_hd(c);
  let pp_tl = Fmt.(list(~sep=sp, pair(~sep=sp, pp_lk, pp_lp)));
  Fmt.pf(out, "%a@ %a", pp_lp, lp, pp_tl, List.combine(lks, lps));
};
let show = (pp_lp, pp_lk) => Fmt.to_to_string(pp(pp_lp, pp_lk));
