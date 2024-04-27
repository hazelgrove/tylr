open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module Base = {
  // invariant: List.length(loops) == List.length(links) + 1
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
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
let loop_length = ((lps, _)) => List.length(lps);
let length = c => loop_length(c) * 2 - 1;

let link = (a: 'lp, b: 'lk, (lps, lks): t('lp, 'lk)): t('lp, 'lk) => (
  [a, ...lps],
  [b, ...lks],
);
let knil = ((lps, lks): t('lp, 'lk), b: 'lk, a: 'lp): t('lp, 'lk) => (
  lps @ [a],
  lks @ [b],
);
let unlink =
    ((lps, lks): t('lp, 'lk)): Result.t(('lp, 'lk, t('lp, 'lk)), 'lp) =>
  switch (lks) {
  | [] => Error(List.hd(lps))
  | [b, ...lks] => Ok((List.hd(lps), b, (List.tl(lps), lks)))
  };
let unknil =
    ((lps, lks): t('lp, 'lk)): Result.t((t('lp, 'lk), 'lk, 'lp), 'lp) =>
  ListUtil.split_last_opt(lks)
  |> Result.of_option(~error=List.hd(lps))
  |> Result.map(~f=((lks, link)) => {
       let (lps, loop) = ListUtil.split_last(lps);
       ((lps, lks), link, loop);
     });

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
  let split_hd =
      ((lks, lps): t('lk, 'lp)): option(('lk, Base.t('lp, 'lk))) =>
    switch (lks) {
    | [] => None
    | [lk, ...lks] => Some((lk, (lps, lks)))
    };
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

let split_ft = ((lps, lks)) => {
  assert(lps != []);
  let (lps, lp) = ListUtil.split_last(lps);
  ((lps, lks), lp);
};
let ft = ((lps, _): t('lp, _)): 'lp => {
  assert(lps != []);
  ListUtil.last(lps);
};
let map_ft = (f: 'lp => 'lp, c: t('lp, 'lk)): t('lp, 'lk) => {
  let ((lps, lks), a) = split_ft(c);
  (lps @ [f(a)], lks);
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
  let (lps, lp) = ListUtil.split_last(loops(c));
  List.fold_right2(
    (lp, lk, xs) => [f_lp(lp), f_lk(lk), ...xs],
    lps,
    links(c),
    [f_lp(lp)],
  );
};

let fold_left =
    (
      f_loop: 'lp => 'acc,
      f_link: ('acc, 'lk, 'lp) => 'acc,
      (lps, lks): t('lp, 'lk),
    )
    : 'acc => {
  let (a, lps) = ListUtil.split_first(lps);
  List.fold_left2(f_link, f_loop(a), lks, lps);
};
let fold_left_map =
    (
      f_loop: 'lp1 => ('acc, 'lp2),
      f_link: ('acc, 'lk1, 'lp1) => ('acc, 'lk2, 'lp2),
      c: t('lp1, 'lk1),
    )
    : ('acc, t('lp2, 'lk2)) =>
  c
  |> fold_left(
       lp1 => {
         let (acc, lp2) = f_loop(lp1);
         (acc, unit(lp2));
       },
       ((acc, mapped), lk1, lp1) => {
         let (acc, lk2, lp2) = f_link(acc, lk1, lp1);
         (acc, knil(mapped, lk2, lp2));
       },
     );

let fold_right =
    (
      f_link: ('lp, 'lk, 'acc) => 'acc,
      f_loop: 'lp => 'acc,
      (lps, lks): t('lp, 'lk),
    ) => {
  let (lps, lp) = ListUtil.split_last(lps);
  List.fold_right2(f_link, lps, lks, f_loop(lp));
};

let cat =
    (cat: ('lp, 'lp) => 'lp, l: t('lp, 'lk), r: t('lp, 'lk)): t('lp, 'lk) =>
  l |> fold_right(link, lp => map_hd(cat(lp), r));

let append = (l: t('lp, 'lk), lk: 'lk, r: t('lp, 'lk)): t('lp, 'lk) =>
  l |> fold_right(link, lp => link(lp, lk, r));

let trim = ((lps, lks): t('lp, 'lk)): option(('lp, t('lk, 'lp), 'lp)) =>
  switch (lks) {
  | [] => None
  | [_, ..._] =>
    let (l, lps) = ListUtil.split_first(lps);
    let (lps, r) = ListUtil.split_last(lps);
    Some((l, mk(lks, lps), r));
  };
let untrim = (l, (lks, lps), r) => mk([l, ...lps] @ [r], lks);

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
      let (lp, lk, c) = unlink(c) |> Result.get_or_raise(invalid);
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

let unzip_link = (n: int, c: t('lp, 'lk)): option((t(_), 'lk, t(_))) => {
  let invalid = Invalid_argument("Chain.unzip_link");
  if (n < 1 || n mod 2 == 0) {
    raise(invalid);
  };
  let rec go = (pre, n, suf) => {
    open OptUtil.Syntax;
    let* (lk, lp, suf) = Affix.unlink(suf);
    n == 1
      ? Some((pre, lk, cons(lp, suf))) : go(link(lp, lk, pre), n - 2, suf);
  };
  let (hd, tl) = split_hd(c);
  go(unit(hd), n, tl);
};
