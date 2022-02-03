open Sexplib.Std;

module rec Aba: {
  /**
   * An odd-length list of elements with alternating types
   */
  [@deriving sexp]
  type t('a, 'b) = ('a, Baba.t('b, 'a));

  let hd: t('a, _) => 'a;

  let prepend: (Baba.t('b, 'a), t('a, 'b)) => t('a, 'b);

  let rev: ('a => 'a, 'b => 'b, t('a, 'b)) => t('a, 'b);

  let get_a: t('a, _) => list('a);
  let get_b: t(_, 'b) => list('b);

  let map_to_list: ('a => 'c, 'b => 'c, t('a, 'b)) => list('c);

  let map_a: ('a1 => 'a2, t('a1, 'b)) => t('a2, 'b);
  let map_b: ('b1 => 'b2, t('a, 'b1)) => t('a, 'b2);
  let mapi_a: ((int, 'a1) => 'a2, t('a1, 'b)) => t('a2, 'b);

  let split: ('b => option('c), t('a, 'b)) => t(t('a, 'b), 'c);
  let split_last: t('a, 'b) => (Baba.t('a, 'b), 'a);

  let cons: ('a, Baba.t('b, 'a)) => t('a, 'b);
  let snoc: (Baba.t('a, 'b), 'a) => t('a, 'b);

  let cons_alist: ('a, Aba.t(list('a), 'b)) => Aba.t(list('a), 'b);

  let concat: (('a, 'a) => 'a, t('a, 'b), t('a, 'b)) => t('a, 'b);

  let fold_right: ('a => 'acc, ('a, 'b, 'acc) => 'acc, t('a, 'b)) => 'acc;
} = {
  /**
   * An odd-length list of elements with alternating types
   */
  [@deriving sexp]
  type t('a, 'b) = ('a, Baba.t('b, 'a));

  let hd = fst;

  let cons = (a: 'a, baba: Baba.t('b, 'a)) => (a, baba);

  let snoc = (abab: Baba.t('a, 'b), a: 'a): Aba.t('a, 'b) =>
    List.fold_right(
      ((a, b), (a', baba)) => (a, [(b, a'), ...baba]),
      abab,
      (a, []),
    );

  let cons_alist = (a: 'a, (hd, tl): t(list('a), 'b)) => (
    [a, ...hd],
    tl,
  );

  let get_a = ((a, baba)) => [a, ...List.map(snd, baba)];
  let get_b = ((_, baba)) => List.map(fst, baba);

  let map_to_list = (_, _, _) => failwith("todo Aba.map_to_list");

  let map_a = (f_a, (a, baba)) => (
    f_a(a),
    List.map(PairUtil.map_snd(f_a), baba),
  );
  let map_b = (f_b, (a, baba)) => (
    a,
    List.map(PairUtil.map_fst(f_b), baba),
  );
  let mapi_a = (f_ia, (a, baba)) => (
    f_ia(0, a),
    List.mapi((i, (b, a)) => (b, f_ia(i + 1, a)), baba),
  );

  let rec fold_right =
          (
            f_a: 'a => 'acc,
            f_ab: ('a, 'b, 'acc) => 'acc,
            (a, baba): t('a, 'b),
          )
          : 'acc =>
    switch (baba) {
    | [] => f_a(a)
    | [(b, a'), ...baba'] =>
      f_ab(a, b, fold_right(f_a, f_ab, (a', baba')))
    };

  let concat =
      (cat: ('a, 'a) => 'a, aba: Aba.t('a, 'b), (hd, tl): Aba.t('a, 'b)) =>
    aba
    |> fold_right(
         a => (cat(a, hd), tl),
         (a, b, (hd, tl)) => (a, [(b, hd), ...tl]),
       );

  let rec prepend = (prefix: Baba.t('b, 'a), aba: t('a, 'b)): t('a, 'b) =>
    switch (prefix) {
    | [] => aba
    | [(b', a'), ...prefix] =>
      let (a, baba) = aba;
      prepend(prefix, (a', [(b', a), ...baba]));
    };

  let rev = (rev_a, rev_b, (hd, tl)) =>
    tl
    |> List.fold_left(
         (reversed, (b, a)) => {
           let (hd, tl) = reversed;
           (rev_a(a), [(rev_b(b), hd), ...tl]);
         },
         (hd, []),
       );

  let split_last = (_aba: t('a, 'b)): (Baba.t('a, 'b), 'a) =>
    failwith("todo Aba.split_last");

  let split =
      (p: 'b => option('c), (a, baba): t('a, 'b)): t(t('a, 'b), 'c) => {
    let (baba, cabacaba) = Baba.split(p, baba);
    ((a, baba), cabacaba);
  };
  // let hd: t('a, 'b) => 'a = fst;
  // let join = (q: 'c => 'b, (aba, cabacaba): t(t('a, 'b), 'c)): t('a, 'b) =>
  //   Baba.append(aba, Baba.join(q, cabacaba));
}
and Baba: {
  /**
   * An even-length list of elements with alternating types
   */
  [@deriving sexp]
  type t('b, 'a) = list(('b, 'a));

  let cons: ('b, Aba.t('a, 'b)) => t('b, 'a);

  let split:
    ('b => option('c), t('b, 'a)) => (t('b, 'a), t('c, Aba.t('a, 'b)));

  let split_last: Baba.t('b, 'a) => option((Aba.t('b, 'a), 'a));

  let append: (Aba.t('a, 'b), t('b, 'a)) => Aba.t('a, 'b);
  let join: ('c => 'b, t('c, Aba.t('a, 'b))) => t('b, 'a);
} = {
  /**
   * An even-length list of elements with alternating types
   */
  [@deriving sexp]
  type t('b, 'a) = list(('b, 'a));

  let cons = (b: 'b, (a, baba): Aba.t('a, 'b)) => [(b, a), ...baba];

  let append = _ => failwith("todo Aba.append");

  let rec split =
          (p: 'b => option('c), baba: t('b, 'a))
          : (t('b, 'a), t('c, Aba.t('a, 'b))) =>
    switch (baba) {
    | [] => ([], [])
    | [(b, a), ...baba] =>
      switch (p(b)) {
      | None =>
        let (baba, cabacaba) = split(p, baba);
        ([(b, a), ...baba], cabacaba);
      | Some(c) =>
        let (aba, _abacaba) = Aba.split(p, (a, baba));
        let _ = failwith("todo fix tl");
        ([], [(c, aba)]);
      }
    };

  let split_last = _ => failwith("todo Baba.split_last");

  let join = (q: 'c => 'b, cabacaba: t('c, Aba.t('a, 'b))): t('b, 'a) =>
    cabacaba |> List.map(((c, aba)) => cons(q(c), aba)) |> List.concat;
};

module Frame = {
  module A = {
    [@deriving sexp]
    type t('a, 'b) = (Baba.t('b, 'a), Baba.t('b, 'a));
  };

  module B = {
    [@deriving sexp]
    type t('a, 'b) = (Aba.t('a, 'b), Aba.t('a, 'b));

    let fill = (b: 'b, (aba_pre, aba_suf): t('a, 'b)) => {
      let baba_pre = {
        let (a, baba) = aba_pre;
        [(b, a), ...baba];
      };
      Aba.prepend(baba_pre, aba_suf);
    };
  };
};

include Aba;
