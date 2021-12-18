open Sexplib.Std;

module rec Aba: {} = {
  /**
   * An odd-length list of elements with alternating types
   */
  type t('a, 'b) = ('a, Baba.t('b, 'a));

  let cons = (a: 'a, baba: Baba.t('a, 'b)) => (a, baba);

  let snoc = (baba: t('b, 'a), b: 'b): Aba.t('b, 'a) =>
    failwith("todo");

  let rec prepend = (prefix: Baba.t('b, 'a), aba: t('a, 'b)): t('a, 'b) =>
    switch (prefix) {
    | [] => aba
    | [(b', a'), ...prefix] =>
      let (a, baba) = aba;
      prepend(prefix, (a', [(b', a), ...baba]));
    };

  let split_last = (aba: t('a, 'b)): (Baba.t('a, 'b), 'a) =>
    failwith("todo");

  let split =
      (p: 'b => option('c), (a, baba): t('a, 'b)): t(t('a, 'b), 'c) => {
    let (baba, cabacaba) = Baba.split(p, baba);
    ((a, baba), cabacaba);
  };

  let hd: t('a, 'b) => 'a = fst;

  let join = (q: 'c => 'b, (aba, cabacaba): t(t('a, 'b), 'c)): t('a, 'b) =>
    Baba.append(aba, Baba.join(q, cabacaba))
}
and Baba: {} = {
  /**
   * An even-length list of elements with alternating types
   */
  type t('b, 'a) = list(('b, 'a));

  let cons = (b: 'b, (a, baba): Aba.t('a, 'b)) => [(b, a), ...baba];

  let split =
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
        let (aba, abacaba) = Aba.split(p, (a, baba));
        ([], [(c, aba), ...tl]);
      }
    };

  let join = (q: 'c => 'b, cabacaba: t('c, t('a, 'b))): t('b, 'a) =>
    cabacaba
    |> List.map(((c, aba)) => cons(q(c), aba))
    |> List.concat;
};

module Frame = {
  type a('a, 'b) = (Baba.t('b, 'a), Baba.t('b, 'a));
  type b('a, 'b) = (Aba.t('a, 'b), Aba.t('a, 'b));

  let fill_b = (b: 'b, (aba_pre, aba_suf): b('a, 'b)) => {
    let baba_pre = {
      let (a, baba) = aba_pre;
      [(b, a), ...baba];
    };
    Aba.prepend(baba_pre, aba_suf);
  };
};

include Aba;