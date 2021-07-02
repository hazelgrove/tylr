open Sexplib.Std;

/**
 * An alternating list with an even number of elements
 */
[@deriving sexp]
type even('x, 'y) = list(('x, 'y));
/**
 * An alternating list with an odd number of elements
 */
[@deriving sexp]
type odd('x, 'y) = ('x, even('y, 'x));

[@deriving sexp]
type t('a, 'b) = odd('a, 'b);

let rec rev = ((hd, tl): t('a, 'b)) =>
  switch (tl) {
  | [] => (hd, [])
  | [(b, a), ...tl] =>
    let (hd', tl') = rev((a, tl));
    (hd', tl' @ [(b, hd)]);
  };

/**
 * The surrounding elements of a subject element of
 * type 'a in an alternating list of type t('a, 'b).
 * The head elements of both prefix and suffix are
 * the ones closest to the subject.
 */
[@deriving sexp]
type a_frame('a, 'b) = (even('b, 'a), even('b, 'a));
/**
 * The surrounding elements of an element of
 * type 'b in an alternating list of type t('a, 'b).
 * The head elements of both prefix and suffix are
 * the ones closest to the subject.
 */
[@deriving sexp]
type b_frame('a, 'b) = (odd('a, 'b), odd('a, 'b));

let rec prepend = (prefix: even('b, 'a), abs: t('a, 'b)): t('a, 'b) =>
  switch (prefix) {
  | [] => abs
  | [(b', a'), ...prefix] =>
    let (a, bas) = abs;
    prepend(prefix, (a', [(b', a), ...bas]));
  };

let fill_b_frame = (b: 'b, (prefix, suffix): b_frame('a, 'b)) => {
  let prefix = {
    let (a, prefix) = prefix;
    [(b, a), ...prefix];
  };
  prepend(prefix, suffix);
};

let map_a = (f: 'a1 => 'a2, (hd, tl): t('a1, 'b)): t('a2, 'b) => (
  hd,
  List.map(((b, a)) => (b, f(a)), tl),
);
let map_b = (f: 'b1 => 'b2, (hd, tl): t('a, 'b1)): t('a, 'b2) => (
  hd,
  List.map(((b, a)) => (f(b), a), tl),
);
let map = (f_a, f_b, l) => map_a(f_a, map_b(f_b, l));

let even_to_list = (f: 'x => 'a, g: 'y => 'a, xys: even('x, 'y)) =>
  xys |> List.map(((x, y)) => [f(x), g(y)]) |> List.flatten;

let odd_to_list = (f: 'x => 'a, g: 'y => 'a, (x, yxs): odd('x, 'y)) =>
  yxs |> even_to_list(g, f) |> List.cons(f(x));

let fold_left_map =
    (
      f_a: ('acc, 'a1) => ('acc, 'a2),
      f_b: ('acc, 'b1) => ('acc, 'b2),
      init: 'acc,
      (hd, tl): t('a1, 'b1),
    )
    : ('acc, t('a2, 'b2)) => {
  let (init_tl, hd') = f_a(init, hd);
  let (acc, tl') =
    tl
    |> ListUtil.fold_left_map(
         (acc, (b, a)) => {
           let (acc, b') = f_b(acc, b);
           let (acc, a') = f_a(acc, a);
           (acc, (b', a'));
         },
         init_tl,
       );
  (acc, (hd', tl'));
};
