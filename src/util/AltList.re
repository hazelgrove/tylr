/**
 * An alternating list with an even number of elements
 */
type even('x, 'y) = list(('x, 'y));
/**
 * An alternating list with an odd number of elements
 */
type odd('x, 'y) = ('x, even('y, 'x));

type t('a, 'b) = odd('a, 'b);

let rev = _ => failwith("AltList.rev todo");

/**
 * The surrounding elements of a subject element of
 * type 'a in an alternating list of type t('a, 'b).
 * The head elements of both prefix and suffix are
 * the ones closest to the subject.
 */
type a_frame('a, 'b) = (even('b, 'a), even('b, 'a));
/**
 * The surrounding elements of an element of
 * type 'b in an alternating list of type t('a, 'b).
 * The head elements of both prefix and suffix are
 * the ones closest to the subject.
 */
type b_frame('a, 'b) = (odd('a, 'b), odd('a, 'b));

/*
 let rev_odd = (_: odd('a, 'b)): odd('b, 'a) =>
   failwith("todo");
 */

let prepend = (_: even('b, 'a), _: t('a, 'b)): t('a, 'b) =>
  failwith("todo");
let append = (_: t('a, 'b), _: even('b, 'a)): t('a, 'b) =>
  failwith("todo");

let fill_b_frame = (b: 'b, (prefix, suffix): b_frame('a, 'b)) => {
  let prefix = {
    let (a, prefix) = prefix;
    [(b, a), ...prefix];
  };
  prepend(prefix, suffix);
};

let map_b = (f: 'b1 => 'b2, (hd, tl): t('a, 'b1)): t('a, 'b2) => (
  hd,
  List.map(((b, a)) => (f(b), a), tl),
);
