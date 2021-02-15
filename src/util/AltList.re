/**
 * An alternating list with
 * an odd number of elements
 */
type t('a, 'b) = ('a, list(('b, 'a)));

let singleton = a => (a, []);

let map_b = (f: 'b1 => 'b2, (hd, tl): t('a, 'b1)): t('a, 'b2) => (
  hd,
  List.map(((b, a)) => (f(b), a), tl),
);
