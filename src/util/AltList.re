type t('a, 'b) = odd('a, 'b)
/**
 * An alternating list with
 * an odd number of elements
 */
and odd('a, 'b) =
  | A('a, option(even('a, 'b)))
/**
 * An alternating list with
 * an even number of elements
 */
and even('a, 'b) =
  | B('b, odd('a, 'b));
