type t('a, 'b) = odd('a, 'b)
and odd('a, 'b) =
  | A('a, option(even('a, 'b)))
and even('a, 'b) =
  | B('b, odd('a, 'b));
