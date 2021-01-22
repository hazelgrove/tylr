type positioning('selection) =
  | Left('selection, list('selection))
  | Right(list('selection), 'selection);

type positioned('selection) = IntMap.t('selection);

type t('selection) = {
  positioning: (int, positioning('selection)),
  positioned,
};