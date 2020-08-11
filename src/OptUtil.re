let map2 = (f, o1, o2) =>
  switch (o1, o2) {
  | (None, _)
  | (_, None) => None
  | (Some(v1), Some(v2)) =>  Some(f(v1, v2))
  };