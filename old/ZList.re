type t('z, 'a) = {
  prefix: list('a),
  z: 'z,
  suffix: list('a),
};

let get_z = ({z, _}) => z;
let put_z = (z, {prefix, suffix, _}) => {prefix, z, suffix};

let rec split_at = (n: int, xs: list('x)): t('x, 'x) =>
  switch (n, xs) {
  | (_, []) => failwith("list index out of bounds")
  | (0, [x, ...xs]) => {prefix: [], z: x, suffix: xs}
  | (_, [x, ...xs]) =>
    let zlist = split_at(n - 1, xs);
    {...zlist, prefix: [x, ...zlist.prefix]};
  };
