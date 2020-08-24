type t =
  | Y(list(HPat.Tile.t), list(HPat.Tile.t))
  | Z(unzipped)
and unzipped =
  | ParenZ(t)
  | AnnZ_subj(HoleStatus.t, unzipped, HTyp.t)
  | AnnZ_ann(HoleStatus.t, HPat.t, ZTyp.t)
  | OperatorHoleZ_l(unzipped, HPat.t)
  | OperatorHoleZ_r(HPat.t, unzipped);

let rec set_hole_status = (status, zp) =>
  switch (zp) {
  | Y(prefix, suffix) =>
    let n = List.length(prefix);
    let (prefix, suffix) =
      List.rev(prefix)
      @ suffix
      |> HPat.parse
      |> HPat.set_hole_status(status)
      |> HPat.Tile.unparse
      |> ListUtil.split_n(n);
    Y(prefix, suffix);
  | Z(z) => Z(set_hole_status_unzipped(status, z))
  }
and set_hole_status_unzipped = (status, z) =>
  switch (z) {
  | ParenZ(zbody) => ParenZ(set_hole_status(status, zbody))
  | AnnZ_subj(_, zsubj, ann) => AnnZ_subj(status, zsubj, ann)
  | AnnZ_ann(_, subj, zann) => AnnZ_ann(status, subj, zann)
  | OperatorHoleZ_l(_)
  | OperatorHoleZ_r(_) => z
  };

let rec erase: t => HPat.t =
  fun
  | Y(prefix, suffix) => HPat.parse(List.rev(prefix) @ suffix)
  | Z(z) => erase_unzipped(z)
and erase_unzipped: unzipped => HPat.t =
  fun
  | ParenZ(zbody) => Paren(erase(zbody))
  | AnnZ_subj(status, zsubj, ann) => Ann(status, erase_unzipped(zsubj), ann)
  | AnnZ_ann(status, subj, zann) => Ann(status, subj, ZTyp.erase(zann))
  | OperatorHoleZ_l(zl, r) => OperatorHole(erase_unzipped(zl), r)
  | OperatorHoleZ_r(l, zr) => OperatorHole(l, erase_unzipped(zr));
