type t =
  | Z(list(HPat.Tile.t), list(HPat.Tile.t))
  | ParenZ(t)
  | AnnZ(HoleStatus.t, HPat.t, ZTyp.t);

let rec erase: t => HPat.t =
  fun
  | Z(prefix, suffix) => HPat.parse(List.rev(prefix) @ suffix)
  | ParenZ(zbody) => Paren(erase(zbody))
  | AnnZ(status, subj, zann) => Ann(status, subj, ZTyp.erase(zann));
