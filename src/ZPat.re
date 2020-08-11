type t =
  | Z(list(HPat.Tile.t), list(HPat.Tile.t))
  | ParenZ(t)
  | AnnZ(HPat.t, ZTyp.t);

let rec erase: t => HPat.t =
  fun
  | Z(prefix, suffix) => HPat.parse(List.rev(prefix) @ suffix)
  | ParenZ(zbody) => Paren(erase(zbody))
  | AnnZ(subj, zann) => Ann(subj, ZTyp.erase(zann));
