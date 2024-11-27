open Stds;

let complete_face = (site: Zipper.Site.t, ctx: Ctx.t) =>
  switch (site) {
  | Within(tok) =>
    Token.complete(tok)
    |> Option.map(tok => ctx |> Ctx.push(~onto=L, Token.clear_marks(tok)))
  | Between =>
    open Options.Syntax;
    let complete = side => {
      let (face, ctx) = Ctx.pull(~from=side, ctx);
      let* tok = Delim.to_opt(face);
      let+ tok = Token.complete(tok);
      Ctx.push(~onto=L, tok, ctx);
    };
    // assuming this is only called when tabbing forward
    let/ () = complete(L);
    complete(R);
  };

let perform = (d: Dir.t, z: Zipper.t): option(Zipper.t) =>
  switch (Zipper.cursor_site(z)) {
  | (Select(_), _) => Move.hstep(d, z)
  | (Point(site), ctx) =>
    open Options.Syntax;
    // first try completing or expanding a neighbor
    let/ () =
      switch (d) {
      | L => None
      | R =>
        let/ () = Option.map(Zipper.mk, complete_face(site, ctx));
        Modify.try_expand(" ", z);
      };
    // otherwise jump to next obligation
    let c = Zipper.zip(~save_cursor=true, z);
    let normal = Zipper.normalize(~cell=c);
    let car =
      c.marks.cursor
      |> Options.get_exn(Zipper.Bug__lost_cursor)
      |> Path.Cursor.get_point
      |> Option.get;
    let path =
      c.marks.obligs
      |> Path.Map.filter((_, mtrl: Mtrl.T.t) => mtrl != Space(Unmolded))
      |> Dir.pick(
           d,
           (
             Path.Map.find_last_opt(p => Path.lt(normal(p), car.path)),
             Path.Map.find_first_opt(p => Path.gt(normal(p), car.path)),
           ),
         )
      |> Option.map(fst)
      |> Options.get(() => Cell.end_path(~side=d, c));
    c
    |> Cell.put_cursor(Point(Caret.focus(path)))
    |> Zipper.unzip_exn
    |> Option.some;
  };
