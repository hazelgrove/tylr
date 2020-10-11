open Virtual_dom.Vdom;

module Point = {
  type t = {
    x: float,
    y: float,
  };
};

module Path = {
  type t = list(cmd)
  and cmd =
    | Z
    | M(Point.t)
    | M_({
        dx: float,
        dy: float,
      })
    | L(Point.t)
    | L_({
        dx: float,
        dy: float,
      })
    | H({x: float})
    | H_({dx: float})
    | V({y: float})
    | V_({dy: float});

  let scale_cmd = (s: float) =>
    fun
    | (Z | M(_) | L(_) | H(_) | V(_)) as cmd => cmd
    | M_({dx, dy}) => M_({dx: s *. dx, dy: s *. dy})
    | L_({dx, dy}) => L_({dx: s *. dx, dy: s *. dy})
    | H_({dx}) => H_({dx: s *. dx})
    | V_({dy}) => V_({dy: s *. dy});

  let reverse = List.rev_map(scale_cmd(-1.));

  let string_of_flag =
    fun
    | false => "0"
    | true => "1";

  let string_of_command =
    fun
    | Z => "Z"
    | M({x, y}) => Printf.sprintf("M %f %f", x, y)
    | M_({dx, dy}) => Printf.sprintf("m %f %f", dx, dy)
    | L({x, y}) => Printf.sprintf("L %f %f", x, y)
    | L_({dx, dy}) => Printf.sprintf("l %f %f", dx, dy)
    | H({x}) => Printf.sprintf("H %f", x)
    | H_({dx}) => Printf.sprintf("h %f", dx)
    | V({y}) => Printf.sprintf("V %f", y)
    | V_({dy}) => Printf.sprintf("v %f", dy);

  let view = (~attrs: list(Attr.t), path: t): Node.t => {
    let buffer = Buffer.create(List.length(path) * 20);
    path
    |> List.iter(cmd => {
         Buffer.add_string(buffer, string_of_command(cmd));
         Buffer.add_string(buffer, " ");
       });
    Node.create_svg(
      "path",
      [Attr.create("d", Buffer.contents(buffer)), ...attrs],
      [],
    );
  };
};
