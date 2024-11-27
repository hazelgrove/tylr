open Js_of_ocaml;

let get_elem_by_id = id => {
  let doc = Dom_html.document;
  Js.Opt.get(doc##getElementById(Js.string(id)), () => {
    assert
      (false)
      //print_endline(id);
  });
};

let clipboard_shim_id = "clipboard-shim";

let focus_clipboard_shim = () => get_elem_by_id(clipboard_shim_id)##focus;

open Virtual_dom.Vdom;
let clipboard_shim = {
  Node.textarea(~attrs=[Attr.id(clipboard_shim_id)], []);
};

let copy = (str: string) => {
  focus_clipboard_shim();
  Dom_html.document##execCommand(
    Js.string("selectAll"),
    Js.bool(false),
    Js.Opt.empty,
  );
  Dom_html.document##execCommand(
    Js.string("insertText"),
    Js.bool(false),
    Js.Opt.option(Some(Js.string(str))),
  );
  Dom_html.document##execCommand(
    Js.string("selectAll"),
    Js.bool(false),
    Js.Opt.empty,
  );
};

let trim_leading_whitespace =
  Re.Str.global_replace(Re.Str.regexp("\n[ ]*"), "\n");

let paste = evt =>
  Js.to_string(evt##.clipboardData##getData(Js.string("text")))
  |> trim_leading_whitespace;
