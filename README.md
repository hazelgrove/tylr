# `tylr` ![Build Status](https://github.com/hazelgrove/tylr/actions/workflows/deploy-branches.yml/badge.svg)

`tylr` is a tiny interactive demonstration of tile-based editing,
a new kind of structure editing that, like text, supports
linear token-level editing workflows but, unlike text, ensures
your manipulated tokens can always be parsed back into a
well-formed abstract syntax tree (AST).

Other structure editors restrict you to simple operations on the AST.
For example, if you have the expression `2 + 3 * 4` in
[MPS](https://www.jetbrains.com/mps/), you can't select things like
`* 4` or even `2 + 3`—never mind directly manipulate them—because they
don't correspond to complete subtrees in the AST.
Uniquely among structure editors, `tylr` lets you select and manipulate
near-arbitrary range selections, including those corresponding to sub- and
cross-tree portions of the AST.

You can try `tylr` online by visiting [tylr.fun](https://tylr.fun).
Click on the help button in the upper-right corner for a gif-laden
Twitter thread on how it works.

## Building and running `tylr` locally

Install OPAM version ≥2.0 and use it to install OCaml 4.08.1 as described
[here](https://ocaml.org/docs/install.html).

Clone this repo, run the following commands in the root directory,
and paste the output of the final command into your browser address bar.
```sh
make deps       # build dependencies
make            # build tylr src
make echo-html  # output path to compiled page
```
