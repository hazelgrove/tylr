all:
	dune build @src/fmt --auto-promote || true
	dune build src

test:
	dune build @src/fmt --auto-promote || true
	dune runtest || true