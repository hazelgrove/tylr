all:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev
