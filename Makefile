all:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

deps:
	opam switch import opam.export

release:
	dune build src --profile release

echo-html:
	@echo "$(shell pwd)/_build/default/src/web/www/index.html"