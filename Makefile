HTML_DIR=_build/default/src/web/www
HTML_FILE=$(HTML_DIR)/index.html

all:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

deps:
	opam switch import opam.export

release:
	dune build src --profile release

echo-html:
	@echo "$(shell pwd)/_build/default/src/web/www/index.html"

clean:
	dune clean

open:
	open "$(HTML_FILE)"

watch:
	dune build @src/fmt --auto-promote src --profile dev
	# dunno why dune stopped doing this...
	cp src/web/www/index.html $(HTML_DIR)/index.html
	cp src/web/www/style.css $(HTML_DIR)/style.css
	cp -r src/web/www/fonts $(HTML_DIR)/fonts
	dune build @src/fmt --auto-promote src --profile dev --watch
