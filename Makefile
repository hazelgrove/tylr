HTML_DIR=_build/default/src/web/www
HTML_FILE=$(HTML_DIR)/index.html

all:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

init:
	mkdir -p src/core/material/precompiled
	@echo "" > src/core/material/precompiled/enter_l_map.txt
	@echo "" > src/core/material/precompiled/enter_r_map.txt
	@echo "" > src/core/material/precompiled/nts.txt
	@echo "" > src/core/material/precompiled/stances.txt
	@echo "" > src/core/material/precompiled/walk_l_map.txt
	@echo "" > src/core/material/precompiled/walk_r_map.txt
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev
	./_build/default/src/js/precompile.exe
	mv enter_l_map.txt src/core/material/precompiled
	mv enter_r_map.txt src/core/material/precompiled
	mv nts.txt src/core/material/precompiled
	mv stances.txt src/core/material/precompiled
	mv walk_l_map.txt src/core/material/precompiled
	mv walk_r_map.txt src/core/material/precompiled
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
	dune build @src/fmt --auto-promote src --profile dev --watch
