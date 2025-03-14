test: all
	@dune exec ./decaml.exe test.dml

all:
	$(MAKE) -C ..

explain:
	@dune build
	less ../_build/default/src/parser.conflicts
