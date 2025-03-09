test: all
	@dune exec ./decaml.exe test.dml

all:
	$(MAKE) -C ..

explain:
	ocamlc -c common.ml extlib.ml preterm.ml term.ml module.ml && menhir --infer --explain parser.mly && rm *.cmi *.cmo parser.ml parser.mli && less parser.conflicts && rm parser.conflicts
