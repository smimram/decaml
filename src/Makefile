SOURCES = lang.ml parser.mly lexer.mll decaml.ml
RESULT = decaml

OCAMLYACC = menhir

test: all
	./decaml test.dml

all: dnc

include OCamlMakefile
