all:
	@dune build

doc:
	@dune build @doc
	@dune build @doc-private

test:
	@dune runtest -f
