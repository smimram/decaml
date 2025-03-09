(lang dune 2.0)
(name decaml)
(source (github smimram/decaml))
(license GPL-3.0)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")

(generate_opam_files true)

(package
 (name decaml)
 (synopsis "OCaml-like language with dependent types")
 (description "DeCaml is a an OCaml-like language with dependent types.")
 (depends
   (ocaml (>= 5.2))
 )
)
(using menhir 2.0)
