(lang dune 3.21)

(name python-interpreter)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Noah Schlenker")

(using menhir 3.0)

(package
 (name python-interpreter)
 (synopsis "A python intepreter in OCaml")
 (description "A python intepreter in OCaml")
 (depends
   ocaml
   base
   ppx_jane
   stdio
 )
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
