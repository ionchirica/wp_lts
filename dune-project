(lang dune 3.17)

(using menhir 2.0)

(name wp_lts)

(version 0.1)

(generate_opam_files true)

(source
 (github ionchirica/wp_lts))

(authors "Ion Chirica <i.chirica@campus.fct.unl.pt>")

(maintainers "Ion Chirica <i.chirica@campus.fct.unl.pt>")

(license MIT)

(package
 (name wp_lts)
 (synopsis "An OCaml implementation of a WP-Calculus for Linear Temporal Specifications")
 (description "An OCaml implementation of a WP-Calculus for Linear Temporal Specifications")
 (depends ocaml)
 (tags
  ("compilers" "wp-calculus" "linear temporal specifications")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
