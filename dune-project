(lang dune 3.0)

(name rails)

(generate_opam_files true)

(source
 (github bluddy/rails))

(authors "Yotam Barnoy")

(maintainers "Yotam Barnoy")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name rails)
 (allow_empty)
 (synopsis "OCaml-based remake of Railroad Tycoon")
 (description "Requires original files")
 (depends ocaml dune iter containers containers-data gen imagelib imagelib.unix tsdl owl-base ppx_deriving ppx_hash lens)
 (tags
  (game engine remake)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
