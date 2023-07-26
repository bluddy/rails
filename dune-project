(lang dune 3.6)

(name rails)

(generate_opam_files true)

(source
 (github bluddy/rails))

(authors "Yotam Barnoy")

(maintainers "yotambarnoy@gmail.com")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name rails)
 (allow_empty)
 (synopsis "OCaml-based remake of Railroad Tycoon")
 (description "Requires original files")
 (depends ocaml dune iter containers containers-data gen imagelib tsdl owl-base
          ocamlgraph logs
          ppx_import ppx_deriving ppx_hash ppx_yojson_conv lens)
 (tags
  (game engine remake)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
