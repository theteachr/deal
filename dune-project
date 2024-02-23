(lang dune 3.12)

(name deal)

(generate_opam_files true)

(source
 (github theteachr/deal))

(authors "theteachr")

(maintainers "theteachr")

(license LICENSE)

(package
 (name deal)
 (synopsis "Monopoly Deal")
 (description "An implementation of the popular card game - Monopoly Deal, in OCaml.")
 (depends ocaml dune minttea)
 (tags
  (game "monopoly deal" multiplayer)))
