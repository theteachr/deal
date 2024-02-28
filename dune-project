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
 (description "An implementation of Monopoly Deal, a game of cards, in OCaml.")
 (depends ocaml dune)
 (tags
  (game "monopoly deal" multiplayer)))
