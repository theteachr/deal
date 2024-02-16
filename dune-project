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
 (synopsis "Monopoly Deal Online")
 (description "An online multiplayer port of the popular card game: Monopoly Deal")
 (depends ocaml dune)
 (tags
  (game "monopoly deal" multiplayer)))
