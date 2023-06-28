(lang dune 3.4)

(name simple_sequence)

(generate_opam_files true)

(source
 (github QAhell/simple_sequence_ocaml))

(authors "Armin Heller <support@arminheller.de>")

(maintainers "Armin Heller <support@arminheller.de>")

(license Apache-2.0)

(documentation https://github.com/QAhell/simple_sequence_ocaml)

(package
 (name simple_sequence)
 (version 0.1)
 (synopsis "List-like sequence implementations based on arrays and lazy lists")
 (description "The main point of this library is to provide a list-like programming interface for arrays. It also provides implementations based on lists and lazy lists. They all have different run-time and space characteristics, e. g. arrays must be copied when they're full or when multiple cons operations are performed on the same sequence. Lists and lazy lists only need constant time for cons. The implementations are not thread-safe except for the trivial list-based one.")
 (depends ocaml dune)
 (tags
    (topics sequence)))
