(lang dune 3.10)

(name ocaml_router)

(generate_opam_files true)

(source
 (github TheoBessel/MLServ))

(authors "Theo Bessel")

(maintainers "Theo Bessel")

(license LICENSE)

(documentation "https://github.com/TheoBessel/MLServ/README.md")

(package
 (name ml_serv)
 (synopsis "A multithread TCP server in OCaml")
 (description "A multithread TCP server written from scratch in OCaml for learning purposes")
 (depends ocaml dune)
 (tags
  ("Server" "TCP" "OCaml" "From scratch" "multithread")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
