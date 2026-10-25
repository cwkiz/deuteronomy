(lang dune 3.21)

(name deuteronomy)

(generate_opam_files true)

(source
 (github cwkiz/deuteronomy))

(authors "Demetrius <illegalbassmedicine@proton.me>")

(maintainers "Demetrius <illegalbassmedicine@proton.me>")

(license LICENSE)

(documentation https://github.com/cwkiz/deuteronomy)

(package
 (name deuteronomy)
 (synopsis "Easy agents in OCaml")
 (description "Easy agents in OCaml")
 (depends ocaml lwt yojson ppx_deriving_yojson)
 (tags ("llm" "agents")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
