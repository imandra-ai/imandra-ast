(lang dune 2.0)
(name imandra-ast)
(generate_opam_files)
(version 0.1)
(license MIT)
(maintainers "the Imandra team")
(homepage "https://github.com/imandra-ai/imandra-ast/")
(bug_reports "https://github.com/imandra-ai/imandra-ast/issues")
(authors "the Imandra team")

(package
  (name imandra-ast)
  (depends
    (yojson (>= 1.6))
    cmdliner
    ppx_deriving
    ppx_deriving_yojson
    yojson
    cbor
    (zarith (>= 1.12))
    (containers (and (>= 3.7) (< 4.)))))
