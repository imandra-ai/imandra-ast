(lang dune 2.0)
(name imandra-ast)
(generate_opam_files)
(version 0.1)

(package
  (name imandra-ast)
  (depends
    (yojson (>= 1.6))
    ppx_deriving
    ppx_deriving_yojson
    (zarith (>= 1.12))
    (containers (and (>= 3.6) (< 4.)))))
