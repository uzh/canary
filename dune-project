(lang dune 2.9)

(generate_opam_files true)

(name canary)

(version 0.0.3)

(license GPL-2.0-or-later)

(authors
 "Department of Economics, University of Zurich"
 "John Christopher McAlpine")

(maintainers "engineering@econ.uzh.ch")

(source
 (github uzh/canary))

(homepage "https://github.com/uzh/canary")

(bug_reports "https://github.com/uzh/canary/issues")

(documentation "https://github.com/uzh/canary")

(package
 (name "canary")
 (synopsis
  "Capture unhandled exceptions and automatically report them through various channels")
 (description
  "Developed by the University of Zurich, Canary is a tool for capturing unhandled exceptions and automatically reporting them to your development team.")
 (depends
  dune
  (dune-release :with-dev-setup)
  (ocaml
   (>= 4.08.0))
  cohttp
  cohttp-lwt
  cohttp-lwt-unix
  containers
  lwt
  lwt_ppx
  (ocamlformat :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)
  ppx_deriving_yojson
  ptime
  (ssl
   (and
    (>= 0.7.0)
    :with-test))
  (lwt_ssl
   (and
    (>= 1.2.0)
    :with-test))
  yojson))
