(lang dune 2.9)

(name canary)

(generate_opam_files true)

(maintainers "John Christopher McAlpine <christophermcalpine@gmail.com>")

(package
 (name "canary")
 (version "0.0.1")
 (synopsis
  "Capture unhandled exceptions and automatically report them through various channels")
 (description
  "Developed by the University of Zurich, Canary is a tool for capturing unhandled exceptions and automatically reporting them to your development team.")
 (depends
  dune
  (opam
   (< 2.1))
  (ocaml
   (and
    (>= 4.08.0)
    (< 4.13.0)))
  lwt
  ptime
  cohttp
  cohttp-lwt
  cohttp-lwt-unix
  yojson
  lwt_ppx
  ppx_deriving_yojson))
