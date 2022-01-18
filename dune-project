(lang dune 2.9)

(name canary)

(generate_opam_files true)

(package
 (name "canary")
 (synopsis
  "Capture unhandled exceptions and automatically report them through various channels")
 (description
  "Developed by the University of Zurich, Canary is a tool for capturing unhandled exceptions and automatically reporting them to your development team.")
 (depends dune lwt lwt.unix ptime cohttp cohttp-lwt cohttp-lwt-unix yojson))
