(lang dune 3.10)

(name logs-async)

(generate_opam_files true)

(source
 (github vbmithr/logs-async))

(authors "Vincent Bernardoff <vb@luminar.eu.org>")

(maintainers "Vincent Bernardoff <vb@luminar.eu.org>")

(license ISC)

(documentation https://vbmithr.github.io/logs-async)

(package
 (name logs-async)
 (synopsis "Jane Street Async logging with Logs")
 (description "This is analogous to the Logs_lwt module in the logs package.
The log functions of this module return Async threads that proceed only
when the log operation is over, as defined by the current
Logs.reporter.")
 (depends
   ocaml
   dune
  (logs (>= 0.7.0))
  (async_kernel (>= v0.16)))
 (tags
  (async logs)))

(package
 (name logs-async-reporter)
 (synopsis "Logs reporter compatible with Async")
 (description "Logs reporter that will play nice with Async's runtime.")
 (depends
   ocaml
   dune
  (logs (>= 0.7.0))
  (fmt (>= 0.9.0))
  (core (>= v0.16))
  (async (>= v0.16))
  (zstandard (>= v0.16))
  (yojson (>= 2.1.0))
 )
 (tags
  (async logs)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
