(** Submission to the systemd journal, when the [systemd] library was
    available at build time.

    The reporter picks its backend through a dune [select], so a switch
    without [ocaml-systemd] -- or a host without libsystemd at all --
    still builds, with {!available} false and {!send} a no-op. *)

val available : bool

(** [send fields] submits one journal entry. Field names must be
    uppercase and journald expects [MESSAGE] and [PRIORITY] among them. *)
val send : (string * string) list -> unit
