(*---------------------------------------------------------------------------
  Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
  Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)
open Core
open Async
open Zstandard.Streaming

(** [reporter ()] picks a format from the environment: [LOGS_FORMAT] if
    it is set (["auto"], ["journal"], ["json"] or ["plain"]), else the
    systemd journal when stderr is the journal, JSON under Kubernetes,
    and the human-readable stderr format otherwise. *)
val reporter : ?identifier:string -> unit -> Logs.reporter

(** [journald_reporter ()] submits entries to the systemd journal
    natively, letting journald supply the timestamp, pid and unit, and
    carrying the level as PRIORITY, the Logs source as LOGS_SRC and each
    tag as a TAG_-prefixed field. [identifier] defaults to
    [$SYSLOG_IDENTIFIER], then to the executable's basename.

    Without [ocaml-systemd] at build time this reporter drops every
    entry; {!reporter} never selects it in such a build. *)
val journald_reporter : ?identifier:string -> unit -> Logs.reporter

(** [stderr_is_journal ()] is whether stderr is the stream systemd named
    in [$JOURNAL_STREAM], matched by device and inode. Both that variable
    and [$INVOCATION_ID] are inherited by children -- and on a host whose
    display manager runs as a unit, every process in the session has
    them -- so their mere presence is not the question. *)
val stderr_is_journal : unit -> bool

val json_reporter : unit -> Logs.reporter
val output_reporter : (bytes -> int -> int -> unit) -> Logs.reporter

val zstd_reporter
  :  ?zstd:Compression.t
  -> ?inbuf:Bigbuffer.t
  -> ?outbuf:Bigstring.t
  -> Writer.t
  -> Logs.reporter * (unit -> unit)

(** Argument type to be used for use in [Command] params. *)
val level_arg : Logs.level option Async.Command.Arg_type.t

(** [set_level_via_param src] is a param that sets the level of [srcs]
    (or all srcs if [src] is [None]). *)
val set_level_via_param
  :  ?arg_name:string
  -> ?doc:string
  -> Logs.src list
  -> unit Async.Command.Param.t

(** [set_color_via_param src] is a param that sets the level of [srcs]
    (or all srcs if [src] is [None]). *)
val set_color_via_param
  :  ?arg_name:string
  -> ?doc:string
  -> unit
  -> unit Async.Command.Param.t

(*---------------------------------------------------------------------------
  Copyright (c) 2019 Vincent Bernardoff

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
