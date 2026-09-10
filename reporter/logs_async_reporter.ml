(*---------------------------------------------------------------------------
  Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
  Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core
open Async
module Time_ns = Time_ns_unix

let app_style = `Cyan
let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green

let pp_header ~pp_h ppf (s, l, h, t) =
  match l with
  | Logs.App -> Option.iter h ~f:(Fmt.pf ppf "[%a] " Fmt.(styled app_style string))
  | Logs.Error -> pp_h ppf err_style s (Option.value h ~default:"ERROR") t
  | Logs.Warning -> pp_h ppf warn_style s (Option.value h ~default:"WARNING") t
  | Logs.Info -> pp_h ppf info_style s (Option.value h ~default:"INFO") t
  | Logs.Debug -> pp_h ppf debug_style s (Option.value h ~default:"DEBUG") t
;;

let pp_exec_header =
  let pp_h ppf style s h t =
    Fmt.pf
      ppf
      " %a %15s [%a] %a"
      Time_ns.pp
      Time_stamp_counter.(now () |> to_time_ns ~calibrator:(Lazy.force calibrator))
      (Logs.Src.name s)
      Fmt.(styled style string)
      h
      (Fmt.option Logs.Tag.pp_set)
      t
  in
  pp_header ~pp_h
;;

let pp_exec_header_nocolor =
  let pp_h ppf _style s h t =
    Fmt.pf
      ppf
      " %a %15s [%s] %a"
      Time_ns.pp
      Time_stamp_counter.(now () |> to_time_ns ~calibrator:(Lazy.force calibrator))
      (Logs.Src.name s)
      h
      (Fmt.option Logs.Tag.pp_set)
      t
  in
  pp_header ~pp_h
;;

let format_reporter
      ?(pp_header = pp_exec_header)
      ?(app = Format.std_formatter)
      ?(dst = Format.err_formatter)
      ()
  =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    msgf
    @@ fun ?header ?tags fmt ->
    let ppf =
      match level with
      | Logs.App -> app
      | _ -> dst
    in
    Format.kfprintf k ppf ("%a@[" ^^ fmt ^^ "@]@.") pp_header (src, level, header, tags)
  in
  { Logs.report }
;;

let json_reporter () =
  let buf = Buffer.create 4096 in
  let report src level ~over k msgf =
    let k () =
      over ();
      k ()
    in
    msgf
    @@ fun ?header ?tags fmt ->
    Format.kfprintf
      (fun _fmt ->
         let msg = Format.flush_str_formatter () in
         let currentTS =
           Time_stamp_counter.(now () |> to_time_ns ~calibrator:(Lazy.force calibrator))
         in
         let assc = [] in
         let add_tag (Logs.Tag.V (def, x)) a =
           let name = Logs.Tag.name def in
           Format.kasprintf
             (fun v -> (name, `String v) :: a)
             "%a"
             (Logs.Tag.printer def)
             x
         in
         let assc =
           match tags with
           | None -> assc
           | Some tags -> Logs.Tag.fold add_tag tags assc
         in
         let assc =
           match header with
           | None -> assc
           | Some h -> ("hdr", `String h) :: assc
         in
         let assc =
           List.rev_append
             [ "msg", `String msg
             ; "level", `String (Logs.level_to_string (Some level))
             ; "caller", `String (Logs.Src.name src)
             ; "ts", `Float Time_ns.(to_span_since_epoch currentTS |> Span.to_sec)
             ]
             assc
         in
         Buffer.clear buf;
         Yojson.Safe.to_buffer ~std:true buf (`Assoc assc);
         Writer.write_line (Lazy.force Writer.stdout) (Buffer.contents buf);
         k ())
      Format.str_formatter
      fmt
  in
  { Logs.report }
;;

(* An empty value is not a value: a variable blanked rather than unset by
   whatever spawned us -- and spawners do that -- must read as absent,
   not as a setting. *)
let getenv_nonempty name =
  match Sys.getenv name with
  | Some "" | None -> None
  | Some v -> Some v
;;

(* systemd stamps every entry with the wall clock, the pid, the unit and
   the syslog identifier, and keeps the level in PRIORITY -- so of what
   {!pp_exec_header} prints, only the Logs source is news to the journal.
   That one travels in the message text, where journalctl's default
   output shows it, and again as an indexed field so it can be filtered
   on. *)
let syslog_priority = function
  | Logs.App -> 5 (* notice: program output is normal, but significant *)
  | Logs.Error -> 3
  | Logs.Warning -> 4
  | Logs.Info -> 6
  | Logs.Debug -> 7
;;

(* Journal field names are uppercase alphanumerics and underscores, and
   may not start with an underscore -- that namespace is journald's own,
   for the fields it attests to. A tag name is arbitrary, so it is
   sanitised and prefixed rather than trusted. *)
let journal_field_of_tag name =
  let keep c =
    match c with
    | 'A' .. 'Z' | '0' .. '9' -> c
    | _ -> '_'
  in
  "TAG_" ^ String.map (String.uppercase name) ~f:keep
;;

(* The identifier is what journalctl shows before the pid and what
   [journalctl -t] matches on. For output piped to the journal systemd
   supplies it from the unit's [SyslogIdentifier=]; a native send has to
   state it, and a unit that wants more than the executable name (one
   binary serving several units, say) passes it in the environment. *)
let default_identifier () =
  match getenv_nonempty "SYSLOG_IDENTIFIER" with
  | Some id -> id
  | None -> Filename.basename Sys.executable_name
;;

let journald_reporter ?identifier () =
  let identifier = Option.value_or_thunk identifier ~default:default_identifier in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let report src level ~over k msgf =
    msgf
    @@ fun ?header ?tags fmt ->
    let fields () =
      let add_tag (Logs.Tag.V (def, x)) a =
        Format.kasprintf
          (fun v -> (journal_field_of_tag (Logs.Tag.name def), v) :: a)
          "%a"
          (Logs.Tag.printer def)
          x
      in
      let tags =
        match tags with
        | None -> []
        | Some tags -> Logs.Tag.fold add_tag tags []
      in
      ("MESSAGE", Buffer.contents buf)
      :: ("PRIORITY", Int.to_string (syslog_priority level))
      :: ("SYSLOG_IDENTIFIER", identifier)
      :: ("LOGS_SRC", Logs.Src.name src)
      :: tags
    in
    let k _ =
      Format.pp_print_flush ppf ();
      Journal_backend.send (fields ());
      over ();
      k ()
    in
    (* The level is PRIORITY and the time is journald's, but a header a
       call site set by hand is neither, so it stays in the text. *)
    let pp_header ppf = function
      | None -> ()
      | Some h -> Format.fprintf ppf "[%s] " h
    in
    Buffer.clear buf;
    Format.kfprintf
      k
      ppf
      ("%s: %a@[" ^^ fmt ^^ "@]")
      (Logs.Src.name src)
      pp_header
      header
  in
  { Logs.report }
;;

(* Presence of an environment variable proves nothing here: children
   inherit it. [INVOCATION_ID] in particular is set for every process in
   a graphical session whose display manager runs as a unit, and
   [JOURNAL_STREAM] survives any redirection of the stream it names. So
   ask the question that actually matters -- is stderr the journal? --
   the way systemd's own log.c does, by matching the "device:inode" the
   variable carries against what stderr really is. *)
let stderr_is_journal () =
  match getenv_nonempty "JOURNAL_STREAM" with
  | None -> false
  | Some v ->
    (match String.lsplit2 (String.strip v) ~on:':' with
     | None -> false
     | Some (dev, ino) ->
       (try
          let st = Core_unix.fstat Core_unix.stderr in
          String.equal dev (Int.to_string st.st_dev)
          && String.equal ino (Int.to_string st.st_ino)
        with
        | _ -> false))
;;

(* An explicit [LOGS_FORMAT] settles it; otherwise the environment does,
   and a plain terminal keeps the timestamped, coloured format. A typo in
   the variable is refused rather than silently ignored: it is read once,
   at startup, and the alternative is discovering the mistake by missing
   logs. *)
let reporter ?identifier () =
  let auto () =
    match Sys.getenv "KUBERNETES_SERVICE_HOST", Sys.getenv "DISABLE_JSON_LOGGING" with
    | Some _, None -> json_reporter ()
    | None, _ | Some _, Some _ ->
      if Journal_backend.available && stderr_is_journal ()
      then journald_reporter ?identifier ()
      else format_reporter ()
  in
  match getenv_nonempty "LOGS_FORMAT" with
  | None | Some "auto" -> auto ()
  | Some ("journal" | "journald") ->
    if Journal_backend.available
    then journald_reporter ?identifier ()
    else failwith "LOGS_FORMAT=journal, but this build has no systemd support"
  | Some "json" -> json_reporter ()
  | Some ("plain" | "text") -> format_reporter ()
  | Some other ->
    failwithf "LOGS_FORMAT: expected auto, journal, json or plain, got %S" other ()
;;

let output_reporter writef =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b
    , fun () ->
        let m = Buffer.contents_bytes b in
        Buffer.reset b;
        m )
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter =
    format_reporter ~pp_header:pp_exec_header_nocolor ?app:(Some app) ?dst:(Some dst) ()
  in
  let report src level ~over k msgf =
    let k () =
      let write () =
        match level with
        | Logs.App ->
          let msg = app_flush () in
          writef msg 0 (Bytes.length msg)
        | _ ->
          let msg = dst_flush () in
          writef msg 0 (Bytes.length msg)
      in
      let finally () =
        over ();
        Deferred.unit
      in
      don't_wait_for
      @@ Monitor.protect
           (fun () ->
              write ();
              Deferred.unit)
           ~finally;
      k ()
    in
    reporter.report src level ~over:(fun () -> ()) k msgf
  in
  { Logs.report }
;;

open Zstandard.Streaming

let zstd_reporter
      ?(zstd = Compression.create 3)
      ?(inbuf = Bigbuffer.create 4096)
      ?(outbuf = Bigstring.create 4096)
      w
  =
  let outlen = Bigstring.length outbuf in
  let rec close_reporter () =
    let rem, nbW = Compression.endstream zstd ~outbuf ~outpos:0 ~outlen in
    if nbW > 0 then Writer.write_bigstring w outbuf ~pos:0 ~len:nbW;
    if rem > 0 then close_reporter ()
  in
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b
    , fun () ->
        let m = Buffer.contents_bytes b in
        Buffer.reset b;
        m )
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter =
    format_reporter ~pp_header:pp_exec_header_nocolor ?app:(Some app) ?dst:(Some dst) ()
  in
  let compress_all inbuf =
    let inbuf = Bigbuffer.big_contents inbuf in
    let rec loop inpos inlen =
      let nbR, nbW =
        Compression.compress zstd ~inbuf ~outbuf ~inpos ~inlen ~outpos:0 ~outlen
      in
      Writer.write_bigstring w outbuf ~pos:0 ~len:nbW;
      if nbR < inlen then loop (inpos + nbR) (inlen - nbR)
    in
    loop 0 (Bigstring.length inbuf)
  in
  let report src level ~over k msgf =
    let k () =
      let write () =
        match level with
        | Logs.App ->
          let src = app_flush () in
          Bigbuffer.clear inbuf;
          Bigbuffer.add_bytes inbuf src;
          compress_all inbuf
        | _ ->
          let src = dst_flush () in
          Bigbuffer.clear inbuf;
          Bigbuffer.add_bytes inbuf src;
          compress_all inbuf
      in
      let finally () =
        over ();
        Deferred.unit
      in
      don't_wait_for
      @@ Monitor.protect
           (fun () ->
              write ();
              Deferred.unit)
           ~finally;
      k ()
    in
    reporter.report src level ~over:(fun () -> ()) k msgf
  in
  { Logs.report }, close_reporter
;;

let level_arg =
  let complete _ ~part =
    List.filter
      ~f:(String.is_prefix ~prefix:part)
      [ "app"; "error"; "warning"; "info"; "debug" ]
  in
  Command.Arg_type.create ~complete (fun s ->
    match Logs.level_of_string s with
    | Ok l -> l
    | Error (`Msg msg) -> failwithf "Unknown level %s" msg ())
;;

let set_level_via_param ?(arg_name = "log-level") ?(doc = "LEVEL The log level") src =
  let open Command.Param in
  map
    (flag arg_name (optional level_arg) ~doc)
    ~f:(fun l ->
      match l, src with
      | None, _ -> ()
      | Some l, [] -> Logs.set_level ~all:true l
      | Some l, srcs -> List.iter srcs ~f:(fun src -> Logs.Src.set_level src l))
;;

let parseColor = function
  | "never" -> `Never
  | "always" -> `Always
  | "auto" -> `Auto
  | msg -> failwithf "Unknown color spec %s" msg ()
;;

let color_arg =
  let complete _ ~part =
    List.filter ~f:(String.is_prefix ~prefix:part) [ "never"; "always"; "auto" ]
  in
  Command.Arg_type.create ~complete parseColor
;;

let set_color_via_param
      ?(arg_name = "color")
      ?(doc = "STRING Use ANSI color in terminal (never|always|auto)")
      ()
  =
  let open Command.Param in
  map
    (flag arg_name (optional color_arg) ~doc)
    ~f:(function
      | Some `Never -> Fmt_tty.setup_std_outputs ~style_renderer:`None ()
      | Some `Always -> Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ()
      | Some `Auto | None -> Fmt_tty.setup_std_outputs ())
;;

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
