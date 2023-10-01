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
          Format.kasprintf (fun v -> (name, `String v) :: a) "%a" (Logs.Tag.printer def) x
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

let reporter () =
  match Sys.getenv "KUBERNETES_SERVICE_HOST", Sys.getenv "DISABLE_JSON_LOGGING" with
  | None, _ | Some _, Some _ -> format_reporter ()
  | Some _, None -> json_reporter ()
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
