(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core
open Async

let app_style = `Cyan
let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green

let pp_header ~pp_h ppf (s, l, h, _t) =
  match l with
  | Logs.App -> (
    match h with
    | None -> ()
    | Some h -> Fmt.pf ppf "[%a] " Fmt.(styled app_style string) h )
  | Logs.Error ->
      pp_h ppf err_style s (match h with None -> "ERROR" | Some h -> h)
  | Logs.Warning ->
      pp_h ppf warn_style s (match h with None -> "WARNING" | Some h -> h)
  | Logs.Info ->
      pp_h ppf info_style s (match h with None -> "INFO" | Some h -> h)
  | Logs.Debug ->
      pp_h ppf debug_style s (match h with None -> "DEBUG" | Some h -> h)

let pp_exec_header =
  let pp_h ppf style s h =
    Fmt.pf ppf " %a %15s [%a] " Int63.pp
      Time_stamp_counter.(now () |> to_int63)
      (Logs.Src.name s)
      Fmt.(styled style string)
      h in
  pp_header ~pp_h

let format_reporter ?(pp_header = pp_exec_header) ?(app = Format.std_formatter)
    ?(dst = Format.err_formatter) () =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    msgf
    @@ fun ?header ?tags fmt ->
    let ppf = match level with Logs.App -> app | _ -> dst in
    Format.kfprintf k ppf
      ("%a@[" ^^ fmt ^^ "@]@.")
      pp_header (src, level, header, tags) in
  ({report} : Logs.reporter)

let reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b,
      fun () ->
        let m = Buffer.contents b in
        Buffer.reset b ; m ) in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = format_reporter ?app:(Some app) ?dst:(Some dst) () in
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  let report src level ~over k msgf =
    let k () =
      let write () =
        match level with
        | Logs.App -> Writer.write stdout (app_flush ())
        | _ -> Writer.write stderr (dst_flush ()) in
      let finally () =
        match level with
        | Logs.App -> Writer.flushed stdout >>| over
        | _ -> Writer.flushed stderr >>| over in
      don't_wait_for
      @@ Monitor.protect (fun () -> write () ; Deferred.unit) ~finally ;
      k () in
    reporter.report src level ~over:(fun () -> ()) k msgf in
  {Logs.report}

let level_arg =
  let complete _ ~part =
    List.filter
      ~f:(String.is_prefix ~prefix:part)
      ["app"; "error"; "warning"; "info"; "debug"] in
  Command.Arg_type.create ~complete (fun s ->
      match Logs.level_of_string s with
      | Ok l -> l
      | Error (`Msg msg) -> failwithf "Unknown level %s" msg () )

let set_level_via_param ?(arg_name = "log-level") ?(doc = "LEVEL The log level")
    src =
  let open Command.Param in
  map
    (flag arg_name (optional level_arg) ~doc)
    ~f:(fun l ->
      match (l, src) with
      | None, _ -> ()
      | Some l, [] -> Logs.set_level ~all:true l
      | Some l, srcs -> List.iter srcs ~f:(fun src -> Logs.Src.set_level src l)
      )

let parseColor = function
  | "never" -> `Never
  | "always" -> `Always
  | "auto" -> `Auto
  | msg -> failwithf "Unknown color spec %s" msg ()

let color_arg =
  let complete _ ~part =
    List.filter ~f:(String.is_prefix ~prefix:part) ["never"; "always"; "auto"]
  in
  Command.Arg_type.create ~complete parseColor

let set_color_via_param ?(arg_name = "color")
    ?(doc = "STRING Use ANSI color in terminal (never|always|auto)") () =
  let open Command.Param in
  map
    (flag arg_name (optional color_arg) ~doc)
    ~f:(function
      | Some `Never -> Fmt_tty.setup_std_outputs ~style_renderer:`None ()
      | Some `Always -> Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ()
      | Some `Auto | None -> Fmt_tty.setup_std_outputs () )

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
