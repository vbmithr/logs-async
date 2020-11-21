(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core
open Async

let ovhtoken =
  Logs.Tag.def ~doc:"OVH id token" "X-OVH-TOKEN" Format.pp_print_string

let obtain_socket url =
  Unix.Addr_info.get
    ?service:(Option.map (Uri.port url) ~f:Int.to_string)
    ~host:(Uri.host_with_default ~default:"" url)
    [AI_SOCKTYPE SOCK_DGRAM]
  >>= function
  | {ai_addr= ADDR_INET (h, port); _} :: _ ->
      let sock = Socket.(create Type.udp) in
      Socket.connect sock (Socket.Address.Inet.create h ~port)
  | _ -> failwith "ovh_reporter: name resolve failed"

let fd_writer_of_sock sock =
  let fd = Fd.file_descr_exn (Socket.fd sock) in
  match Iobuf_unix.send_nonblocking_no_sigpipe () with
  | Error e -> Error.raise e
  | Ok writer -> (fd, writer)

let maybe_send sendf level msg =
  let msg = Format.asprintf "%a" Rfc5424.pp msg in
  (match level with Logs.App -> print_endline msg | _ -> prerr_endline msg) ;
  match sendf with
  | None -> ()
  | Some (writer, fd) -> ignore (writer (Iobuf.of_string msg) fd)

let ptime_of_time t =
  let ns = Time_ns.to_int63_ns_since_epoch t in
  Option.value_exn (Ptime.of_float_s (Int63.to_float ns /. 1e9))

let make_reporter ?(defs = []) ?ovh_url logf =
  let token =
    let open Option.Monad_infix in
    ovh_url >>= Uri.user >>| fun token -> Logs.Tag.(add ovhtoken token empty)
  in
  let tokens = match token with None -> Logs.Tag.empty | Some tags -> tags in
  let tokens =
    if Logs.Tag.is_empty tokens then []
    else
      [ Rfc5424.create_sd_element
          ~defs:[Rfc5424.Tag.string ovhtoken]
          ~section:"tokens" ~tags:tokens ] in
  let hostname = Unix.gethostname () in
  let app_name = Filename.basename Sys.executable_name in
  let procid = Pid.to_string (Unix.getpid ()) in
  let zone = Time_ns.get_sexp_zone () in
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  let report_monitor = Monitor.create () in
  let report src level ~over k msgf =
    let m ?header:_ ?(tags = Logs.Tag.empty) fmt =
      let othertags = Rfc5424.create_sd_element ~defs ~section:"logs" ~tags in
      let structured_data =
        if Logs.Tag.is_empty tags then tokens else othertags :: tokens in
      let ts, tz_offset_s =
        let ts = Time_ns.now () in
        (ts, Time_ns.(Span.to_int_sec (utc_offset ts ~zone))) in
      let pf =
        Rfc5424.create ~tz_offset_s ~ts:(ptime_of_time ts) ~hostname ~procid
          ~severity:(Rfc5424.severity_of_level level)
          ~app_name:(app_name ^ "." ^ Logs.Src.name src)
          ~structured_data in
      let prlog msg =
        logf level (pf ~msg:(`Ascii msg) ())
        >>= fun () -> Writer.flushed stdout >>= fun () -> Writer.flushed stderr
      in
      let print_log msg =
        Scheduler.within' ~monitor:report_monitor (fun () -> prlog msg) >>> over ;
        k () in
      Format.kasprintf print_log fmt in
    msgf m in
  Deferred.return {Logs.report}

let udp_reporter ?defs ?ovh_url () =
  ( match ovh_url with
  | None -> Deferred.return None
  | Some url ->
      obtain_socket url
      >>| fun sock ->
      let fd, w = fd_writer_of_sock sock in
      Some (w, fd) )
  >>= fun sendf ->
  make_reporter ?defs ?ovh_url (fun l m -> maybe_send sendf l m ; Deferred.unit)

let udp_or_systemd_reporter () =
  match Option.map (Sys.getenv "OVH_LOGS_URL") ~f:Uri.of_string with
  | None ->
      return Logs_async_reporter.(reporter ~pp_header:pp_systemd_header ())
  | Some ovh_url -> udp_reporter ~ovh_url ()

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
