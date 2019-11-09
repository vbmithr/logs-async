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
    [AI_SOCKTYPE SOCK_DGRAM] >>= function
  | { ai_addr = ADDR_INET (h, port) ; _ } :: _ ->
    let sock = Socket.(create Type.udp) in
    Socket.connect sock (Socket.Address.Inet.create h ~port)
  | _ -> failwith "ovh_reporter: name resolve failed"

let fd_writer_of_sock sock =
  let fd = Fd.file_descr_exn (Socket.fd sock) in
  match Iobuf.send_nonblocking_no_sigpipe () with
  | Error e -> Error.raise e
  | Ok writer -> fd, writer

let maybe_send sendf level msg =
  let msg = Format.asprintf "%a" Rfc5424.pp msg in
  begin match level with
    | Logs.App -> print_endline msg
    | _ -> prerr_endline msg
  end ;
  match sendf with
  | None -> ()
  | Some (writer, fd) ->
    ignore (writer (Iobuf.of_string msg) fd)

let warp10 (type a) (t:a Rfc5424.Tag.typ) (v:a) =
  match t with
  | Rfc5424.Tag.String -> Warp10.String v
  | Rfc5424.Tag.Bool   -> Warp10.Bool v
  | Rfc5424.Tag.Float  -> Warp10.Double v
  | Rfc5424.Tag.I64    -> Warp10.Long v
  | Rfc5424.Tag.U64    -> Warp10.Long (Uint64.to_int64 v)
  | Rfc5424.Tag.U      -> Warp10.Bool true

let warp10_of_tags defs tags =
  let open Rfc5424 in
  let q = Queue.create () in
  List.iter defs ~f:begin fun ((Tag.Dyn (t, _d)) as tydef) ->
    match Tag.find t tydef tags with
    | None -> ()
    | Some (_, None) -> ()
    | Some (d, Some v) ->
      Warp10.create ~name:(Logs.Tag.name d) (warp10 t v) |>
      Queue.enqueue q
  end ;
  q

let make_reporter ?(defs=[]) ?logs ?metrics logf =
  let p =
    Option.map metrics ~f:begin fun uri ->
      let warp10_r, warp10_w = Pipe.create () in
      don't_wait_for (Warp10_async.record uri warp10_r) ;
      warp10_w
    end in
  let send_metrics_from_tags tags =
    match p with
    | Some p when (not (Pipe.is_closed p)) -> begin
        Monitor.try_with_or_error begin fun () ->
          Pipe.transfer_in p ~from:(warp10_of_tags defs tags)
        end >>= function
        | Error e ->
          Logs_async.err (fun m -> m "%a" Error.pp e)
        | Ok () -> Deferred.unit
      end
    | _ -> Deferred.unit in
  let token =
    let open Option.Monad_infix in
    logs >>= Uri.user >>| fun token ->
    Logs.Tag.(add ovhtoken token empty) in
  let tokens =
    match token with
    | None -> Logs.Tag.empty
    | Some tags -> tags in
  let tokens =
    if Logs.Tag.is_empty tokens then []
    else
      [Rfc5424.create_sd_element
         ~defs:[Rfc5424.Tag.string ovhtoken]
         ~section:"tokens" ~tags:tokens] in
  let hostname = Unix.gethostname () in
  let app_name = Filename.basename Sys.executable_name in
  let procid = Pid.to_string (Unix.getpid ()) in
  let pf = Rfc5424.create
      ?tz_offset_s:(Ptime_clock.current_tz_offset_s ())
      ~hostname ~procid in
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  let report_monitor = Monitor.create () in
  let report src level ~over k msgf =
    let m ?header:_ ?(tags=Logs.Tag.empty) fmt =
      let othertags =
        Rfc5424.create_sd_element ~defs ~section:"logs" ~tags in
      let structured_data =
        if Logs.Tag.is_empty tags
        then tokens
        else othertags :: tokens in
      let pf = pf
          ~severity:(Rfc5424.severity_of_level level)
          ~app_name:(app_name ^ "." ^ Logs.Src.name src)
          ~structured_data in
      let prlog msg =
        send_metrics_from_tags tags >>= fun () ->
        logf level
          (pf ~ts:(Ptime_clock.now ()) ~msg:(`Ascii msg) ()) >>= fun () ->
        Writer.flushed stdout >>= fun () ->
        Writer.flushed stderr in
      let print_log msg =
        Scheduler.within' ~monitor:report_monitor (fun () -> prlog msg) >>>
        over ;
        k () in
      Format.kasprintf print_log fmt
    in
    msgf m
  in
  Deferred.return { Logs.report = report }

let udp_reporter ?defs ?logs ?metrics () =
  begin match logs with
    | None -> Deferred.return None
    | Some url ->
      obtain_socket url >>| fun sock ->
      let fd, w = fd_writer_of_sock sock in
      Some (w, fd)
  end >>= fun sendf ->
  make_reporter ?defs ?logs ?metrics
    (fun l m -> maybe_send sendf l m; Deferred.unit)

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
