(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core
open Async

let ovhtoken =
  Logs.Tag.def ~doc:"OVH id token" "X-OVH-TOKEN" Format.pp_print_string

let send_tcp_tls uri =
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  let conn = Mvar.create () in
  let host = Uri.host_with_default ~default:"" uri in
  let port = Option.value ~default:0 (Uri.port uri) in
  let hp = Host_and_port.create ~host ~port in
  let tcp_hp = Tcp.Where_to_connect.of_host_and_port hp in
  let rec loop_connect () =
    Monitor.try_with begin fun () ->
      Tcp.connect tcp_hp >>= fun (_, r, w) ->
      let app_to_ssl, app_to_ssl_w = Pipe.create () in
      let ssl_to_app_r, ssl_to_app = Pipe.create () in
      let net_to_ssl = Reader.pipe r in
      let ssl_to_net = Writer.pipe w in
      begin Async_ssl.Std.Ssl.client
          ~app_to_ssl ~ssl_to_app
          ~net_to_ssl ~ssl_to_net () >>= function
        | Error err ->
          Format.eprintf "%a" Error.pp err ;
          Error.raise err
        | Ok conn -> return conn
      end >>= fun _ssl_conn ->
      Mvar.update conn ~f:(fun _ -> app_to_ssl_w) ;
      Deferred.any [ Reader.close_finished r ;
                     Writer.close_started w ] >>= fun () ->
      let _ = Mvar.take_now_exn conn in
      Pipe.close_read app_to_ssl ;
      Pipe.close_read ssl_to_app_r ;
      Pipe.close_read net_to_ssl ;
      Pipe.close ssl_to_net ;
      Pipe.close app_to_ssl_w ;
      Pipe.close ssl_to_app ;
      begin if not (Reader.is_closed r) then
          Reader.close r else Deferred.unit
      end >>= fun () ->
      begin if not (Writer.is_closed w) then
          Writer.close w else Deferred.unit
      end
    end >>= fun _ ->
    Clock_ns.after (Time_ns.Span.of_int_sec 3) >>=
    loop_connect in
  don't_wait_for @@ loop_connect () ;
  return @@ fun level msg ->
  let msg_str = Format.asprintf "%a@." Rfc5424.pp msg in
  Mvar.value_available conn >>= fun () ->
  let w = Mvar.peek_exn conn in
  begin match level with
    | Logs.App -> Writer.write stdout msg_str
    | _ -> Writer.write stderr msg_str
  end ;
  Pipe.write w msg_str

let send_udp uri =
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  let service = Option.(Uri.port uri >>| Int.to_string) in
  let sock = Socket.(create Type.udp) in
  begin
    Unix.Addr_info.get ?service
      ~host:(Uri.host_with_default ~default:"" uri)
      [AI_SOCKTYPE SOCK_DGRAM] >>= function
    | { ai_addr = ADDR_INET (h, port) ; _ } :: _ ->
      Socket.connect sock (Socket.Address.Inet.create h port)
    | _ -> failwith "ovh_reporter: name resolve failed"
  end >>| fun sock ->
  let fd = Socket.fd sock in
  let send_fun =
    match Udp.send () with
    | Ok a -> a
    | Error err -> raise (Error.to_exn err) in
  fun level msg ->
    let msg = Format.asprintf "%a@." Rfc5424.pp msg in
    begin match level with
      | Logs.App -> Writer.write stdout msg
      | _ -> Writer.write stderr msg
    end ;
    send_fun fd (Iobuf.of_string msg)

let make_reporter make_f ~uri ~token =
  let ovhtag = Logs.Tag.(add ovhtoken token empty) in
  let ovhtag = "ovh", ovhtag in
  let hostname = Unix.gethostname () in
  let procid = Pid.to_string (Unix.getpid ()) in
  let pf = Rfc5424.create ~hostname ~procid in
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  make_f uri >>= fun f ->
  let report src level ~over k msgf =
    let m ?header ?(tags=Logs.Tag.empty) fmt =
      let tags =
        if Logs.Tag.is_empty tags
        then [ovhtag]
        else [ovhtag; "logs", tags] in
      let pf = pf
        ~severity:(Rfc5424.severity_of_level level)
        ~app_name:(Logs.Src.name src)
        ~tags ~ts:(Ptime_clock.now ()) in
      let k msg =
        don't_wait_for @@
        Monitor.protect
          (fun () -> f level (pf ~msg ())) ~finally:begin fun () ->
          Writer.flushed stdout >>= fun () ->
          Writer.flushed stderr >>| fun () ->
          over ()
        end ;
        k () in
      Format.kasprintf k fmt
    in
    msgf m
  in
  Deferred.return { Logs.report = report }

let udp_reporter = make_reporter send_udp
let tcp_tls_reporter = make_reporter send_tcp_tls

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
