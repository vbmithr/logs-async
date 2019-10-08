(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core_kernel
open Async

let reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
        | Logs.App -> Writer.write stdout (app_flush ())
        | _ -> Writer.write stderr (dst_flush ()) in
      let unblock () =
        Writer.flushed stdout >>= fun () ->
        Writer.flushed stderr >>|
        over in
      don't_wait_for @@
      Monitor.protect (fun () -> write () ; Deferred.unit) ~finally:unblock ;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }

let level_arg =
  let complete _ ~part =
    List.filter ~f:(String.is_prefix ~prefix:part)
      ["app"; "error"; "warning"; "info"; "debug"] in
  Command.Arg_type.create ~complete begin fun s ->
    match Logs.level_of_string s with
    | Ok l -> l
    | Error (`Msg msg) -> failwithf "Unknown level %s" msg ()
  end

let set_level_via_param
    ?(arg_name="log-level") ?(doc="LEVEL The log level") src =
  let open Command.Param in
  map (flag arg_name (optional level_arg) ~doc) ~f:begin fun l ->
    match l, src with
    | None, _ -> ()
    | Some l, [] -> Logs.set_level ~all:true l
    | Some l, srcs -> List.iter srcs ~f:(fun src -> Logs.Src.set_level src l)
  end

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
