(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

val reporter : unit -> Logs.reporter

val level_arg : Logs.level option Async.Command.Arg_type.t
(** Argument type to be used for use in [Command] params. *)

val set_level_via_param :
  ?arg_name:string -> ?doc:string -> Logs.src list -> unit Async.Command.Param.t
(** [set_level_via_param src] is a param that sets the level of [srcs]
    (or all srcs if [src] is [None]). *)

val set_color_via_param :
  ?arg_name:string -> ?doc:string -> unit -> unit Async.Command.Param.t
(** [set_color_via_param src] is a param that sets the level of [srcs]
    (or all srcs if [src] is [None]). *)

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
