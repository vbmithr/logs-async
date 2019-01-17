(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type t = {
  header : header ;
  tags : (string * Logs.Tag.set) list ;
  msg : string option ;
}

and header = {
  facility : facility ;
  severity : severity ;
  version : int ;
  ts : Ptime.t ;
  hostname : string option ;
  app_name : string option ;
  procid : int option ;
  msgid : int option ;
}

and facility =
  | Kernel_Message
  | User_Level_Messages
  | Mail_System
  | System_Daemons
  | Security_Authorization_Messages
  | Messages_Generated_Internally_By_Syslogd
  | Line_Printer_Subsystem
  | Network_News_Subsystem
  | UUCP_subsystem
  | Clock_Daemon
  | Security_Authorization_Messages_10
  | Ftp_Daemon
  | Ntp_Subsystem
  | Log_Audit
  | Log_Alert
  | Clock_Daemon_15
  | Local0
  | Local1
  | Local2
  | Local3
  | Local4
  | Local5
  | Local6
  | Local7

and severity =
  | Emergency
  | Alert
  | Critical
  | Error
  | Warning
  | Notice
  | Informational
  | Debug

val int_of_facility : facility -> int
val facility_of_int : int -> facility option
val string_of_facility : facility -> string
val pp_print_facility : Format.formatter -> facility -> unit

val int_of_severity : severity -> int
val severity_of_int : int -> severity option
val string_of_severity : severity -> string
val pp_print_severity : Format.formatter -> severity -> unit

val pp : Format.formatter -> t -> unit
val to_string : t -> string
val show : t -> string

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
