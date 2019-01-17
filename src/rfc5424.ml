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

let int_of_facility = function
  | Kernel_Message -> 0
  | User_Level_Messages -> 1
  | Mail_System -> 2
  | System_Daemons -> 3
  | Security_Authorization_Messages -> 4
  | Messages_Generated_Internally_By_Syslogd -> 5
  | Line_Printer_Subsystem -> 6
  | Network_News_Subsystem -> 7
  | UUCP_subsystem -> 8
  | Clock_Daemon -> 9
  | Security_Authorization_Messages_10 -> 10
  | Ftp_Daemon -> 11
  | Ntp_Subsystem -> 12
  | Log_Audit -> 13
  | Log_Alert -> 14
  | Clock_Daemon_15 -> 15
  | Local0 -> 16
  | Local1 -> 17
  | Local2 -> 18
  | Local3 -> 19
  | Local4 -> 20
  | Local5 -> 21
  | Local6 -> 22
  | Local7 -> 23

let facility_of_int = function
  | 0  -> Some Kernel_Message
  | 1  -> Some User_Level_Messages
  | 2  -> Some Mail_System
  | 3  -> Some System_Daemons
  | 4  -> Some Security_Authorization_Messages
  | 5  -> Some Messages_Generated_Internally_By_Syslogd
  | 6  -> Some Line_Printer_Subsystem
  | 7  -> Some Network_News_Subsystem
  | 8  -> Some UUCP_subsystem
  | 9  -> Some Clock_Daemon
  | 10 -> Some Security_Authorization_Messages_10
  | 11 -> Some Ftp_Daemon
  | 12 -> Some Ntp_Subsystem
  | 13 -> Some Log_Audit
  | 14 -> Some Log_Alert
  | 15 -> Some Clock_Daemon_15
  | 16 -> Some Local0
  | 17 -> Some Local1
  | 18 -> Some Local2
  | 19 -> Some Local3
  | 20 -> Some Local4
  | 21 -> Some Local5
  | 22 -> Some Local6
  | 23 -> Some Local7
  | _  -> None

let string_of_facility = function
  | Kernel_Message -> "kern"
  | User_Level_Messages -> "user"
  | Mail_System -> "mail"
  | System_Daemons -> "daemon"
  | Security_Authorization_Messages -> "security/auth"
  | Messages_Generated_Internally_By_Syslogd -> "syslog"
  | Line_Printer_Subsystem -> "lpr"
  | Network_News_Subsystem -> "news"
  | UUCP_subsystem -> "uucp"
  | Clock_Daemon -> "clock"
  | Security_Authorization_Messages_10 -> "security/auth-10"
  | Ftp_Daemon -> "ftp"
  | Ntp_Subsystem -> "ntp"
  | Log_Audit -> "log-audit"
  | Log_Alert -> "log-alert"
  | Clock_Daemon_15 -> "clock-15"
  | Local0 -> "local0"
  | Local1 -> "local1"
  | Local2 -> "local2"
  | Local3 -> "local3"
  | Local4 -> "local4"
  | Local5 -> "local5"
  | Local6 -> "local6"
  | Local7 -> "local7"

let pp_print_facility ppf f =
  Format.pp_print_string ppf (string_of_facility f)

let int_of_severity = function
  | Emergency -> 0
  | Alert -> 1
  | Critical -> 2
  | Error -> 3
  | Warning -> 4
  | Notice -> 5
  | Informational -> 6
  | Debug -> 7

let severity_of_int = function
  | 0 -> Some Emergency
  | 1 -> Some Alert
  | 2 -> Some Critical
  | 3 -> Some Error
  | 4 -> Some Warning
  | 5 -> Some Notice
  | 6 -> Some Informational
  | 7 -> Some Debug
  | _ -> None

let string_of_severity = function
  | Emergency -> "emerg"
  | Alert -> "alert"
  | Critical -> "crit"
  | Error -> "err"
  | Warning -> "warning"
  | Notice -> "notice"
  | Informational -> "info"
  | Debug -> "debug"

let pp_print_severity ppf f =
  Format.pp_print_string ppf (string_of_severity f)

let pp_print_nil_option pp ppf = function
  | None -> Format.pp_print_char ppf '-'
  | Some v -> Format.fprintf ppf "%a" pp v

let pp_print_header ppf { facility ; severity ; version ; ts ;
                    hostname ; app_name ; procid ; msgid } =
  Format.fprintf ppf "<%d>%d %a %a %a %a %a"
    (int_of_facility facility * 8 + int_of_severity severity)
    version
    (Ptime.pp_rfc3339 ~frac_s:6 ~tz_offset_s:0 ()) ts
    (pp_print_nil_option Format.pp_print_string) hostname
    (pp_print_nil_option Format.pp_print_string) app_name
    (pp_print_nil_option Format.pp_print_int) procid
    (pp_print_nil_option Format.pp_print_int) msgid

let pp_print_kv ppf (Logs.Tag.V (d, v)) =
  Format.fprintf ppf "%s=\"%a\""
    (Logs.Tag.name d) (Logs.Tag.printer d) v

let pp_print_tags ?pp_space pp ppf set =
  Logs.Tag.fold begin fun t a ->
    begin match a, pp_space with
    | true, Some pp -> Format.fprintf ppf "%a" pp ()
    | _ -> ()
    end ;
    Format.fprintf ppf "%a" pp t ;
    true
  end set false |> fun _ -> ()

let pp_print_group ppf (name, set) =
  let pp_space ppf () = Format.pp_print_char ppf ' ' in
  Format.fprintf ppf "[%s %a]"
    name (pp_print_tags ~pp_space pp_print_kv) set

let pp ppf { header ; tags ; msg } =
  match msg with
  | None ->
    Format.fprintf ppf "%a %a" pp_print_header header
      (Format.pp_print_list pp_print_group) tags
  | Some msg ->
    Format.fprintf ppf "%a %a BOM%s"
      pp_print_header header
      (Format.pp_print_list pp_print_group) tags msg

let to_string t =
  Format.asprintf "%a" pp t

let show = to_string

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
