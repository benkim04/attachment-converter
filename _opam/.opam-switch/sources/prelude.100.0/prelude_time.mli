(* Prelude.Time: Format Date and Time
 * time.mli
 * Keith WACLENA <http://www.lib.uchicago.edu/keith/>
 *
 * Copyright 2022 Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)

(**{1 Time and Date Functions} 

    See: {{:http://www.w3.org/TR/NOTE-datetime}Misha Wolf and Charles Wicksteed, Date and Time Formats, September 15, 1997}

*)

(** raised when there is an error in [strftime] or [convert]'s format string *)
exception Parse_error of string

(** raised when a [strftime] %-escape is Not Yet Implemented *)
exception NYI of string

(** convert an integer 0..6 to the name of a weekday (0 = Sunday)
 *)
val weekdayname : int -> string

(** convert an integer 0..11 to the name of a month (0 = January)
 *)
val monthname : int -> string

(** [gmtime ()]: return the Greenwich Mean Time (GMT / UTC +0) for now. *)
val gmtime : unit -> Unix.tm

(** [localtime ()]: return the localtime for now. *)
val localtime : unit -> Unix.tm

(** [tz ()]: return local timezone as [(hours,minutes)] offset from zulu. *)
val tz : unit -> int * int

(** format date and time (pure-ocaml implementation of Posix strftime(3))

  Bugs:
- Locale-specific escapes are hard-wired to EN_US
- [%U %V %W] escapes are Not Yet Implemented and raise [NYI]

    @param tz time zone as [(hours,minutes)] offset from zulu
    @param fmt string with %-escapes to format
    @param tm Unix.tm record representing the time
    @return formatted string
    @raise Parse_error for an invalid format string
    @raise NYI for %-escapes that are Not Yet Implemented: ['U'], ['V'], ['W']
*)
val strftime : ?tz:int * int -> string -> Unix.tm -> string

(** w3c time format %Y-%m-%dT%H:%M:%S *)
val w3c : string
