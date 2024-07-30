(* Prelude.Time: Format Date and Time
 * time.ml
 * Keith WACLENA <http://www.lib.uchicago.edu/keith/>
 *
 * Copyright 2022 Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)
(* docstrings go in kwtime.mli *)

open Printf

exception Parse_error of string
exception NYI of string                 (* feature Not Yet Implemented *)

type chunk = Esc | Str

let weekdayname = function
  | 0 -> "Sunday"   | 1 -> "Monday" | 2 -> "Tuesday"  | 3 -> "Wednesday"
  | 4 -> "Thursday" | 5 -> "Friday" | 6 -> "Saturday"
  | _ -> raise (Invalid_argument "weekdayname")

let monthname = function
  | 0 -> "January"   | 1 -> "February" |  2 -> "March"    |  3 -> "April"
  | 4 -> "May"       | 5 -> "June"     |  6 -> "July"     |  7 -> "August"
  | 8 -> "September" | 9 -> "October"  | 10 -> "November" | 11 -> "December"
  | _ -> raise (Invalid_argument "month")

let gmtime    () = Unix.time () |> Unix.gmtime

let localtime () = Unix.time () |> Unix.localtime

(* Calculate local zone offset in minutes
 * stolen from Ocamlnet Netstring.Netdate
 *)
let localzone () =
  let t = Unix.time () in
  let gt = Unix.gmtime t
  and lt = Unix.localtime t in

  let min_diff = (lt.Unix.tm_hour * 60 + lt.Unix.tm_min) -
                 (gt.Unix.tm_hour * 60 + gt.Unix.tm_min) in
  let day_diff = lt.Unix.tm_yday - gt.Unix.tm_yday in

  if day_diff < -1 || day_diff = 1 then      (* local day is UTC day + 1 *)
    min_diff + 24*60
  else if day_diff > 1 || day_diff = -1 then (* local day is UTC day - 1 *)
    min_diff - 24*60
  else                                       (* local day is UTC day *)
    min_diff

let tz () = localzone () / 60, localzone () mod 60

let mytz = tz

let rec convert ?tz tm = function
  | 'A' -> (* locale's full weekday name. *)
      weekdayname tm.Unix.tm_wday
  | 'a' -> (* locale's abbreviated weekday name. *)
      let wdn = weekdayname tm.Unix.tm_wday in
        assert (String.length wdn >= 3);
        String.sub wdn 0 3
  | 'B' -> (* locale's full month name. *)
      monthname tm.Unix.tm_mon
  | 'b' | 'h' -> (* locale's abbreviated month name. *)
      let mn = monthname tm.Unix.tm_mon in
        assert (String.length mn >= 3);
        String.sub mn 0 3
  | 'C' -> (* century (a year divided by 100 and truncated to an integer) as a decimal number [00,99]. *)
      string_of_int tm.Unix.tm_year
  | 'c' -> (* locale's appropriate date and time representation. *)
      sprintf "%s %s %2d %02d:%02d:%02d %s"
        (convert tm 'a')                (* abbr weekday name *)
        (convert tm 'b')                (* abbr month name *)
        tm.Unix.tm_mday                 (* day of month *)
        tm.Unix.tm_hour                 (* hour *)
        tm.Unix.tm_min                  (* minute *)
        tm.Unix.tm_sec                  (* second *)
        (convert tm 'Y')                (* full year *)
  | 'D' -> (* date in the format ``%m/%d/%y''. *)
      sprintf "%s/%s/%s" (convert tm 'm') (convert tm 'd') (convert tm 'y')
  | 'd' -> (* day of the month as a decimal number [01,31]. *)
      sprintf "%02d" tm.Unix.tm_mday
  | 'e' -> (* day of month as a decimal number [1,31]; single digits are preceded by a blank. *)
      sprintf "%2d" tm.Unix.tm_mday
  | 'F' -> (* date in the format ``%Y-%m-%d'' *)
      sprintf "%s-%s-%s" (convert tm 'Y') (convert tm 'm') (convert tm 'd')
  | 'G' -> (* ISO 8601 year with century as a decimal number. *)
      string_of_int (tm.Unix.tm_year + 1900)
  | 'g' -> (* ISO 8601 year without century as a decimal number *)
      sprintf "%02d" ((tm.Unix.tm_year + 1900) mod 1000)
  | 'H' -> (* hour (24-hour clock) as a decimal numbe [00,23]. *)
      sprintf "%02d" tm.Unix.tm_hour
  | 'I' -> (* hour (12-hour clock) as a decimal number [01,12]. *)
      sprintf "%02d" (tm.Unix.tm_hour mod 12)
  | 'j' -> (* day of the year as a decimal number [001,366]. *)
      string_of_int (tm.Unix.tm_yday + 1)
  | 'k' -> (* hour (24-hour clock) as a decimal number [0,23]; single digits are preceded by a blank. *)
      sprintf "%2d" tm.Unix.tm_hour
  | 'l' -> (* hour (12-hour clock) as a decimal number [1,12]; single digits are preceded by a blank. *)
      sprintf "%2d" (tm.Unix.tm_hour mod 12)
  | 'M' -> (* minute as a decimal number [00,59]. *)
      sprintf "%02d" tm.Unix.tm_min
  | 'm' -> (* month as a decimal number [01,12]. *)
      sprintf "%02d" (tm.Unix.tm_mon + 1)
  | 'n' -> (* is replaced by a newline. *)
      "\n"
  | 'p' -> (* locale's equivalent of either ``AM'' or ``PM''. *)
      if tm.Unix.tm_hour < 12 then "AM" else "PM"
  | 'R' -> (* time in the format ``%H:%M''. *)
      sprintf "%s:%s" (convert tm 'H') (convert tm 'M')
  | 'r' -> (* locale's representation of 12-hour clock time using AM/PM notation. *)
      sprintf "%02d:%02d:%02d %s"
        (tm.Unix.tm_hour mod 12)        (* hour *)
        tm.Unix.tm_min                  (* minute *)
        tm.Unix.tm_sec                  (* second *)
        (convert tm 'p')
  | 'S' -> (* second as a decimal number [00,61]. *)
      sprintf "%02d" tm.Unix.tm_sec     (* ? 61 ? *)
  | 's' -> (* number of seconds since the Epoch, UTC (see mktime(3)). *)
      sprintf "%.0f" (Unix.time ())
  | 'T' -> (* time in the format ``%H:%M:%S''. *)
      sprintf "%02d:%02d:%02d"
        tm.Unix.tm_hour                 (* hour *)
        tm.Unix.tm_min                  (* minute *)
        tm.Unix.tm_sec                  (* second *)
  | 't' -> (* is replaced by a tab. *)
      "\t"
  | 'U' -> (* week number of the year (Sunday as the first day of the week) as a decimal number [00,53]. *)
      raise (NYI "convert: %U")
  | 'u' -> (* weekday (Monday as the first day of the week) as a decimal number [1,7]. *)
      let wday = tm.Unix.tm_wday in if wday = 0 then "7" else string_of_int wday
  | 'V' -> (* week number of the year (Monday as the first day of the week) as a decimal number [01,53]. *)
      raise (NYI "convert: %V")
  | 'v' -> (* date in the format ``%e-%b-%Y''. *)
      sprintf "%s-%s-%s" (convert tm 'e') (convert tm 'b') (convert tm 'Y')
  | 'W' -> (* week number of the year (Monday as the first day of the week) as a decimal number [00,53]. *)
      raise (NYI "convert: %W")
  | 'w' -> (* weekday (Sunday as the first day of the week) as a decimal number [0,6]. *)
      string_of_int tm.Unix.tm_wday
  | 'X' -> (* locale's appropriate time representation. *)
      convert tm 'T'
  | 'x' -> (* locale's appropriate date representation. *)
      convert tm 'D'
  | 'Y' -> (* year with century as a decimal number. *)
      string_of_int (tm.Unix.tm_year + 1900)
  | 'y' -> (* year without century as a decimal number [00,99]. *)
      sprintf "%02d" ((tm.Unix.tm_year + 1900) mod 100)
  | 'Z' -> (* time zone name. *)
      begin
        let h,m = match tz with Some (h,m) -> h,m | None -> mytz () in
        sprintf "%s%02d:%02d" (if h < 0 then "-" else "+") (abs h) m
      end
  | 'z' -> (* offset from ITC in the ISO 8601 format ``[+-]hhmm''. *)
      begin
        let h,m = match tz with Some (h,m) -> h,m | None -> mytz () in
        sprintf "%s%02d%02d" (if h < 0 then "-" else "+") (abs h) m
      end
  | '%' -> (* is replaced by `%'. *)
      "%"
  |  c  -> raise (Parse_error (sprintf "unknown %%-escape: %%%c" c))

let strftime ?tz fmt tm =
  let rec parse acc cs =
    match cs with
    | '%'::c::rest -> parse ((Esc, c)::acc) rest
    | '%'::[]      -> raise (Parse_error "incomplete %-escape at string end")
    |  c ::rest    -> parse ((Str, c)::acc) rest
    |    []        -> List.rev acc
  in
  let len = (String.length fmt) in
  let result = Buffer.create (len + 20) in
  let rec loop = function
      | (Esc, c)::rest -> Buffer.add_string result (convert ?tz tm c); loop rest
      | (Str, c)::rest -> Buffer.add_char   result c;                  loop rest
      |         []     -> Buffer.contents   result
  in
  loop (parse [] (String.to_seq fmt |> List.of_seq))
(*$= strftime
  (Prelude.Unix.year () |> Int.to_string) (localtime () |> strftime "%Y")
 *)

let w3c = "%Y-%m-%dT%H:%M:%S"
