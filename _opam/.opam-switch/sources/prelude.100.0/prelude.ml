(*   _  __
 *  | |/ /  prelude
 *  | / /   prelude.ml
 *  | \ \   Keith WACLENA <http://www.lib.uchicago.edu/keith/>
 *  |_|\ \
 *      > > Copyright 2017 Keith Waclena. All rights reserved.
 *     / /  Distributed under the GPL2 license, see terms at the end of the file.
 *    / /
 *    \ \
 *     \_\
 *
 * PREREQUISITES:
 *   Prelude:      ocaml >= v4.11.0 ?
 *   build system: ocaml-makefile
 *)
(** {1 OCaml Standard Library additions and renamings.}

    See the {!example}.

    - {i Warning}: the test suite for this library is only 3/4's done!
    - {i Warning}: this is my personal library and the API may change
    at any time (though actually I'm very reluctant to do so).

    {2 Philosophy and Usage}
    - this module is written so that it can be opened at the top of your source
      files: [open Prelude] (but it's not required)
    - lists are the heart of FP, so within [Prelude] we [include List];
      after opening [Prelude], we have direct (unprefixed) access to all the
      functions in {!List}, e.g. [map], [rev].
    - we also [include Printf]
    - many commonly used OCaml functions have names that are longer
      than I like (e.g. [List.fold_left]), so I provide aliases (e.g. [foldl])
    - [Prelude] has no dependencies on 3rd-party modules that don't come with the OCaml compiler
      (this requirement is temporarily untrue now that the latest
      versions of the compiler have dropped [Stream] syntax; I'm
      working on making this claim true again)

    Note: I much prefer terse function names, especially since OCaml's
    nestable module system provides ready access to longer names via
    the module prefixes, and OCaml's local-open syntax allows these
    short names to be easily used without global namespace
    contamination.

    {2 Incompatibilities With [Stdlib] When You [open Prelude]}

    Occasionally new functions are added to [Stdlib] or [Stdlib.List]
    whose names are already used in [Prelude]; this is something to be
    aware of if you [open Prelude] as I do.  An example is that
    recently [Stdlib.List] acquired a [compare] function.  Since
    [Prelude] opens [List], this new [compare] would shadow
    [Stdlib.compare], which is IMHO annoying.

    My fix is to arrange that [Prelude.compare] is [Stdlib.compare].
    The downside is that if you want [Stdlib.List.compare] you have to
    use the fully-qualified name.  Since [Stdlib.List.compare] has a
    different type than [Stdlib.compare], you'll always get a
    compile-time error if you ever get confused.

    If there's anything about this you don't like, you can simply use
    [Prelude] without opening it.

    {2 Conventions for Data Types and Modules}
    Aspirationally, all modules that define data types should include the following
    functions:
    - [random]: to generate a random value of that type
    - [to_string]: to generate a string representation of any value of that type
    - [print]: typically just [(to_string >> print)]

    I find these functions to be extremely useful for development and testing
    (especially in the top-level), and for unit tests.

    {2 Classified Index:}
    - Module Types: {!ARITH}, {!NULL}, {!OrderedType}, {!AIString}
    - Basics: {!arithmetic}, {!combinators}, {!section-comparison}, {!functionals}
    - Enhanced OCaml Data Types: {!arrays}, {!bool}, {!buffers}, {!chars}, {!exceptions}, {!files}, {!hashtables}, {!int}, {!lists}, {!maps}, {!options}, {!pairs}, {!section-printf}, {!section-random}, {!refs}, {!seq}, {!sets}, {!strings}, {!system}
    - New Data Types: {!a1}, {!generators}, {!ia}, {!interval}, {!leftist} heap, {!list1}, {!multisets}, {!n0}, {!n1}, functional {!queues}, {!results}, {!rosetrees}, functional {!stacks}, {!triples}, {!units}, {!vectors}
    - Development (Top-level) Utils: {!show}, {!section-print}
    - Unix: {!unix}, {!Unix.Timer}, {!Unix.Env}. {!Unix.Proc}, {!Unix.Shell}
    - {!io}, {!interactive}, {!messages}, {!logging}
    - Specialized: {!isn}, {!macros}, {!prereq}, {!refer}
    - {!extensions} (3rd-party extensions): {!Extended.benchmark}, {!Extended.re}, {!Extended.uri}, {!Extended.xmlm}.

    @author Keith WACLENA
*)

(** {1:combinators Combinators and Operators} *)

(** [id x]: identity function ([I] combinator), returns [x]. *)
let id x = x
(*$Q id
  Q.int (fun i -> id i = i)
*)

(** [(k x y)] is [x]; this is the constant function generator ([K] combinator). *)
let k x y = x
(*$Q k
  Q.int (fun i -> k i 1 = i)
  Q.int (fun i -> k i i = i)
*)

(** [(ki x y)] is [y]; [ki] = [(flip k)] and is [KI] in combinatory logic. *)
let ki a b = b
(*$Q ki
  Q.(pair int int) (fun (x,y) -> ki x y = y && ki x y = (k id) x y)
*)

(** [flip f x y]: argument-flipping combinator ([C] combinator);
    returns [(f y x)].  [(flip (^) "a" "b") = "ba"] *)
let flip f x y = f y x
(*$Q flip
  Q.int (fun i -> flip (+) i i =  i+i)
  Q.(pair string string) (fun (a,b) -> flip (^) a b = b^a)
*)

(** [(w f x)] is [(f x x)]; this is the argument-doubling combinator [W]. *)
let w f x = f x x               (* warbler W combinator argument doubler *)
(*$Q w
  Q.int (fun x -> w (+) x = x + x)
 *)

(** [dbl] is [w]. *)
let dbl = w

(** [(whenever this that x)] is [(if x = this then that else x)].

    Example:
    {[ (argv |> whenever [] ["-"] |> iter (within process)) ]}

    will invoke [process] on [stdin] if no files are in [argv].
*)
let whenever a b c  = if c = a then b else c
(*$= whenever & ~printer:string_of_int
   ~-1 (whenever 0 ~-1 0)
     1 (whenever 0 ~-1 1)
     2 (whenever 0 ~-1 2)
*)

(** [(curry f)]: converts a function on pairs to a curried function of two arguments. *)
let curry f = fun x y  -> f (x,y)
(*$T uncurry;curry
  curry (uncurry (+)) 1 2 = 1+2
*)

(** [(uncurry f)]: converts a curried function of two arguments to a function on pairs.*)
let uncurry f = fun (x,y) -> f  x y
(*$T uncurry
  uncurry (+) (1, 2) = 1+2
*)

(** [(tap f x)]: applies [(f x)] for side-effect, then returns [x].
    [("foo" |> tap print |> String.length)] prints ["foo"] and returns [3].
 *)
let tap (f : 'a -> unit) x = f x; x
(*$= tap & ~printer:string_of_int
  1 (tap (k ()) 1)
*)

(** [(|-)] is [(flip tap)].

    {[ foo
    |> List.map ...
    |- (len >> printf "%d\n")
    |> List.concat
    |> ...
    ]}
    @author Idea due to Xavier Clerc.
*)
let (|-) x f = f x; x
(*$= (|-) & ~printer:string_of_int
  1 (1 |- (k ()))
*)

(** [(thunk f x)] is [(fun () -> f x)], i.e. it wraps the application [(f x)] in a thunk. *)
let thunk f x = fun () -> f x
(*$= thunk  & ~printer:string_of_int
  1 (thunk id 1 ())
*)

(** [y] is the fixed-point finding Y-combinator; {i not} tail-recursive.

    [y] can be used to write recursive functions without using [let rec]:
    for example, given:

    {[ let fact_ fact x = if x = 0 then 1 else x * fact (x-1) ]}

    then [(y fact_ 6) = 720].

    Perhaps more practically, [y] can be used, for example to trace a
    recursive function without signifcantly modifying the function
    definition:

    {[ let trace name f_ f x = let r = f_ f x in printf "%s %d -> %d\n" name x r; r ]}

    and now:
    {v
    # y (trace "fact" fact_) 6;;
    fact 0 -> 1
    fact 1 -> 1
    fact 2 -> 2
    fact 3 -> 6
    fact 4 -> 24
    fact 5 -> 120
    fact 6 -> 720
    _ : int = 720 v} *)
let rec y f x = f (y f) x
(*$T y
  (let f_ f x = x+1 in y f_ 1 = 2)
  (let fact_ fact x = if x = 0 then 1 else x * fact (x-1) in y fact_ 6 = 720)
*)

(** [(fork (±) f g x)] is [(f x ± g x)] and is the unary version of {!fork2}.

    {i N.B.} this is J's fork conjunction (with monadic [f] and [g], and dyadic [(±)]).

    {v
     ±
    / \
   f   g
   |   |
   x   y
   v}

    Example: [(fork (/) sum len)] is the arithmetic mean:
    - [(fork (/) sum len (1--100)) = 50]

    Example: [(fork (-) maximum minimum)] is the range of the ints in its list argument:
    - [(fork (-) maximum minimum [2;8;5;4]) = 6]
 *)
let fork (+) f g x = f x + g x
(*$Q fork
  Q.(list int) (fun xs -> xs = [] || fork (/) sum len xs = sum xs / len xs)
*)

(** [(fork2 (±) ( × ) (÷) n m)] is [(n×m + n÷m)] for arbitrary functions [(+)], [( × )], and [(÷)].

    {i N.B.} this is J's fork conjunction (with dyadic [(±)], [( × )], and [(÷)]).

    {v
      ±
    /   \
   ×     ÷
  / \   / \
 n   m n   m
 v}

 *)
let fork2 (+) ( * ) (/) x y = (x * y) + (x / y)
(*$Q fork2
  Q.(pair int int) (fun (x,y) -> fork2 (+) ( * ) (-) x y = (x * y) + (x - y))
 *)

(** [(lefthook (±) (¬) x y)] is [(¬x ± y)].

    {v
     ±
    / \
   ¬   y
   |
   x
   v}

    Example: [(lefthook cons succ) = (conswith succ)]

    Example: fastest way to get the size of the data on an input
    channel like [stdin]:

    {[ Gen.(optional readblock $ fold (lefthook (+) String.len) 0) ]}
*)
let lefthook g f x y = g (f x) y
(*$= lefthook & ~printer:string_of_int
  1 (lefthook (+) succ 0 0)
*)
(*$Q lefthook
  Q.(pair int (small_list int)) (fun (x,xs) -> (succ x :: xs)= lefthook cons succ x xs)
 *)

(** [(<?)] is {!lefthook}.

    Example: [foldr (cons <? even) [] (0--4) = [true; false; true; false; true]]
*)
let (<?) = lefthook
(*$Q (<?)
  Q.unit (fun () -> foldr (cons <? even) [] (0--4) = [true; false; true; false; true])
 *)
(*$Q (<?);lefthook
  Q.(list int) (fun xs -> foldr (cons <? even) [] xs = foldr (lefthook cons even) [] xs)
 *)

(** [(righthook (±) (¬) x y)] is [(x ± ¬y)].

    {v
     ±
    / \
   x   ¬
       |
       y
    v}

    Example: [(righthook snoc succ) = (snocwith succ)] *)
let righthook g f x y = g x (f y) (* righthook snoc = snocwith *)
(*$Q righthook
  Q.(pair int (small_list int)) (fun (x,xs) -> (succ x :: xs)= righthook snoc succ xs x)
 *)

(** [(>?)] is {!righthook}.

    Example: [foldl (snoc >? even) [] (0--4) = [true; false; true; false; true]]
*)
let (>?) = righthook
(*$Q (>?)
  Q.unit (fun () -> foldl (snoc >? even) [] (0--4) = [true; false; true; false; true])
 *)
(*$Q (>?);righthook
  Q.(list int) (fun xs -> foldl (snoc >? even) [] xs = foldl (righthook snoc even) [] xs)
 *)

(** [on (±) (¬)] is [(fun x y -> ¬x ± ¬y)].  Typically used to
    generate comparison functions for [List.sort].

    {v
     ±
    / \
   ¬   ¬
   |   |
   x   y
    v}

    For example, to sort an alist in reverse order of values, use:

    {[ (sort (on (flip compare) snd)) ]}
 *)
let on g f x y = g (f x) (f y)
(*$= on & ~printer:string_of_int
   2 (on (+) succ 0 0)
*)
(*$T
   0 = (on compare fst (0,0) (0,1))
  -1 = (on compare fst (0,0) (1,0))
*)

(** [(on1 (±) (¬) x)] is [(fork (±) (¬) (¬))] i.e. [(¬x ± ¬x)].

    {v
     ±
    / \
   ¬   ¬
   |   |
   x   x
    v}

   Example: [(on1 ( + ) (dbl ( * ))) x = (x*x) + (x*x)]
*)
let on1 g f x = g (f x) (f x)
(*$Q on1
  Q.int (fun x -> on1 (+) (dbl ( * )) x = (x*x) + (x*x))
 *)

(** {2:comparison Comparison}

    A comparison function is one that compares two values and returns
    one of [(-1, 0, 1)] (e.g. {!OrderedType.compare}).

    See also {!Lists.comparison} and {!AIString.compare}.
 *)

(** [(revcmp cmp)] converts the comparison function [cmp] to one that compares in reverse order.
*)
let revcmp cmp a b : int = cmp b a

(** [(cmpeq cmp)] converts the comparison function [cmp] into an equality predicate. *)
let cmpeq cmp = fun a b -> cmp a b = 0
(*$= cmpeq & ~printer:string_of_bool
  true (cmpeq  compare 0 0)
  false (cmpeq compare 1 0)
  false (cmpeq compare 0 1)
*)

(** {2:relational Relational Operators for Partial Application}

    These functions are flipped versions of the standard relational
    operators [(<)], [(<=)], [(>)], and [(>=)] (I throw in [(=)] and
    [(<>)] just for symmetry) and are intended to be used {i
    partially-applied}.  I think this function:

    [(len >> gt 64)]

    looks much better and is easier to understand than:

    [(len >> flip (>) 64)]

    or:

    [(len >> fun n -> n > 64)]

    or even, sometimes:

    [(fun xs -> len xs > 64)]

    YMMV!
 *)

let lt n u = u < n
(** [(lt n)] is [true] iff it is applied to a value strictly less than [n].

    [lt] is [(flip (<))].
*)

let lte n u = u <= n
(** [(lte n)] is [true] iff it is applied to a value less than or equal to [n].

    [lte] is [(flip (<=))].
*)

let gt n u = u > n
(** [(gt n)] is [true] iff it is applied to a value strictly greater than [n].

    [gt] is [(flip (>))].
*)

let gte n u = u >= n
(** [(gte n)] is [true] iff it is applied to a value greater than or equal to [n].

    [gte] is [(flip (>=))].

*)

let eq = (=)
(** [(eq n)] is [true] iff it is applied to a value equal to [n].

    [eq] is just [(=)].
*)

let neq = (<>)
(** [(neq n)] is [true] iff it is applied to a value not equal to [n].

    [neq] is just [(<>)].
*)

(** {2 Function Composition and Application} *)

(** [(apply f x)]: function application: [(apply f x)] is exactly equivalent
    to [(f x)]. *)
external apply : ('a -> 'b) -> 'a -> 'b = "%apply"
(*$= apply & ~printer:string_of_int
  3 (apply succ 1 + 1)
*)

(** [(@@)] is [apply]; right associative.

    This is Haskell's [($)], and is useful for eliminating parentheses.

    Example:
    {[ (print_endline (string_of_int (1 + 1))) = (print_endline @@ string_of_int @@ 1 + 1) ]}
 *)
let ( @@ ) = apply
(*$Q (@@)
  Q.int (fun n -> id @@ n = apply id n)
  Q.int (fun n -> succ @@ n = apply succ n)
  Q.int (fun n -> (k 0) @@ n = apply (k 0) n)
*)

(** [(f << g)]: function composition: [((f << g) x)] is exactly equivalent to [(f (g x))].

    This is Haskell's [(.)].
*)
let (<<) f g x = f (g x)
(*$= (<<) & ~printer:string_of_int
  (pred (succ 0)) ((pred << succ) 0)
*)

(** @deprecated Use {!(<<)}. *)
let ($.) f g x = f (g x)

(** [(f >> g)]: reverse function composition: [((f >> g) x)] is exactly equivalent to [(g (f x))]. *)
let (>>) f g x = g (f x)
(*$= (>>) & ~printer:string_of_int
  (pred (succ 0)) ((succ >> pred) 0)
*)

(** @deprecated Use {!(>>)}. *)
let ($) f g x = g (f x)

(**/**)
(* define before use conflicts with the order I prefer for the doc *)
module Pre = struct (*$< Pre *)
  let default d f x = try f x with _ -> d
  (*$= default & ~printer:string_of_int
    1 (default 0 id 1)
    0 (default 0  (fun _ -> failwith "") 1)
  *)
  let foldil f v (a,z) =
    let rec foldil' v a =
      if a >= succ z
      then v
      else foldil' (f v a) (succ a)
    in
    foldil' v a
  (*$Q foldil
    Q.(pair small_nat small_nat) (fun (lo,hi) -> foldil (+) 0 (lo,hi) = if lo <= hi then lo--hi |> sum else 0)
   *)
  let rec whilst p f x = if p x then whilst p f (f x) else x
  (*$Q whilst
    Q.unit (fun () -> whilst (k false) id 0 = 0)
    Q.small_int (fun m -> whilst (fun n -> n < m) succ 0 = m)
   *)

  module Random = struct (*$< Random *)
    let maxbound = pred (int_of_float (2.**30.))
  end (*$>*)

  module Strings = struct (*$< Strings *)
    let foldl f a str =
      let rec fold i acc = if i = String.length str then acc else fold (i+1) (f acc str.[i]) in
      fold 0 a
    (*$Q foldl
      Q.string (fun s -> String.length s = len (foldl snoc [] s))
      Q.string (fun s -> s = "" || s.[0] = hd (foldl snoc [] s |> rev))
    *)
    (*$= foldl
      [] (foldl snoc [] "")
    *)
    let explode str = foldl (fun xs x -> x::xs) [] str |> List.rev
    (*$Q explode
      Q.string (fun s -> s = "" || explode s |> hd = s.[0])
    *)
    (*$= explode
      [] (explode "")
      ['a'] (explode "a")
      ['a';'b'] (explode "ab")
      ['a';'b';'c'] (explode "abc")
    *)
  end (*$>*)

  module Hashtbl = struct (*$< Hashtbl *)
    (*$inject
      let ht = Hashtbl.create 10
      let () = Hashtbl.(adjoin cons [] ht "a" 0; adjoin cons [] ht "b" 2; adjoin cons [] ht "a" 1)
      let stub f = f cons [] (Hashtbl.create 0) "" 0
    *)
    include Hashtbl
    let adjoin vadd empty ht k v = default empty (flip find k) ht |> vadd v |> replace ht k
    (* There's got to be a better way to organize this test... *)
    (*$T adjoin
      stub adjoin; Hashtbl.find ht "a" |> len = 2
      stub adjoin; Hashtbl.find ht "b" |> len = 1
      stub adjoin; Hashtbl.find ht "a" = [1;0]
      stub adjoin; Hashtbl.find ht "b" = [2]
    *)
  end (*$>*)

  module Lists = struct         (*$< Lists *)
    let upto m n =
      let rec loop m acc = if m > n then acc else loop (m+1) (m::acc) in
      loop m [] |> List.rev
      (*$= upto
        [] (upto 1 0)
      *)
      (* monotonically increasing *)
      (*$Q upto
        Q.(pair small_int small_int) (fun (a,b) -> a > b || let xs = upto a b in len xs = b - a + 1 && a = hd xs)
        Q.small_int (fun b' -> let b = abs b' + 1 in upto 1 b |> sum = b * (b+1) / 2)
        Q.small_int (fun b -> let n = abs b + 1 in let xs = upto 1 n in foldr2 (fun a b r -> r && a < b ) xs (tl xs @ [n +1]) true)
      *)
  end (*$>*)

  module Unix = struct         (*$< Unix *)
    open Unix open Printf
    let to_string = function
      | Unix_error (err,f,"") -> sprintf "error: %s (%s)" (error_message err) f
      (* these are (I think) all the file-related exceptions ie we can expect a filename *)
      | Unix_error ((EACCES|EEXIST|EFBIG|EISDIR|ENAMETOOLONG
                     |ENOENT|ENOEXEC|ENOSPC|ENOTDIR|ENOTEMPTY
                     |EPERM|EROFS|ELOOP) as err,_,fn)  ->
         sprintf "%s: %s" fn (error_message err)
      | Unix_error (err,f,x)  -> sprintf "error: %s %s: %s" (error_message err) f x
      | exn                   -> raise_notrace exn
    (* TODO TESTS *)
  end (*$>*)
end (*$>*)
(**/**)

(** {1 Exception} *)

exception Failed_prerequisite of string list
(** [(Failed_prerequisite cmds)] is the type of exceptions raised by functions in {!Prereq}. *)

(** {1 Module Types} *)

(** {2 Null} *)

(** The empty module. *)
module type  NULL = sig end
module Null : NULL = struct end

(** {2 OrderedType} *)

(** [(sig type t val compare : t -> t -> int end)]. *)
module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

(** {2 TOSTRING} *)

(** [(sig type t val to_string : t -> string end)]. *)
module type TOSTRING = sig
  type t
  val to_string : t -> string
  val compare : t -> t -> int
end

(** {2 ARITH} *)

(** Signature for Arithmetics ([type t], [zero], [succ], [( + )] and [( * )]); we
    also include [compare], [one], [pred], [( - )], [( / )], and [random].  *)
module type ARITH = sig
  type t
  val compare : t -> t -> int
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val random : unit -> t
end

(** {1:bool Bool} *)

(** [Prelude.Bool] is [Stdlib.Bool] with additional Boolean functions; satisifes [OrderedType]. *)
module Bool = struct (*$< Bool *)

  (** @closed *)
  include Bool

  (** [print] is [(string_of_bool >> print_endline)]. *)
  let print = to_string >> print_endline

  (** [random] is [Random.bool]. *)
  let random = Random.bool
  (* TODO TESTS *)
end (*$>*)

(** {1:n0 N0: The Natural Numbers from zero. } *)

(** Type-specialized comparison; satisifes [OrderedType].

    The Natural Numbers from zero. *)
module N0 : sig
  type t = private int
  val to_int : t -> int
  val of_int : int -> t option
  val to_string : t -> string
  val compare : t -> t -> int
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t option
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
  val print : t -> unit
  val random : unit -> t
end = struct (*$< N0 *)
  type t = int
  let [@inline] to_int t = t
  let [@inline] of_int n = if n < 0 then None else Some n
  let [@inline] compare a b = compare a b
  let zero = 0
  let one = 1
  let [@inline] succ t = succ t
  let [@inline] pred n = if n < 1 then None else Some (pred n)
  let [@inline] ( + ) a b = a + b
  let [@inline] ( * ) a b = a * b
  let to_string = string_of_int
  (* needs Multiset.Make' hack... *)
  (* TODO TEST $Q to_string
    Q.bool (fork (=) to_string string_of_int)
   *)
  let print = string_of_int >> print_endline
  let random () = Random.int Pre.Random.maxbound
  (* TODO TESTS *)
end (*$>*)

(** {1:n1 N1: The Natural Numbers from one.} *)

(** Type-specialized comparison; satisifes [OrderedType].

    The Natural Numbers from one  *)
module N1 : sig
  type t = private int
  val to_int : t -> int
  val of_int : int -> t option
  val compare : t -> t -> int
  val one : t
  val succ : t -> t
  val pred : t -> t option
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
  val print : t -> unit
  val random : unit -> t
end = struct (*$< N1 *)
  type t = int
  let [@inline] to_int t = t
  let [@inline] of_int n = if n < 1 then None else Some n
  let [@inline] compare a b = compare a b
  let one = 1
  let [@inline] succ t = succ t
  let [@inline] pred n = if n < 2 then None else Some (pred n)
  let [@inline] ( + ) a b = a + b
  let [@inline] ( * ) a b = a * b
  let [@inline] to_string n = string_of_int n
  let print = string_of_int >> print_endline
  let random () = Random.int Pre.Random.maxbound |> succ
end (*$>*)

(** {1:int Int} *)

(** Type-specialized operators and comparisons; satisifes [ARITH]. *)
module Int = struct (*$< Int *)
  (** @closed *)
  include Int
  (** {2 Specialized and inlined versions of arithmetic operators.} *)

  let [@inline] succ (x:t) : t = succ x
  (*$Q succ
    Q.int (fun n -> Stdlib.succ n = succ n)
  *)
  let [@inline] pred (x:t) : t = pred x
  (*$Q pred
    Q.int (fun n -> Stdlib.pred n = pred n)
  *)
  let [@inline] ( + ) (x:t) (y:t) : t = add x y
  (*$Q (+)
    Q.(pair int int) (fun (a,b) -> Stdlib.(+) a b = a+b)
  *)
  let [@inline] ( - ) (x:t) (y:t) : t = sub x y
  (*$Q (-)
    Q.(pair int int) (fun (a,b) -> Stdlib.(-) a b = a-b)
  *)
  let [@inline] ( * ) (x:t) (y:t) : t = mul x y
  (*$Q ( * )
    Q.(pair int int) (fun (a,b) -> Stdlib.( * ) a b = a*b)
  *)
  let [@inline] ( / ) (x:t) (y:t) : t = div x y
  (*$Q (/)
    Q.(pair int int) (fun (a,b) -> Stdlib.(/) a b = a/b)
  *)

  (** [(even i)] is [true] iff [(i mod 2 = 0)]. *)
  let even i = i mod 2 = 0
  (*$T even
    even ~-2
    not (even ~-1)
    even 0
    not (even 1)
    even 2
  *)

  (** [(odd i)] is [true] iff [(i mod 2 <> 0)]. *)
  let odd  i = i mod 2 <> 0
  (*$T odd
    not (odd ~-2)
    odd ~-1
    not (odd 0)
    odd 1
    not (odd 2)
  *)

  (** [(divisible n)] is that function of [u] that returns [true] iff
      [u] is evenly divisible by [n], with no remainder. *)
  let divisible n = fun u -> u mod n = 0

  (** Specialized and inlined version of [Stdlib.max]. *)
  let [@inline] max (v0:t) (v1:t) = if v0 > v1 then v0 else v1
  (*$Q max
    Q.(pair int int) (fun (a,b) -> Stdlib.max a b = max a b)
  *)

  (** Specialized and inlined version of [Stdlib.min]. *)
  let [@inline] min (v0:t) (v1:t) = if v0 < v1 then v0 else v1
  (*$Q min
    Q.(pair int int) (fun (a,b) -> Stdlib.min a b = min a b)
  *)

  (** [(digits ?base n)] is the number of digits required to express
      [n] in [base] (default base: 10).  The number of digits in a
      negative number does not include the sign.
   *)
  let digits ?(base=10) n =
    let d10 n = n+1 |> float_of_int |> log10 |> ceil |> int_of_float in
    if n = 0
    then 1
    else if base = 10
    then d10 (abs n)
    else Pre.whilst (fun (n,_) -> n <> 0) (fun (n,ds) -> n / base, succ ds) (abs n, 0) |> snd
  (*$Q digits
    Q.int                       (fun n -> digits n = digits ~-n)
    Q.(int_range      0      9) (fun n -> digits          n = 1)
    Q.(int_range     10     99) (fun n -> digits          n = 2)
    Q.(int_range    100    999) (fun n -> digits          n = 3)
    Q.(int_range   1000   9999) (fun n -> digits          n = 4)

    Q.(int_range    0o0    0o7) (fun n -> digits ~base:8  n = 1)
    Q.(int_range   0o10   0o77) (fun n -> digits ~base:8  n = 2)
    Q.(int_range  0o100  0o777) (fun n -> digits ~base:8  n = 3)
    Q.(int_range 0o1000 0o7777) (fun n -> digits ~base:8  n = 4)

    Q.(int_range    0X0    0XF) (fun n -> digits ~base:16 n = 1)
    Q.(int_range   0X10   0XFF) (fun n -> digits ~base:16 n = 2)
    Q.(int_range  0X100  0XFFF) (fun n -> digits ~base:16 n = 3)
    Q.(int_range 0X1000 0XFFFF) (fun n -> digits ~base:16 n = 4)
   *)
  (*$= digits
    1 (digits 0)
   *)

  (** [(missing ints)] return the integers that are missing in [ints]; tail-recursive.

      I.e., return the list of integers between [(minimum ints)] and
      [(maximum ints)] that are not in [ints].
  *)
  let missing = function
  | [] -> []
  | is ->
      let module S = Set.Make (Int) in
      let s = S.of_list is in
      let lo,hi = S.min_elt s, S.max_elt s in (* ASSERT via []-match above *)
      let each acc n = if S.mem n s then acc else n::acc in
      Pre.foldil each [] (lo,hi)
  (*$Q missing
    Q.unit (fun () -> missing [] = [])
    Q.(list small_int) (fun xs -> let module S = Set.Make (Int) in let ms = missing xs |> S.of_list in S.for_all (not $. flip mem xs) ms)
    Q.(list small_int) (fun xs -> xs = [] || let module S = Set.Make (Int) in let ms = missing xs |> S.of_list in foldil (fun r n -> r && (S.mem n ms || mem n xs)) true (minimum xs, maximum xs))
  *)

  (** [print] is [(string_of_int >> print_endline)]. *)
  let print = string_of_int >> print_endline

  (** [(random ())] is a random [int]. *)
  let random () = Random.(int Pre.Random.maxbound)
  (* TODO TESTS *)

  (* let interval lo hi () =
   *   Random.int (hi-lo+1) + lo *)

end (*$>*)

(** {1:arithmetic Arithmetic} *)

(** [(~++) x] is [(succ x)]; like C's [++]; minimizes parens in function parameters. *)
let (~++) = succ

(** [(~--) x] is [(pred x)]; like C's [--]; minimizes parens in function parameters. *)
let (~--) = pred

(** [(even n)] is [true] iff [n] is an even integer. *)
let even n = n mod 2 = 0
(*$Q even;odd
  Q.int (fun n -> odd (abs n+3) = even (abs n))
*)

(** [(odd n)] is [true] iff [n] is an odd integer. *)
let odd n = n mod 2 <> 0
(* test: see even above *)

(** {1:float Float} *)

(** Type-specialized operators and comparisons; satisifes [ARITH]. *)
module Float = struct (*$< Float *)
  (** @closed *)
  include Float

  (** Type [t = float]. *)
  type t = float

  (** Specialized and inlined version of [compare]. *)
  let [@inline] compare (a:float) (b:float) : int = compare a b
  (*$Q compare
    Q.(pair float float) (fun (a,b) -> compare a b = compare a b)
  *)

  (** {2 Specialized and inlined versions of arithmetic operators.} *)

  let [@inline] ( + ) (x:float) (y:float) : float = add x y
  (*$Q (+)
    Q.(pair float float) (fun (a,b) -> Stdlib.(+.) a b = a+b)
  *)
  let [@inline] ( - ) (x:float) (y:float) : float = sub x y
  (*$Q (-)
    Q.(pair float float) (fun (a,b) -> Stdlib.(-.) a b = a-b)
  *)
  let [@inline] ( * ) (x:float) (y:float) : float = mul x y
  (*$Q ( * )
    Q.(pair float float) (fun (a,b) -> Stdlib.( *. ) a b = a*b)
  *)
  let [@inline] ( / ) (x:float) (y:float) : float = div x y
  (*$Q (/)
    Q.(pair float float) (fun (a,b) -> Stdlib.(/.) a b = a/b)
  *)

  (** Specialized and inlined version of [Stdlib.Float.max]. *)
  let [@inline] max (v0:float) (v1:float) = max v0 v1
  (*$Q max
    Q.(pair float float) (fun (a,b) -> Stdlib.max a b = max a b)
  *)

  (** Specialized and inlined version of [Stdlib.Float.min]. *)
  let [@inline] min (v0:float) (v1:float) = min v0 v1
  (*$Q min
    Q.(pair float float) (fun (a,b) -> Stdlib.min a b = min a b)
  *)

  (** [(bankers x)] converts [x] to the nearest float whose fractional part is zero
      (i.e. the nearest integer, represented as a float so as to preserve NaN)
      using {i Banker's Rounding} aka round-half-to-even as tie-breaking rule.
      See {{: https://en.wikipedia.org/wiki/Rounding#Round_half_to_even } Wikipedia}. *)
  let bankers x =
    let f,i = modf x in
    let negative () = match compare (abs_float f) 0.5 with
      | -1 -> ceil x
      |  0 -> if odd (truncate i) then i -. 1. else i
      |  1 -> floor x
      |  _ -> assert false
    in
    let positive () = match compare f  0.5 with
      | -1 -> floor x
      |  0 -> if odd (truncate i) then i +. 1. else i
      |  1 -> ceil  x
      |  _ -> assert false
    in
    match compare f 0.0 with
    | -1 -> negative ()
    |  0 -> i
    |  1 -> positive ()
    |  _ -> assert false
  (*$= bankers
    (bankers (-4.5    )) (-4.)
    (bankers (-3.5    )) (-4.)
    (bankers (-0.678  )) (-1.)
    (bankers (-0.5    )) (0.)
    (bankers (-0.1234 )) (0.)
    (bankers (0.      )) (0.)
    (bankers (0.1234  )) (0.)
    (bankers (0.5     )) (0.)
    (bankers (0.678   )) (1.)
    (bankers (3.5     )) (4.)
    (bankers (4.5     )) (4.)
   *)
  (*$Q bankers
    Q.float (fun x -> let r = bankers x in let i = modf x |> snd in if x >= 0. then i <= r && r <= i+.1. else i-.1. <= r && r <= i)
   *)

  (** [(roundto m x)] rounds [x] to the nearest multiple of [m].

      For example, round 2.1784 dollars to whole cents:
      - [(roundto 0.01 2.1784) = 2.18]
   *)
  let roundto m x = bankers (x /. m) *. m
  (* TODO TESTS *)
  (*$= roundto
    (roundto 0.01 2.1784) 2.18
   *)

  (** [(random ())] is a random [float]. *)
  let random () = Random.float max_float

  (* TODO TESTS *)
end (*$>*)

(** [round] is [Float.round]. *)
let round = Float.round

(** {1:pairs Pairs} *)

(** Useful functions on 2-tuples. *)
module Pair = struct (*$< Pair*)
  (** [(of_list [a;b])] is the pair [(a,b)]; a list of any length other than 2 raises [invalid_arg _]. *)
  let of_list = function [a;b] -> a,b | xs -> invalid_arg (Printf.sprintf "len %d <> 2" (List.length xs))
  (*$Q of_list
    Q.unit (fun () -> not @@ succeeds of_list [])
    Q.unit (fun () -> succeeds of_list [1;2])
    Q.unit (fun () -> not @@ succeeds of_list [1;2;3])
    Q.(list int) (fun xs -> succeeds of_list xs || len xs <> 2)
   *)

  (** [(to_list (a,b))] is [[a; b]]. *)
  let to_list (a,b) = [a; b]
  (*$Q to_list
    Q.(pair int int) (fun (a,b) -> to_list (a,b) = [a;b])
   *)

  (** [(dup a)] is [(a,a)]. *)
  let dup a = a,a
  (*$= dup
    (0,0) (dup 0)
  *)

  (** [(up a b)] is [(a,b)]. *)
  let up a b = a,b
  (*$= up
    (0,1) (up 0 1)
  *)

  (** [(swap (a,b))] is [(b,a)] *)
  let swap (a,b) = b,a
  (*$= swap
    (1,0) (swap (0,1))
  *)

  (** [(map f (a,b))] is [(f a, f b)]: applies the same function to
      each element of the pair. *)
  let map f (a,b) = f a, f b
  (*$= map
    (0,1) (map id (0,1))
    (1,2) (map succ (0,1))
  *)

  (** [(iter f (a,b))] is [(f a, f b)]: applies the same function to
      each element of the pair, for side-effect. *)
  let iter f (a,b) = f a; f b
  (*$= iter
    () (iter (k ()) (0,1))
    () (iter ignore (0,1))
  *)

  (** [(onleft f (a,b))] is [(f a, b)]. *)
  let onleft f (a,b) = f a, b
  (*$Q onleft
    Q.(pair int int) (fun p -> p = onleft id p)
    Q.(pair int int) (fun (a,b) -> onleft succ (a,b) = (succ a, b))
   *)

  (** [(onright f (a,b))] is [(a, f b)]. *)
  let onright f (a,b) = a, f b
  (*$Q onright
    Q.(pair int int) (fun p -> p = onright id p)
    Q.(pair int int) (fun (a,b) -> onright succ (a,b) = (a, succ  b))
   *)

  (** [(cross f g (a,b))] is [(f a, g b)]: applies two different
      functions, one to each element of the pair.

      {[ (cross succ pred (1,1)) = (2,0) ]}
  *)
  let cross f g (a,b) = f a, g b (* unit test: see Op.(&&&) *)
  (*$= cross
    (0,1) (cross id id (0,1))
    (2,0) (cross succ pred (1,1))
  *)

  (** [(&&&)] is {!Pair.cross}. *)
  let ( &&& ) = cross           (* unit test: see Op.(&&&) *)

  (** [(diag f g x)] is [(f x, g x)]: applies two different functions
      to the same value, returning a pair of results.

      {[ (diag succ pred x) = (cross succ pred (x,x)) ]}
  *)
  let diag f g x = f x, g x
  (*$= diag
    (0,0) (diag id id 0)
    (2,0) (diag succ pred 1)
  *)
  (*$Q diag
    Q.int (fun x -> diag succ pred x = cross succ pred (x,x))
  *)

  (** [( *** )] is {!Pair.diag}. *)
  let ( *** ) = diag            (* unit test: see Op.( *** ) *)

  (** [(cartesian_product x y)] is the Cartesian product of the two pairs.

      {[ cartesian_product (a,b) (c,d) = [a,c; a,d; b,c; b,d] ]}
  *)
  let cartesian_product (a,b) (c,d) = [a,c; a,d; b,c; b,d]
  (*$Q cartesian_product
    Q.(pair (pair int int) (pair int int)) (fun ((a,b),(c,d)) -> cartesian_product (a,b) (c,d) = [a,c; a,d; b,c; b,d])
  *)

  (** [( * )] is {!cartesian_product}. *)
  let ( * ) = cartesian_product (* unit test: see Op.( * ) *)

  (** [(to_string ?left ?sep ?right f g (a,b))] returns a string representation of [(a,b)].

      [f] is used for the string representation of [a] and [g] for [b].

      The representation of the two values is inserted between [?left]
      (default: ["("]) and [?right] (default: [")"]), separated by [?sep] (default: [", "]).

      Example: [(to_string string_of_int id (12,"foo")) = {|(12, "foo")|}]
  *)
  let to_string ?(left="(") ?(sep=", ") ?(right=")") f g (a,b) = Printf.sprintf "%s%s%s%s%s" left (f a) sep (g b) right
  (*$Q to_string
    Q.(pair int int) (fun (a,b) -> to_string string_of_int string_of_int (a,b) = sprintf "(%s, %s)" (string_of_int a) (string_of_int b))
    Q.(pair string string) (fun (a,b) -> to_string id id (a,b) = sprintf "(%s, %s)" a b)
  *)

  (** [(print ?left ?sep ?right f g)] is [(to_string ?left ?sep ?right f g >> print_endline)]. *)
  let print ?left ?sep ?right f g = to_string ?left ?sep ?right f g >> print_endline

  (** [(random r1 r2)] is a random pair of values [(v1,v2)] s.t. [(v1 = r1 ())] and [(v2 = r2 ())]. *)
  let random r1 r2 () = r1 (), r2 ()
  (* TODO TESTS *)

  (** {1 Ops} *)

  (** Infix and prefix operators. *)
  module Ops = struct (*$< Ops*)
    (** [(&&&)] is {!Pair.cross}. *)
    let ( &&& ) = cross
    (*$= cross;(&&&)
      (cross id id (0,1)) ((id &&& id) (0,1))
      (cross succ pred (1,1)) ((succ &&& pred) (1,1))
    *)

    (** [( *** )] is {!Pair.diag}. *)
    let ( *** ) = diag
    (*$= diag; ( *** )
      (diag id id 0) ((id *** id) 0)
      (diag succ pred 1) ((succ *** pred) 1)
    *)

    (** [( * )] is {!cartesian_product}. *)
    let ( * ) = cartesian_product
    (*$Q ( * );cartesian_product
      Q.(pair (pair int int) (pair int int)) (fun ((a,b),(c,d)) -> cartesian_product (a,b) (c,d) = (a,b) * (c,d))
    *)
  end (*$>*)
end (*$>*)

(** [(&&&)] is {!Pair.cross}. *)
let ( &&& ) = Pair.cross

(** [( *** )] is {!Pair.diag}. *)
let ( *** ) = Pair.diag

(** {1:triples Triples} *)

(** Useful functions on 3-tuples. *)
module T3 = struct (*$< T3*)
  let fst (a,b,c) = a
  (*$Q fst
    Q.(triple int int int) (fun ((a,_,_) as t) -> fst t = a)
   *)
  let snd (a,b,c) = b
  (*$Q snd
    Q.(triple int int int) (fun ((_,b,_) as t) -> snd t = b)
   *)
  let thd (a,b,c) = c
  (*$Q thd
    Q.(triple int int int) (fun ((_,_,c) as t) -> thd t = c)
   *)
  (** [(up a b c)] is [(a,b,c)]. *)
  let up a b c = a,b,c
  (*$= up
    (0,1,2) (up 0 1 2)
  *)

  (** [(map f (a,b,c))] is [(f a, f b, f c)]: applies the same function to
      each element of the triple. *)
  let map f (a,b,c) = f a, f b, f c
  (*$= map
    (0,1,2) (map id (0,1,2))
    (1,2,3) (map succ (0,1,2))
  *)

  (** [(random r1 r2 r3)] is a random triple of values [(v1,v2,v3)]
      s.t. [(v1 = r1 ())], [(v2 = r2 ())] and [(v3 = r3 ())]. *)
  let random r1 r2 r3 () = r1 (), r2 (), r3 ()
  (* TODO TESTS *)
end (*$>*)

(** {1:interval Interval} *)

(** Interval arithmetic over a type [T: ARITH]. *)
module Interval = struct
  module Make (T : ARITH) = struct

    (** The type of [T.t] intervals. *)
    type t = T.t * T.t

    (** [(v lo hi)] is the interval [(lo,hi)]. *)
    let v lo hi = lo, hi

    (** [(map f (a,b))] is [(f a, f b)]: applies the same function to
        each element of the pair. *)
    let map f (a,b) = f a, f b

    open T

    (** [( + )] is interval addition. *)
    let ( + ) (x1,y1) (x2,y2) = (x1+y1),(x2+y2)

    (** [( - )] is interval subtraction. *)
    let ( - ) (x1,y1) (x2,y2) = (x1-y2),(x2-y1)

    (** [( * )] is interval multiplication. *)
    let ( * ) (x1,y1) (x2,y2) = min (min (min (x1*y1) (x1*y2)) (x2*y1)) (x2*y2)

    (** [( / )] is interval division. *)
    let ( / ) (x1,y1) (x2,y2) = (x1,x2) * (map ((/) one) (y1,y2))

    (** [(between (lo,hi) n)] is [true] iff [n] is contained in the {i open} interval [(lo,hi)].

        - [(between (lo,hi) n) = (lo < n && n < hi)]
    *)
    let between (lo,hi) n = lo < n && n < hi

    (** [(contains (lo,hi) n)] is [true] iff [n] is contained in the {i closed} interval [[lo,hi]].

        - [(contains (lo,hi) n) = (lo <= n && n <= hi)]
    *)
    let contains (lo,hi) n = lo <= n && n <= hi

    (** [(inbounds (lo,hi) n)] is [true] iff [n] is contained in the half-open interval [\[lo,hi)].

        This is suitable for bounds-testing 0-indexed data structures
        like {!String}'s and {!Vector}'s.

        - [(inbounds (lo,hi) n) = (lo <= n && n < hi)]

        Example:
        - [String.(if inbounds (0,length str) i then str.[i] else '?')]
    *)
    let inbounds (lo,hi) n = lo <= n && n < hi

    (** {1 Functionals} *)

    (** [foldil f i (a,z)]: folds [f] left-associatively across the closed interval [[a,z]] (tail-recursive).

        [i] is the initial accumulator value.

        Examples:
        - [(foldil ( * ) 1 (1,6)) = ((((((1*1)*2)*3)*4)*5)*6) = 720]
    *)
    let foldil = Pre.foldil

    (** [foldir f i (a,z)]: folds [f] right-associatively across the closed interval [[a,z]] (not tail-recursive)

        [i] is the initial accumulator value.

        Examples:
        - [(foldir ( + ) 0 (1,6)) = (0+(1+(2+(3+(4+(5+6)))))) = 21]
        - [(foldir cons [] (1,6)) = [1; 2; 3; 4; 5; 6]]
    *)
    let foldir f i (a,z) =
      let rec foldir' a =
        if a >= succ z
        then i
        else f a (foldir' (succ a))
      in
      foldir' a

    (** [foreach f (a,z)]: iterates [f] for side-effect across the closed interval [[a,z]] (tail-recursive)

        {[ (let r = ref 1 in foreach (( *:= ) r) (1,6); !r) = 720 ]}
    *)
    let rec foreach f (a,z) =
      if a >= succ z
      then ()
      else (f a; foreach f (succ a, z))

    (** [(to_string ?left ?sep ?right f (a,z))] returns a string representation of the interval [(a,z)].

        The optional parameters are interpreted as for {!Pair.to_string}. *)
    let to_string ?left ?sep ?right f (a,z) = Pair.to_string ?left ?sep ?right f f (a,z)

    (** [(print ?left ?sep ?right f)] is [(to_string ?left ?sep ?right f >> print_endline)]. *)
    let print ?left ?sep ?right f = to_string ?left ?sep ?right f >> print_endline

    (** [(random ())] is a random {!t}. *)
    let random () =
      let a, b = T.random (), T.random () in
      T.(if a < b then a,b else b,a)
    (* TODO TESTS *)
  end

  (** Integer intervals. *)
  module Int = struct
    (** @closed *)
    include (Make (Int))
    let to_string ?left ?sep ?right (a,z) = to_string ?left ?sep ?right string_of_int (a,z)
    let print ?left ?sep ?right = to_string ?left ?sep ?right >> print_endline
  end
  (*$inject module Test = struct open Interval.Int *)
  (*$= v
      (0,1) (v 0 1)
  *)
  (*$= map
    (0,1) (map id (0,1))
    (1,2) (map succ (0,1))
  *)
  (*$= ( + )
    (0,0) ((0,0) + (0,0))
  *)
  (*$= ( - )
    (0,0) ((0,0) - (0,0))
  *)
  (*$= ( * )
    0 ((0,0) * (0,0))
  *)
  (* TODO TESTS for (/) *)
  (*$Q between
    Q.int (fun i -> between (i,i) i |> not)
    Q.(pair neg_int pos_int) (fun (lo,hi) -> between (lo,hi) lo |> not)
    Q.(pair neg_int pos_int) (fun (lo,hi) -> between (lo,hi) hi |> not)
  *)
  (*$T between
    between (0,100) 0 |> not
    between (0,100) 100 |> not
    between (0,100) ~-1 |> not
    between (0,100) 101 |> not
    between (0,100) 1
    between (0,100) 50
    between (0,100) 99
  *)
  (*$Q contains
    Q.int (fun i -> contains (i,i) i)
    Q.(pair neg_int pos_int) (fun (lo,hi) -> contains (lo,hi) lo)
    Q.(pair neg_int pos_int) (fun (lo,hi) -> contains (lo,hi) hi)
  *)
  (*$T contains
    contains (0,100) 0
    contains (0,100) 100
    contains (0,100) ~-1 |> not
    contains (0,100) 101 |> not
    contains (0,100) 1
    contains (0,100) 50
    contains (0,100) 99
  *)
  (*$Q inbounds
    Q.int (fun i -> inbounds (i,i) i |> not)
    Q.(pair neg_int pos_int) (fun (lo,hi) -> inbounds (lo,hi) lo)
    Q.(pair neg_int pos_int) (fun (lo,hi) -> inbounds (lo,hi) hi |> not)
  *)
  (*$T inbounds
    inbounds (0,100) 0
    inbounds (0,100) 100 |> not
    inbounds (0,100) ~-1 |> not
    inbounds (0,100) 101 |> not
    inbounds (0,100) 1
    inbounds (0,100) 50
    inbounds (0,100) 99
  *)
  (*$= foldil
    1   (foldil Stdlib.( * ) 1 (1,0))
    1   (foldil Stdlib.( * ) 1 (1,1))
    2   (foldil Stdlib.( * ) 1 (1,2))
    720 (foldil Stdlib.( * ) 1 (1,6))
  *)
  (*$= foldil
    [] (foldil snoc [] (1,0))
    (rev (1--6)) (foldil snoc [] (1,6))
  *)
  (*$= foldir
    [] (foldir cons [] (1,0))
    (1--6) (foldir cons [] (1,6))
  *)
  (*$= foreach
    1   (let r = ref 1 in (foreach (( *:=) r) (1,0); !r))
    1   (let r = ref 1 in (foreach (( *:=) r) (1,1); !r))
    2   (let r = ref 1 in (foreach (( *:=) r) (1,2); !r))
    720 (let r = ref 1 in (foreach (( *:=) r) (1,6); !r))
  *)
  (*$inject end *)
  end

(** [between] is {!Interval.Int.between}. *)
let between = Interval.Int.between

(** [contains] is {!Interval.Int.contains}. *)
let contains = Interval.Int.contains

(** [inbounds] is {!Interval.Int.inbounds}. *)
let inbounds = Interval.Int.inbounds

(** {1:refs Refs} *)

(** [(r +:= n)] increments the int in [r] by [n]; like C's [+=]. *)
let (+:=) r n = r := !r+n
(*$= (+:=)
  1 (let r = ref 0 in r +:= 1; !r)
*)

(** [(r -:= n)] decrements the int in [r] by [n]; like C's [-=]. *)
let (-:=) r n = r := !r-n
(*$= (-:=)
  ~-1 (let r = ref 0 in r -:= 1; !r)
  9 (let r = ref 10 in r -:= 1; !r)
*)

(** [(r *:= n)] multiplies the int in [r] by [n]; like C's [*=]. *)
let ( *:= ) r n = r := !r*n
(*$= ( *:= )
  0 (let r = ref 0 in r *:= 1; !r)
  20 (let r = ref 10 in r *:= 2; !r)
*)

(** [(r /:= n)] divides the int in [r] by [n]; like C's [/=]. *)
let (/:=) r n = r := !r/n
(*$= ( /:= )
  0 (let r = ref 0 in r /:= 1; !r)
  5 (let r = ref 10 in r /:= 2; !r)
*)

(** [(r @:= x)] conses [x] onto the list in ref [r].  N.B. conses {i not} appends.

    I'd prefer to use [(::=)] as the operator, but OCaml syntax doesn't allow it.
*)
let ( @:= ) r x = r := x :: !r
(*$Q ( @:= )
  Q.(pair int (small_list int)) (fun (x, xs) -> let r = ref xs in r @:= x; !r = (x::xs))
*)

(** {1:printf Printf} *)

(** [Printf] is opened. *)
(** @closed *)
include Printf

(** [(fmt % x)] is [(sprintf fmt x)].*)
let ( % ) fmt x = sprintf fmt x
(*$= (%)
  "123" ("%d" % 000123)
  "foo" ("%s" % "foo")
  "xfooz" ("x%sz" % "foo")
*)

(** {1 Exceptions} *)

(** Functions for working with exceptions.*)
module Exn = struct (*$< Exn *)

  (** [(ignore f x)] is [(f x)] except that any exception is ignored. *)
  let ignore f x = try f x with _ -> ()
  (*$T ignore
    succeeds (ignore (fun _ -> failwith "")) ()
   *)

  (** [fold ?exn f acc x]: tail-recursively nest applications of [(f x acc)]
      until [f] raises an exception.

      If [~exn] is given, only [exn] cleanly terminates the recursion
      and any other exception is reraised; otherwise, any exception
      terminates cleanly.

      {[ (fold f acc x) = (f x (... (f x (f x (f x (f x acc)))))) ]}

      Example: this function counts the number of lines on an input channel:
      {[ fold (fun chan n -> Stdlib.ignore (input_line chan); succ n) 0 ]}
  *)
  let rec fold ?exn f acc x = match f x acc with
  | acc -> fold f acc x
  | exception e -> match exn with Some e' -> if e=e' then acc else raise_notrace e | None -> acc
  (*$= fold
    [] (fold (fun _ _ -> raise Not_found) [] 0)
    [] (fold ~exn:Not_found (fun _ _ -> raise Not_found) [] 0)
    [1] (try fold ~exn:Not_found (fun _ _ -> raise Division_by_zero) [] 0 with Division_by_zero -> [1])
    [] (within (fold (conswith input_line) []) "/dev/null")
  *)
  (*$T fold
    within (fold (conswith input_line) []) "prelude.ml" |> len > 0
  *)

  (** [default d f x]: return [(f x)] unless an exception is raised, in
      which case return default value [d]. *)
  let default = Pre.default
  (* tests: see Pre.default above *)

  (** [(succeeds f x)] evaluates [(f x)] and returns [true] iff the
      evaluation did {i NOT} raise an exception.

      - [(succeeds f x) =  (catch f x |> Option.something)]
  *)
  let succeeds f x = try f x |> Stdlib.ignore; true with _ -> false
  (*$T succeeds
    succeeds id 0
    succeeds (fun n -> n / 0) 100 |> not
    succeeds (fun _ -> failwith "") 100 |> not
  *)

  (** [(reraise exn f x)] is [(f x)], converting any exception to [exn].

      Specifically, [(reraise exn f x)] is [(f x)] unless an exception
      is raised, in which case we instead raise [exn].

      [(reraise ~this exn f x)] is [(f x)] unless some exception [e] is
      raised, in which case we raise:
      - [exn] if [(e = this)]
      - [e]   if [(e <> this)]
  *)
  let reraise ?this that f x = try f x with e -> match this with
  | None              -> raise_notrace that
  | Some t when t = e -> raise_notrace that
  | Some _ (* else *) -> raise_notrace e
  (*$= reraise
    100 (try reraise Not_found (fun n -> n / 0) 1 with Not_found -> 100 | _ -> 99)
    100 (try reraise ~this:Division_by_zero Not_found (fun n -> n / 0) 1 with Not_found -> 100 | _ -> 99)
    100 (try reraise ~this:(Failure "") Not_found (fun n -> n / 0) 1 with Not_found -> 99 | _ -> 100)
  *)

  (** [finalize f g x]: evaluate [(f x)], afterwards calling [(g x)] to
      finalize the resource [x].

      Evaluation of [(g x)] is guaranteed even if [f] raises an
      exception (which is re-raised).

      Process the lines of a file, avoiding a file descriptor
      leak in the case that [process] raises an exception:

      {[ open_in "/etc/passwd" |> finalize process close_in ]}
  *)
  let finalize f (g : 'a -> unit) x = match f x with r -> g x; r | exception e -> g x; raise_notrace e
  (*$= finalize
    (0,0) (let f = ref ~-1 in let r = finalize id (fun n -> f := n) 0 in r, !f)
    (1,0) (let f = ref ~-1 in let r = try finalize (fun _ -> failwith "") (fun n -> f := n) 0 with Failure _ -> 1 in r, !f)
  *)

  (** [(to_string exn)] produces a "better" (IMHO) representation of an exception for end users.

      Specifically, [Sys_error] and [Unix.Unix_error] exceptions, the
      ones most often encountered by end users, are rendered like so:

      [Printexc.to_string]: [(Sys_error "X: No such file or directory")]

      [Exn.to_string]: [X: No such file or directory]

      [Printexc.to_string]: [Unix.Unix_error(Unix.ENOENT, "open", "X")]

      [Exn.to_string]: [X: No such file or directory]

      All other exceptions are just passed to [Printexc.to_string].
  *)
  let to_string = Unix.(function
  | Sys_error err        -> err
  | Unix_error _ as exn  -> Pre.Unix.to_string exn
  | exn                  -> Printexc.to_string exn)
  (*$Q to_string
    Q.unit (fun () -> match Random.int 1_000_000 |> sprintf "%s%d" String.alphabet |> within Stdlib.ignore with exception ((Sys_error e) as exn) -> to_string exn = e | () -> false)
   *)

  (** {1 Labels}
      Short-circuiting Execution

      [return] and [label] taken from Batteries.
      See {{: http://batteries.forge.ocamlcore.org/ } OCaml Batteries Included}.
  *)

  (** The type of labels returning ['a]. *)
  type 'a label = 'a -> exn

  (** [(label f)] creates a label around the invocation of [f], which
      is a computation that can be short-circuited (exited) via [return].

      Usage is: [(label (fun l -> ... return l v ...; d))] where [l] is
      the label, [v] is the value to be returned, and [d] is the
      default value of the entire expression if [return] is never
      called.

      Example: return immediately with [(Some x)], the first value [< 10] in [xs],
      presumably a long list of integers, or [None] if no such value is
      found: [label (fun l -> iter (fun x -> if x < 10 then return l (Some x)) xs; None)]

      Compare this to:
      {[ foldl (fun r x -> if x < 10 && r = None then Some x else r) None xs ]}
      which evaluates to the same value, but always examines every
      element of [xs], where the [label] / [return] version stops at
      the first [x < 10].

      "label f creates a new label x and invokes f x. If, during the
      execution of f, return x v is invoked, the execution of f x
      stops immediately and label f returns v. Otherwise, if f x
      terminates normally and returns y, label f returns y.

      Calling return x v from outside scope f is a run-time error and
      causes termination of the program."
      See {{: https://ocaml-batteries-team.github.io/batteries-included/hdoc2/Batteries.Return.html } Batteries}.
  *)
  (*$Q label;return
     Q.(list small_int) (fun xs -> label (fun l -> iter (fun x -> if x < 10 then return l (Some x)) xs; None) = (foldl (fun r x -> if x < 10 && r = None then Some x else r) None xs))
     Q.(list small_int) (fun xs -> let xs = rev xs in label (fun l -> iter (fun x -> if x < 10 then return l (Some x)) xs; None) = (foldl (fun r x -> if x < 10 && r = None then Some x else r) None xs))
     Q.(list small_int) (fun xs -> let xs = shuffle xs in label (fun l -> iter (fun x -> if x < 10 then return l (Some x)) xs; None) = (foldl (fun r x -> if x < 10 && r = None then Some x else r) None xs))
  *)
  let label (type u) (f : u label -> u) : u =
    let module M = struct exception Return of u end in
    try f (fun x -> M.Return x)
    with M.Return u -> u

  (** [(return label v)] returns to the point where [label] was defined and has the value [v]. *)
  let return label value = raise_notrace (label value)
  (* tests: see label above *)
end (*$>*)

(** [finalize] is [Exn.finalize] *)
let finalize = Exn.finalize

(** [succeeds] is [Exn.succeeds] *)
let succeeds = Exn.succeeds

(** [default] is [Exn.default] *)
let default = Exn.default

(** [reraise] is [Exn.reraise] *)
let reraise = Exn.reraise

(** {1:functionals Functionals} *)

(** [foldil] is {!Interval.Int.foldil}. *)
let foldil = Interval.Int.foldil

(** [foldir] is {!Interval.Int.foldir}. *)
let foldir = Interval.Int.foldir

(** [foreach] is {!Interval.Int.foreach}. *)
let foreach = Interval.Int.foreach

(** [foldex] is {!Exn.fold}. *)
let foldex = Exn.fold

(** [(whilst p f x)] is a functional while-loop; it is [(f ( ... (f (f x))))] while [(p x)] is [true].

    The quaint name is because [while] is an OCaml keyword.

    Example: [(whilst (function x::_ -> x <= 10 | [] -> false) tl) = (dropwhile (fun x -> x <= 10))]
 *)
let whilst = Pre.whilst

(** [(until p f x)] is a functional repeat-until-loop; it is [whilst (not << p) f (f x)].*)
let until p f x = whilst (not $. p) f (f x)
(*$Q until
  Q.unit (fun () -> until (k true) (k 1) 0 = 1)
  Q.small_int (fun m -> m < 0 || until (fun n -> n > m) succ 0 = m+1)
 *)

(** [(iff p consequent alternative x)] is [if p x then consequent x else alternative x].

    Examples: [(iff even cons (fun _ xs -> xs)) = (conswhen even)]
    {[ (foldr (iff even cons (fun _ xs -> xs)) [] (1--20)) = [2; 4; 6; 8; 10; 12; 14; 16; 18; 20] ]}
*)
let iff p cons alt x = if p x then cons x else alt x
(*$= iff
  ~-1 (iff ((=) 0) (k ~-1) id 0)
 *)
(*$Q iff
  Q.pos_int (fun i -> iff ((=) 0) (k ~-1) id i = i)
 *)

(** [(onlyif p f x)] is [(f x)] iff [(p x)] and is [x] otherwise.

    Example: [(map (onlyif even (k 0)) (1--10)) = [1; 0; 3; 0; 5; 0; 7; 0; 9; 0]]
*)
let onlyif p f x = if p x then f x else x
(*$= onlyif
  ~-1 (onlyif ((=) 0) (k ~-1) 0)
 *)
(*$Q onlyif
  Q.pos_int (fun i -> onlyif ((=) 0) (k ~-1) i = i)
 *)

(** {1:seq Seq} *)

(** Functional Iterators *)
module Seq = struct (*$< Seq *)
  (* TODO TESTS *)
  (** @closed *)
  include Seq

  (** [(length xs)] is the length of the sequence [xs]; tail recursive.

      The traversal happens immediately and will not terminate on infinite sequences.
   *)
  let length s = fold_left (fun n _ -> succ n) 0 s

  (** [len] is [length]. *)
  let len = length

  (** [foldl] is {!Seq.fold_left}. *)
  let foldl = fold_left

  (** [(index ?z xs)] is a sequence of pairs; the [fst]'s of the pairs
     are successive integers (starting from [z] (default: 0) and the
     [snd]'s of the pairs are the elements of [xs]. *)
  let index ?(z=0) xs =
    let rec loop i xs = match xs () with
      | Nil         -> fun () -> Nil
      | Cons (x,xs) -> fun () -> Cons ((i, x), loop ~++i xs)
    in
    loop z xs

  (** [(to_list xs)] is the list consisting of the elements of the sequence [xs].

      The traversal happens immediately and will not terminate on infinite sequences.
   *)
  let to_list t = fold_left (flip List.cons) [] t |> List.rev

  (** {1 I/O} *)

  (* TODO DOC *)
  let of_lines chan =
    let eachline _ = match input_line chan with
      | exception End_of_file -> None
      | line                  -> Some (line, chan)
    in
    unfold eachline chan

  (* TODO DOC *)
  let of_chars chan =
    let eachchar _ = match input_char chan with
      | exception End_of_file -> None
      | char                  -> Some (char, chan)
    in
    unfold eachchar chan

  (** {1 Random Value} *)

  (** [(random ?size r ())] is a random sequence of size [(size ())]
      (default: [< 100]) elements given by [(r ())]. *)
  let random ?size r () =
    let n = match size with None -> Random.int 100 | Some f -> f () in
    let each n =
      if n = 0
      then None
      else Some (r (), pred n)
    in
    unfold each n
  (*$Q random
    Q.small_int (fun n -> random ~size:(k n) Int.random () |> Seq.length = n)
   *)

end (*$>*)

(** {1:options Options} *)

(** Option monad and other useful functions on options. *)
module Option = struct          (*$< Option *)
  (** @closed *)
  include Option

  (** {1 Options } *)

  (** [(return x)] is [(Some x)]. *)
  let return x = Some x
  (*$= return
    (Some 0) (return 0)
  *)

  (** [some] is {!return}. *)
  let some = return
  (*$= some
    (return 0) (some 0)
  *)

  (** [(get x)] is [v] if [(x = Some v)] and raises [Invalid_argument] otherwise. *)
  let get = Option.get
  (*$T get
    0 = get (Some 0)
    "foo" = get (Some "foo")
    (try get None |> ignore; false with Invalid_argument _ -> true | _ -> false)
  *)

  (** [(default d)] is [(] {!either} [id d)] *)
  let default default o = Option.value o ~default
  (*$= default
    0 (default 0 None)
    99 (default 0 (Some 99))
  *)

  (** {1 Predicates} *)

  (** [(something r)] is [true] if [(r = Some v)] for any [v] and [false] otherwise. *)
  let something = function Some _ -> true | None -> false
  (*$T something
    something None |> not
    something (Some "foo")
    something (Some 99)
  *)

  (** [(nothing r)] is [true] if [(r = None)] and [false] otherwise. *)
  let nothing o = something o |> not
  (*$T nothing
    nothing None
    nothing (Some 0) |> not
    nothing (Some "foo") |> not
  *)
  (*$Q nothing;something
    Q.(option int) (fun o -> something o = not (nothing o))
  *)

  (** {1 Option Monad} *)

  (** [(bind o f)] is [(Some (f v))] if [(o = Some v)] and [None] if [(o = None)]. *)
  let bind = Option.bind
  (*$= bind
    None (bind None id)
    None (bind None (k (Some 0)))
    (Some 0) (bind (Some 0) some)
    (Some 1) (bind (Some 0) (k (Some 1)))
  *)
  (*$Q bind
    Q.int (fun i -> bind (Some i) (fun n -> Some (succ n)) = (Some (succ i)))
  *)

  (** [(>>=)] is {!bind}. *)
  let (>>=) = bind              (* unit test: see Op.(>>=) *)

  (** [(map f o)] is [(bind o (fun v -> Some (f v)))]. *)
  let map = Option.map
  (*$= map
    None (map id None)
    None (map (k 0) None)
    (Some 0) (map id (Some 0))
    (Some 1) (map (k 1) (Some 0))
  *)
  (*$Q map
    Q.int (fun i -> map succ (Some i) = (Some (succ i)))
  *)

  (** [(>>|)] is [flip] {!map}. *)
  let (>>|) o f = map f o              (* unit test: see Op.(>>|) *)

  (** [(>=>)] is monadic composition: [((f >=> g) x = (f x) >>= g)] *)
  let (>=>) f g x = bind (f x) g
  (* TODO TEST *)

  (** [(join o)] is [v] if [(o = Some v)] and [o] otherwise. *)
  let join = Option.join
  (*$= join
    None (join None)
    None (join (Some None))
    (Some 1) (join (Some (Some 1)))
  *)

  (** [(on_none o f)] is [(Some (f ()))] if [o] is [None] and [o] otherwise. *)
  let on_none o f = match o with
  | None        -> Some (f ())
  | (Some _) as o -> o
  (*$= on_none
    (Some 1) (on_none None (fun () -> 1))
    (Some 2) (on_none (Some 2) (fun () -> 1))
  *)

  (** [(>>/)] is {!on_none} *)
  let (>>/) = on_none

  (** [reduce] is [(filter something $ List.map get)], i.e. it throws away [None]'s and pulls the values out of [Some]'s.

      Example: [(map (catch (fun x -> 2 / x)) [-1;0;1] |> Option.reduce) = [-2; 2]]
  *)
  let reduce list =
    let rec loop acc = function
      | Some x :: xs -> loop (x :: acc) xs
      |   None :: xs -> loop acc xs
      |           [] -> List.rev acc
    in
    loop [] list
  (*$= reduce
    [] (reduce [])
    [] (repeat 1 None |> reduce)
    [] (repeat 10 None |> reduce)
  *)
  (*$Q reduce
    Q.(small_list (option int)) (fun xs -> reduce xs = (filter something $ List.map get) xs)
  *)

  (** {1 Functionals} *)

  (** [(foldl f z o)] is [(f z x)] iff [(o = Some x)], and otherwise is [z]. *)
  let foldl f z = function None -> z | Some x -> f z x
  (*$= foldl
    0 (foldl (fun _ _ -> failwith "") 0 None)
    11 (foldl (+) 1 (Some 10))
    [10] (foldl snoc [] (Some 10))
  *)

  (** [(foldr f z o)] is [(f x z)] iff [(o = Some x)], and otherwise is [z]. *)
  let foldr f z = function None -> z | Some x -> f x z
  (*$= foldr
    0 (foldr (fun _ _ -> failwith "") 0 None)
    11 (foldr (+) 1 (Some 10))
    [10] (foldr cons [] (Some 10))
  *)

  (** [(either some none x)] is [(some v)] if [x] is [(Some v)] and [none] otherwise. *)
  let either some none = function Some x -> some x | None -> none
  (*$Q either
    Q.(option int) (fun o -> either (snoc []) [] o = foldl snoc [] o)
    Q.(option int) (fun o -> either (snoc []) [] o = foldr cons [] o)
  *)

  (** [(maybe f o)] is [(f x)] if [o] is [(Some x)], and [()] otherwise. *)
  let maybe f = function None -> () | Some x -> f x
  (*$T maybe
    (let r = ref 0 in maybe (fun x -> incr r) None = () && !r = 0)
    (let r = ref 0 in maybe (fun x -> incr r) (Some "") = () && !r = 1)
   *)

  (** [(call def fopt x)] is [(f x)] if [fopt] is [(Some f)] and is [def] otherwise.

      This is nice for optional function parameters:
      {[
      let f ?error x = match f x with
      | exception exn -> Exn.to_string exn |> Option.call () error; []
      | list -> list
      ]}
  *)
  let call def f x = match f with Some f -> f x | None -> def
  (*$= call
    1 (call 0 (Some (k 1)) 100)
    0 (call 0 None 100)
  *)

  (** {1 Exception Handling} *)

  (** [(catch ?this f x)] is [(Some (f x))] unless [f] raises an exception, in which case it is [None].

      Any exception will result in [None]; if you want more precision,
      specify the exception with [~this] -- in this case, any other
      exception will be re-raised: e.g.  [(catch ((/) 1) 0)] is [None]
      while [(catch ~this:Not_found ((/) 1) 0)] raises
      [Division_by_zero].*)
  let catch ?this f x = try Some (f x) with e -> match this with
    | None              -> None
    | Some t when t = e -> None
    | Some _ (* else *) -> raise_notrace e
  (*$= catch
    (Some 1) (catch id 1)
    (Some 2) (catch (k 2) 1)
    None (catch failwith "foo")
  *)
  (*$T catch
    (try catch ~this:Not_found failwith "foo" |> ignore; false with Failure _ -> true | _ -> false )
  *)

  (** {1 Type Conversion} *)

  (** [(to_bool o)] is [false] if [(o = None)] and is [true] otherwise. *)
  let to_bool = function None -> false | Some _ -> true
  (*$Q to_bool
    Q.(option int) (fun o -> to_bool o && something o || nothing o)
   *)

  (** [(to_list o)] is [[v]] if [(o = Some v)] and is [[]] otherwise. *)
  let to_list = Option.to_list
  (*$= to_list
    [] (to_list None)
    [1] (to_list (Some 1))
  *)

  (** [to_exn exn (Some x)] is [x] and [to_exn exn None] raises [exn]. *)
  let to_exn exn = function None -> raise_notrace exn | Some x -> x
  (*$T to_exn
    1 = (to_exn Not_found (Some 1))
    not (succeeds (to_exn Not_found) None)
   *)

  (** [(of_result r)] is [(Some x)] iff [(r = Ok x)] and is [None] otherwise. *)
  let of_result = function Ok x -> Some x | Error _ -> None
  (* TODO TESTS *)

  (** {1 Development Utils} *)

  (** [(to_string conv o)] converts the [('a option) o] to a string
      using the conversion function [conv].

      Examples:
      - [(to_string string_of_int None) = "None"]
      - [(to_string string_of_int (Some 12)) = "Some 12"]
   *)
  let to_string conv = function None -> "None" | Some x -> conv x |> sprintf "Some %s"
  (*$= to_string
    "None" (to_string (k "") None)
    "Some foo" (to_string (k "foo") (Some 0))
    "Some foo" (to_string (k "foo") (Some "bar"))
   *)

  (** [(print conv)] is [(to_string conv >> print_endline)]. *)
  let print conv = to_string conv >> print_endline

  (** [(random ())] is, with equal probability, either [None] or [(Some v)] s.t. [(v = r ())]. *)
  let random r () = if Random.bool () then None else Some (r ())

  (** {1 Ops} *)

  (** Infix and prefix operators. *)
  module Ops = struct           (*$< Ops *)

    (** [(>>=)] is {!bind}. *)
    let (>>=) = (>>=)
    (*$Q (>>=); bind
      Q.int (fun i -> bind (Some i) (fun n -> Some (succ n)) = ((Some i) >>= (fun n -> Some (succ n))))
    *)

    (* [( let* )] is {!bind}. *)
    let ( let* ) = bind

    (** [(>>|)] is {!map}. *)
    let (>>|) = (>>|)
    (*$Q (>>|); map
      Q.int (fun i -> map succ (Some i) = ((Some i) >>| succ))
    *)

    (** [(>=>)] is {!(>=>)}. *)
    let (>=>) = (>=>)
    (* TODO TEST *)

    (** [(>>/)] is {!on_none} *)
    let (>>/) = on_none
  end (*$>*)
end (*$>*)

(** [catch] is {!Option.catch} *)
let catch = Option.catch

(** [some] is {!Option.some} *)
let some = Option.some

(** [none] is {!Option.none} *)
let none = Option.none

(** [something] is {!Option.something} *)
let something = Option.something

(** [nothing] is {!Option.nothing} *)
let nothing = Option.nothing

(** {1:results Results} *)

(** Result (error) monad.

    See {{: http://erratique.ch/software/rresult }Rresult} for a much more sophisticated package.
 *)
module Result = struct (*$< Result *)
  (** {1 Constructors} *)

  (** [(ok x)] is [(Ok x)]. *)
  let ok x = Ok x
  (*$= ok
    (Ok 0) (ok 0)
    (Ok "") (ok "")
  *)

  (** [(error x)] is [(Error x)]. *)
  let error x = Error x
  (*$= error
    (Error 0) (error 0)
    (Error "") (error "")
  *)

  (** {1 Predicates} *)

  (** [(good x)] is [true] if [(x = Ok _)] and [false] if [(x = Error _)]. *)
  let good = function Ok _ -> true | Error _ -> false
  (*$T good
    good (Ok 1)
    not (good (Error 1))
  *)

  (** [(bad x)] is [true] if [(x = Error _)] and [false] if [(x = Ok _)]. *)
  let bad = function Ok _ -> false | Error _ -> true
  (*$T bad
    bad (Error 1)
    not (bad (Ok 1))
  *)

  (** {1 Selectors} *)

  (** [(get_ok r)] is [x] iff [(r = (Ok x))].

      @raise Invalid_argument when [(r = (Error _))]
  *)
  let get_ok = function Ok x -> x | Error _ -> invalid_arg "get_ok"
  (*$T get_ok
    get_ok (Ok 0) = 0
    (try get_ok (Error "err") |> ignore; false with Invalid_argument _ -> true | _ -> false)
  *)

  (** [(get_error r)] is [x] iff [(r = (Error x))].

      @raise Invalid_argument when [(r = (Ok _))]
  *)
  (*$T get_error
    get_error (Error 0) = 0
    (try get_error (Ok "") |> ignore; false with Invalid_argument _ -> true | _ -> false)
  *)
  let get_error = function Error x -> x | Ok _ -> invalid_arg "get_error"

  (** [(default d r)] is [x] when [r = Ok x], and [d] otherwise. *)
  let default d = function Ok x -> x | Error _ -> d
  (*$Q default
    Q.int (fun n -> n = (default 0 (Ok n)))
    Q.int (fun n -> 0 = (default 0 (Error n)))
    Q.string (fun s -> 0 = (default 0 (Error s)))
  *)

  (** {1 Operators} *)

  (** [(bind r f)] is [(Ok (f v))] if [(r = Ok v)] and [r] if [(r = Error e)]. *)
  let bind r f = match r with Ok x -> f x | Error _ as err -> err
  (*$= bind
    (Ok 1) (bind (Ok 1) ok)
    (Ok 12) (bind (Ok 1) (fun _ -> ok 12))
    (Error "err") (bind (Error "err") ok)
  *)

  (** [(>>=)] is {!bind}. *)
  let (>>=) = bind              (* unit test: see Op.(>>=) *)

  (** [(map f r)] is [(bind r (fun v -> Ok (f v)))]. *)
  let map f r = match r with Ok x -> Ok (f x) | Error _ as err -> err
  (*$= map
     (Ok 1) (map id (Ok 1))
     (Ok 12) (map (k 12) (Ok 1))
     (Error "err") (map id (Error "err"))
  *)

  (** [(>>|)] is {!map}. *)
  let (>>|) r f = map f r              (* unit test: see Op.(>>=) *)

  (** [(>=>)] is monadic composition: [((f >=> g) x = (f x) >>= g)] *)
  let (>=>) f g x = bind (f x) g
  (* TODO TEST *)

  (** [(join r)] is v if [r = Ok v] and [r] otherwise.

      This is particularly useful when nesting [trap]'s:
      {[ trap Exn.to_string (withcd (fun _ -> trap Exn.to_string readfile "NONEXISTENT")) "/etc"
= Ok (Error "NONEXISTENT: No such file or directory") ]}
      whereas:
      {[ trap Exn.to_string (withcd (fun _ -> trap Exn.to_string readfile "NONEXISTENT")) "/etc" |> join
= Error "NONEXISTENT: No such file or directory" ]}
   *)
  let join = function
    | Ok x           -> x
    | Error _ as err -> err
  (*$Q join
    Q.int (fun a -> Ok (a / 2) = (trap id (trap id ((/) a)) 2 |> join))
    Q.int (fun a -> Error Division_by_zero = (trap id (trap id ((/) a)) 0 |> join))
   *)

  (** [(reduce rs)] is the list of all the values carried by [Ok]'s in
      the list, with all the [Error]'s tossed out. *)
  let reduce list =
    let rec loop acc = function
      | Ok    x :: xs -> loop (x :: acc) xs
      | Error _ :: xs -> loop acc xs
      |            [] -> List.rev acc
    in
    loop [] list
  (*$Q reduce
    Q.(list (option int)) (fun os -> let rs = List.map (Option.to_result ~none:"") os in foldl (fun acc -> function Ok x -> x::acc | _ -> acc) [] rs |> rev = reduce rs)
   *)

  (** [(on_error r f)] is [(f e)] if [r] is [(Error e)] and is otherwise [(Ok x)]. *)
  let on_error r f = match r with
  | Error e -> f e
  | Ok x    -> Ok x
  (*$Q on_error
    Q.int (fun n -> on_error (Error n) (k (Ok ~++n)) = Ok ~++n)
    Q.int (fun n -> on_error (Ok n) (k (Ok ~++n)) = Ok n)
   *)

  (** [(>>/)] is {!on_error} *)
  let (>>/) r f = on_error r f
  (* test: see on_error above *)

  (** @deprecated Use {!(>>/)}. *)
  let (//=)[@deprecated] = on_error

  (** [a or b] is [Ok x] if [(a = Ok x)] and is otherwise [b]. *)
  (* TODO BUG just rename (>>/) to (or)!  it'll be lazy! *)
  let (or) a b = match a with Ok _ as r -> r | Error _ -> b
  (* TODO TESTS *)

  (** [(always r f)] is [(f ())], regardless of what [r] is. *)
  let always _r f = f ()
  (*$Q always
    Q.int (fun n -> always (Ok n) (k ~++n) = ~++n && always (Error n) (k ~++n) = ~++n)
   *)

  (** [( //* )] is {!always}. *)
  let ( //* )  = always
  (* test: see always above *)

  (** {1 Exceptions} *)

  (** [(trap e f x)] is [(Ok (f x))] unless an exception [exn] is raised, in which case
      it is [(Error (e exn))].
  *)
  let trap e f x = try Ok (f x) with exn -> Error (e exn)
  (*$= trap
    (Ok 1) (trap id id 1)
    (Ok 12) (trap id (k 12) 1)
    (Error Not_found) (trap id (fun _ -> raise Not_found) 1)
    (Error "err") (trap (k "err") (fun _ -> raise Not_found) 1)
  *)

  (** [(trapc c f x)] is [(Ok (f x))] unless an exception [exn] is raised, in which case
      it is [(Error c)].
  *)
  let trapc c f x = try Ok (f x) with exn -> Error c
  (*$= trapc
    (Ok 1) (trapc 0 id 1)
    (Ok 12) (trapc 0 (k 12) 1)
    (Error 0) (trapc 0 (fun _ -> raise Not_found) 1)
  *)

  (** {1 Error Manipulation} *)

  (** [(witherr f r)] is [(Error (f e))] iff [(r = Error e)] and is otherwise [r]. *)
  let witherr f = function Ok _ as r -> r | Error e -> Error (f e)
  (*$= witherr
    (Ok 1) (witherr id (Ok 1))
    (Ok 1) (witherr (k "err") (Ok 1))
    (Error "err") (witherr id (Error "err"))
    (Error "foo") (witherr (k "foo") (Error "err"))
    (Error 1) (witherr (k 1) (Error "err"))
  *)

  (** [(witherrc e r)] is [(Error e)] iff [(r = Error _)] and is otherwise [r]. *)
  let witherrc e = function Ok _ as r -> r | Error _ -> Error e
  (*$= witherrc
    (Ok 1) (witherrc 0 (Ok 1))
    (Ok 1) (witherrc 0 (Ok 1))
    (Error 0) (witherrc 0 (Error "err"))
  *)

  (** {1 Type Conversion} *)

  (** [(to_bool result)] is [true] iff [result] is [Ok _] and [false] otherwise. *)
  let to_bool = function Ok _ -> true | Error _ -> false
  (*$Q to_bool
    Q.int (fun n -> Ok n |> to_bool)
    Q.int (fun n -> Error n |> to_bool |> not)
  *)

  (** [(of_bool ?err x bool)] converts [true] to [Ok x] and [false] to
      [Error err], where the default value for [err] is [x]. *)
  let of_bool ?err str = function true -> Ok str | false -> Error (Option.default str err)
  (*$Q of_bool
    Q.bool (fun b -> of_bool 1 b = (if b then Ok 1 else Error 1))
    Q.bool (fun b -> of_bool ~err:2 1 b = (if b then Ok 1 else Error 2))
   *)

  (** [(to_option r)] is [(Some x)] iff [(r = Ok x)] and is [None] otherwise. *)
  let to_option = function Ok x -> Some x | Error _ -> None
  (*$Q to_option
    Q.int (fun n -> to_option (Error n) = None)
    Q.int (fun n -> to_option (Ok n) = Some n)
   *)

  (** [(of_option err o)] is [(Ok x)] iff [(o = Some x)] and is [(Error err)] otherwise. *)
  let of_option err = function Some x -> Ok x | None -> Error err
  (*$Q of_option
    Q.int (fun n -> of_option n None = Error n)
    Q.int (fun n -> of_option "" (Some n) = Ok n)
   *)

  (** [(some_error r)] is in some sense the inverse of {!to_option}:
      it is [None] if [r] is [Ok], and [(Some err)] if [r] is [(Error err)]. *)
  let some_error = function Ok ok -> None | Error err -> Some err
  (*$Q some_error
    Q.unit (fun () -> some_error (Ok ()) = None )
    Q.unit (fun () -> some_error (Error ()) = Some ())
   *)

  (** {1 Sequences} *)

  (** Functions on sequences of results. *)
  module Seq = struct (*$< Seq *)
    module S = Seq

    (** [(oks seq)] is the sequence of [Ok] values from the sequence [seq]. *)
    let oks seq = Seq.filter_map Result.to_option seq
    (*$T oks
      S.random (random ~ok:(k 1) ~error:(k 2)) () |> oks |> S.to_list |> all ((=) 1)
     *)

    (** [(errors seq)] is the sequence of [Error] values from the sequence [seq]. *)
    let errors seq = Seq.filter_map some_error seq
    (* TODO TESTS *)
  end (*$>*)

  (** {1 Development Utils} *)

  (** [(to_string ~error ~ok r)] is a string representation of [r],
      where [~error] converts the [Error] case and [~ok] the [Ok] case. *)
  let to_string ~error ~ok = function Error err -> "Error %s" % error err | Ok x -> "Ok %s" % ok x

  (** [(print ~error ~ok)] = [(to_string ~error ~ok >> print_endline)]. *)
  let print ~error ~ok = to_string ~error ~ok >> print_endline

  (** [(random ~error ~ok ())] is either [(Ok (ok ()))] or [(Error (error ()))] with equal probability. *)
  let random ~error ~ok () =
    if Random.bool () then Error (error ()) else Ok (ok ())

  (** {1 Infix Operators} *)

  (** Infix operators. *)
  module Ops = struct (*$< Ops *)

    (** [(>>=)] is {!bind}. *)
    let (>>=) = (>>=)
    (*$= (>>=)
      (bind (Ok 1) ok) ((Ok 1) >>= ok)
      (bind (Ok 1) (fun _ -> ok 12)) ((Ok 1) >>= (fun _ -> ok 12))
      (bind (Error "err") ok) ((Error "err") >>= ok)
    *)

    (* [( let* )] is {!bind}. *)
    let ( let* ) = bind

    (** [(>>|)] is {!map}. *)
    let (>>|) = (>>|)
    (*$= (>>|)
       (map id (Ok 1)) ((Ok 1) >>| id)
       (map (k 12) (Ok 1)) ((Ok 1) >>| (k 12))
       (map id (Error "err")) ((Error "err") >>| id)
    *)

    (** [(>=>)] is {!(>=>)}. *)
    let (>=>) = (>=>)

    (** [(>>/)] is {!(>>/)}. *)
    let (>>/)  = (>>/)

    (** [(//* )] is {!(//* )}. *)
    let ( //* )  = always

    (** @deprecated  Use {!(>>/)}. *)
    let (//=)[@deprecated]  = on_error

  end (*$>*)
end (*$>*)

(* TODO ?buf optional reuse of a buffer *)
(** {1:buffers Buffer} *)

(** Additional Buffer functions. *)
module Buffer = struct (*$< Buffer *)
  (** @closed *)
  include Buffer

  (** [(scons s b)] adds the string [s] to buffer [b], returning [b].

      Useful for folding into a buffer accumulator. *)
  let scons s b = add_string b s; b
  (*$Q scons
    Q.string (fun s -> s = withbuf 10 (scons s))
  *)

  (** [snocs] is [(flip scons)].

      Example: [(fun xs -> withbuf 100 (flip (foldl snocs) xs))] is [(String.concat "")].
  *)
  let snocs b s = add_string b s; b
  (*$Q snocs
    Q.string (fun s -> s = withbuf 10 (flip snocs s))
  *)

  (** [(ccons c b)] adds the char [c] to buffer [b], returning [b]. *)
  let ccons c b = add_char b c; b
  (*$= ccons
    "x" (withbuf 10 (ccons 'x'))
  *)
  (*$Q ccons
    Q.char (fun c -> String.make 1 c = (withbuf 10 (ccons c)))
  *)

  (** [(withbuf2 n f)] is [((let r = f buf in Buffer.contents buf, r))]
      where [buf] is a fresh [Buffer.t] of size [n]. *)
  let withbuf2 size f = Buffer.(let buf = create size in let r = f buf in contents buf, r)
  (*$Q withbuf2
    Q.(pair small_int string) (fun (n,s) -> withbuf2 n (scons s) |> (id &&& contents) = (s,s))
  *)

  (** [(withbuf size f)] is [(withbuf2 size f |> fst)].

      It discards the value of [(f buf)], which is frequently [()].

      This abstracts the common pattern:

      {[
      let f x =
        let buf = Buffer.create n in
        let f' () =
          something (); Buffer.add_THING buf thing; something ()
        in
        f' () |> ignore; Buffer.contents buf
      ]}

      into:

      {[ let f' buf = ... Buffer.add_... buf ... in withbuf n f' ]}

      For example:

      {[ let implode cs = withbuf (len cs) (fun b -> iter (Buffer.add_char b) cs) ]}
  *)
  let withbuf size f = withbuf2 size f |> fst
  (*$Q withbuf
    Q.(pair small_int string) (fun (n,s) -> withbuf n (scons s) = s)
  *)
end (*$>*)

(** [withbuf2] is {!Buffer.withbuf2}. *)
let withbuf2 = Buffer.withbuf2
(*$Q withbuf2
  Q.(pair small_int string) (fun (n,s) -> withbuf2 n (Buffer.scons s) |> (id &&& Buffer.contents) = (Buffer.withbuf2 n (Buffer.scons s) |> (id &&& Buffer.contents)))
*)

(** [withbuf] is {!Buffer.withbuf}. *)
let withbuf = Buffer.withbuf
(*$Q withbuf
  Q.(pair small_int string) (fun (n,s) -> withbuf n (Buffer.scons s) = Buffer.withbuf n (Buffer.scons s))
*)

(** {1:lists Lists} *)

(** Additional List functions and renamings.

    We also include all the [List] functions that we want when we
    [include Lists] in {!Prelude}.
*)
module Lists = struct (*$< Lists *)
  open List

  (**/**)
  type 'a t = 'a list
  let compare = Stdlib.compare
  (**/**)
  (* What's the deal with compare?
   * Prelude has always opened List.
   * Recently the List module got a function compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
   * (This is the same as Prelude.List.comparison.)
   * This broke a ton of code in Prelude, and in all my programs that use Prelude.
   * The fix is just to do let compare = Stdlib.compare after opening List.
   * The new List.compare is of course accessible as Stdlib.List.compare, and Prelude.List.comparison
   *   is now implemented as let comparison = Stdlib.List.compare
   *)

  (** {1:lists_basic Basic Functions} *)

  (** [len] is [List.length]. *)
  let len = List.length
  (*$= len
    0 (len [])
    1 (len [100])
    2 (len [100;0])
  *)

  (** [comparison] is [Stdlib.List.compare]. *)
  let comparison = Stdlib.List.compare
  (*$Q comparison
    Q.unit (fun () -> comparison compare [] [] = compare [] [])
    Q.(list small_printable_string) (fun xs -> comparison compare xs xs = 0)
    Q.(list small_printable_string) (fun xs -> xs = [] || let ys,zs = (map String.uppercase_ascii xs), (map String.lowercase_ascii xs) in comparison compare ys zs = compare ys zs)
    Q.(list small_printable_string) (fun xs -> xs = [] || comparison AIString.compare (map String.uppercase_ascii xs) (map String.lowercase_ascii xs) = 0)
    Q.(pair (list small_printable_string) (list small_printable_string)) (fun (xs,ys) -> comparison compare xs ys = compare xs ys)
  *)

  (** [(to_string ?left ?sep ?right f xs)] returns a string representation of [xs].

      [f] is used for the string representation of each element of [xs].

      The representation of the list is inserted between [?left]
      (default: ["\["]) and [?right] (default: ["\]"]); each element of
      [xs] is separated by [?sep] (default: ["; "]).

      Example: [(to_string string_of_int (1--10)) = "[1; 2; 3; 4; 5; 6; 7; 8; 9; 10]"]
  *)
  let to_string ?(left="[") ?(sep="; ") ?(right="]") f xs =
    let open Buffer in
    let buf = create 256 in
    match xs with
    |    [] -> left ^ right
    | x::xs ->
        add_string buf left;
        add_string buf (f x);
        iter (fun x -> f x |> (^) sep |> add_string buf) xs;
        add_string buf right;
        contents buf
  (*$= to_string
    "[]"        (to_string id [])
    "()"        (to_string ~left:"(" ~right:")" id [])
    "[1]"       (to_string string_of_int [1])
    "[1; 2]"    (to_string string_of_int [1;2])
    "[1; 2; 3]" (to_string string_of_int [1;2;3])
    "(1)"       (to_string ~left:"(" ~right:")" ~sep:"," string_of_int [1])
    "(1,2)"     (to_string ~left:"(" ~right:")" ~sep:"," string_of_int [1;2])
    "(1,2,3)"   (to_string ~left:"(" ~right:")" ~sep:"," string_of_int [1;2;3])
  *)

  (** {1:lists_accessors Accessors} *)
  (** [(hd xs)] is the first element of xs.

      @raise Failure if the list is empty.
  *)
  let hd = List.hd
  (*$= hd
    12 (hd [12])
  *)
  (*$T hd
    try ignore (hd []); false with Failure _ -> true
  *)

  (** [(head xs)] is [None] if [(xs = [])] and [(Some (hd xs))] otherwise. *)
  let head = function [] -> None | x::_ -> Some x
  (*$= head
    None (head [])
  *)
  (*$Q head;hd
    Q.(list small_int) (fun l -> match head l with Some x -> x = hd l | None -> l = [])
  *)

  (** [(init xs)] is all the elements of [xs] except the last one; tail-recursive

      {[ (init xs) = (rev $ tl $ rev) ]}

      @raise Failure if the list is empty.
  *)
  let init xs = rev xs |> function [] -> failwith "init" | _::tl -> rev tl
  (*$Q init
    Q.(list small_int) (fun xs -> xs = [] || init xs = (rev $ tl $ rev) xs)
  *)
  (*$T init
    try ignore (init []); false with Failure _ -> true
  *)

  (** [(tl (x::xs))] is [xs].

      @raise Failure if the list is empty.
  *)
  let tl = List.tl
  (*$= tl
    [] (tl [12])
    [45;1] (tl [12;45;1])
  *)
  (*$T tl
    try ignore (tl []); false with Failure _ -> true
  *)

  (** [(tail xs)] is [None] if [(xs = [])] and [(Some (tl xs))] otherwise. *)
  let tail = function [] -> None | _::xs -> Some xs
  (*$= tail
    None (tail [])
  *)
  (*$Q tail;tl
    Q.(list small_int) (fun l -> match tail l with Some x -> x = tl l | None -> l = [])
  *)

  (** [(last xs)] is the last element of [xs]; tail-recursive.

      {[ (last xs) = (hd (rev xs)) ]}

      @raise Failure if the list is empty.
  *)
  let rec last = function
  |    [] -> failwith "last"
  |   [x] -> x
  | _::xs -> last xs
  (*$Q last
    Q.(list small_int) (fun xs -> xs = [] || last xs = (hd (rev xs)))
  *)
  (*$T last
    try ignore (last []); false with Failure _ -> true
  *)

  (** [(get)] is [(flip List.nth)]. *)
  let get n list = List.nth list n
  (*$Q get
    Q.(pair small_int (list small_int)) (fun (i,l) -> i >= len l || get i l = List.nth l i)
  *)

  (** {1:lists_building Building Lists} *)

  (** [cons] is [List.cons]. *)
  let cons x xs = x :: xs
  (*$= cons
    [1] (cons 1 [])
  *)
  (*$Q cons
    Q.(pair int (list small_int)) (fun (i,l) -> cons i l = List.cons i l)
  *)

  (** [snoc] is [(flip cons)]. *)
  let snoc xs x = x::xs
  (*$= snoc
    [1] (snoc [] 1)
  *)
  (*$Q snoc;cons
    Q.(pair int (list small_int)) (fun (i,l) -> snoc l i = cons i l)
  *)

  (** [(consup x)] is [(x::[])]. *)
  let consup x = x::[]
  (*$Q consup;cons
    Q.int (fun i -> consup i = cons i [])
  *)

  (** [(revcons)] is [(xs @ [x])]. *)
  let revcons x xs = xs @ [x]

  (** [(unfoldr p f g x)] builds a list [(f x :: unfoldr p f g (g x))]
      from a seed value [x] until [((p x) = true)].  {i Not} tail-recursive.

      Example:
      - [(unfoldr ((=) 0) id pred 10) = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]]
  *)
  let rec unfoldr p f g x =
    if p x then [] else f x :: unfoldr p f g (g x)
  (*$= unfoldr
    [] (unfoldr ((=) 0) id pred 0)
    [10; 9; 8; 7; 6; 5; 4; 3; 2; 1] (unfoldr ((=) 0) id pred 10)
  *)
  (*$Q revcons;last
    Q.(pair int (list small_int)) (fun (i,l) -> revcons i l |> last = i)
  *)

  (** [(make n f)] is [[f 0; f 1; ...; f (n-1)]], evaluated left to right.  Tail-recursive.

      Example: a list of 10 random integers in the range 0-99:
      {[ make 10 (fun _ -> Random.int 100) ]}
  *)
  let make n f =
    let rec loop i acc =
      if i < n
      then loop ~++i (f i::acc)
      else rev acc
    in
    loop 0 []
  (*$Q make
    Q.unit (fun () -> make 0 id = [] && make 0 (k 1) = [])
    Q.neg_int (fun n -> make n id = [] && make n (k 1) = [])
    Q.small_int (fun n -> make n id |> len = n && make n (k 1) |> len = n)
    Q.small_int (fun n -> n = 0 || make n id = (1--n |> map pred))
    Q.(pair small_int small_int) (fun (n,m) -> make n (k m) |> for_all ((=) m))
  *)

  (** [(repeat n x)] is the list of exactly [n] [x]'s. *)
  let repeat n x = make n (k x)
  (*$= repeat
    [] (repeat 0 0)
  *)
  (*$Q repeat
    Q.small_int (fun n -> repeat n 0 |> len = n)
    Q.small_int (fun n -> repeat n 23 |> all ((=) 23))
  *)

  (** [trappend] is a tail-recursive {!List.append}. *)
  let trappend xs ys = rev_append (rev xs) ys
  (* TODO TESTS *)

  (** [(prepend xs ys)] prepends [xs] onto the front of [ys] and is another name for [List.append]. *)
  let prepend xs ys = xs @ ys
  (*$Q prepend
    Q.(pair (list small_int) (list small_int)) (fun (a,b) -> prepend a b = List.append a b)
  *)

  (** [(postpend xs ys)] postpends [xs] onto the end of [ys] and is the same as [(flip List.append)]. *)
  let postpend xs ys = ys @ xs
  (*$Q postpend
    Q.(pair (list small_int) (list small_int)) (fun (a,b) -> postpend a b = List.append b a)
  *)

  (** [(scanl f z list)] is similar to foldl, but returns a list of
      successive reduced values from the left:

      {[ scanl (±) z [x1; x2; ...] = [z; z ± x1; (z ± x1) ± x2; ...] ]}

      Invariant: [(last (scanl f z xs)) = (foldl f z xs)] *)
  let scanl f a list =
    let rec scanl acc f a list = scanl' (a::acc) f a list
    and     scanl' acc f a = function
    |  []   -> acc
    | x::xs -> scanl acc f (f a x) xs
    in
    scanl [] f a list |> rev
  (* TODO need scanr *)
  (*$= scanl
    [0] (scanl (+) 0 [])
    [0;1] (scanl (+) 0 [1])
    [0;1;3] (scanl (+) 0 [1;2])
  *)
  (*$Q scanl
    Q.int (fun i -> scanl (+) i [] = [i])
    Q.int (fun i -> scanl (-) i [] = [i])
    Q.(list small_int) (fun xs -> (last (scanl (+) 0 xs)) = (foldl (+) 0 xs))
  *)

  (** [(eachpair f list)] applies [f] to each consecutive pair of elements in [list].

      Example: [(eachpair f [1;2;3;4]) = (f 1 2 :: f 2 3 :: f 3 4 :: [])]

      Invariants:
      - ∀f.[(eachpair f []) = []]
      - ∀f.∀x.[(eachpair f [x]) = []]
      - ∀f.∀xs.[xs = [] || (len (eachpair f xs)) = (len xs - 1)]
  *)
  let eachpair f list =
    let rec loop acc = function
    | []       -> acc
    | x::[]    -> acc
    | x::y::zs -> loop (f x y :: acc) (y::zs)
    in
    loop [] list |> rev
  (*$Q eachpair
    Q.unit (fun () -> [] = eachpair (+) [])
    Q.int (fun x -> [] = eachpair (+) [x])
    Q.(list int) (fun xs -> xs = [] || (len (eachpair (+) xs)) = (len xs - 1))
  *)

  (** [(permutations xs)] is the list of all permutations of [xs]; {i
      not} tail-recursive.  {m O(n!)}.

      @author Michel Quercia *)
  let rec permutations lst =
    let rec insert x l = match l with
    | [] -> [[x]]
    | a::m -> (x::l) :: (map (fun y -> a::y) (insert x m))
    in
    match lst with
    | a::m -> flatten (map (insert a) (permutations m))
    | _ -> [lst]
  (*$= permutations
    [[]] (permutations [])
    [[1]] (permutations [1])
    [[1;2];[2;1]] (permutations [1;2] |> sort compare)
  *)
  (*$inject let rec fact x = if x = 0 then 1 else x * fact (x-1) *)
  (* stack overflow is possible above 6... *)
  (*$T permutations
    foldir (fun i r -> r && let ps = permutations (1--i) in len ps = fact i && foldr (fun x r -> r && len x = i) true ps  && foldr (fun xs r -> r && sort Int.compare xs = 1--i) true ps) true (1,6)
  *)

  (** [(upto m n)] is the list of ints in the interval [[m,n]] (tail-recursive).

      [(upto 1 5) = [1; 2; 3; 4; 5]].
  *)
  let upto = Pre.Lists.upto
  (* tests: see Pre.upto *)

  (** [(--)] is {!upto}.

      [(1--5) = [1; 2; 3; 4; 5]]
  *)
  let (--) = upto


  (** {1 Random Value} *)

  (** [(random ?size r ())] is a list of size [(size ())] (default: [< 100]);
      the elements are given by [(r ())]. *)
  let random ?size r () =
    let n = match size with None -> Random.int 100 | Some f -> f () in
    make n (fun _ -> r ())
  (* TODO TESTS *)

  (** {1:list_predicates Predicates} *)

  (** [(null xs)] is [true] iff [(xs = [])]. *)
  let null = function [] -> true | _ -> false
  (*$T null;empty
    let xs = [] in null xs && empty xs
    let xs = [1] in not (null xs) && not (empty xs)
    let xs = 1--10 in not (null xs) && not (empty xs)
  *)

  (** [empty] is [null]. *)
  let empty = null
  (* tests: see null above *)

  (** [(nonempty xs)] is [true] iff [(not (null xs))]. *)
  let nonempty = function [] -> false | _ -> true
  (*$T nonempty
    let xs = []    in      nonempty xs  = false
    let xs = [1]   in not (nonempty xs) = false
    let xs = 1--10 in not (nonempty xs) = false
  *)

  (** [(singleton xs)] is [true] iff [(len xs = 1)], {m O(1)}. *)
  let singleton = function _::[] -> true | _ -> false
  (*$T singleton
    singleton []      = false
    singleton [1]     = true
    singleton (1--10) = false
  *)

  (** [(many xs)] is [true] iff [(len xs > 1)], {m O(1)}. *)
  let many = function _::_::_ -> true | _ -> false
  (*$T many
    many []      = false
    many [1]     = false
    many (1--10) = true
  *)

  (** [(prefix pat subj)] is [true] if [pat] is a prefix of [subj] and
      [false] otherwise.

      Invariants:
      - ∀xs : [(prefix [] xs) = true]
      - ∀xs : [(prefix xs xs) = true]
      - ∀xs : [(xs = [] || prefix (init xs) xs) = true]
  *)
  let rec prefix pat subj = match pat, subj with
    | [],    _     -> true
    | _::_,     [] -> false
    | p::ps, s::ss -> if p = s then prefix ps ss else false
  (*$T prefix
    prefix [] []
    prefix (1--10) (1--11)
    prefix (1--10) (1--100)
    not (prefix (1--10) (2--10))
    not (prefix (1--10) (0--10))
  *)
  (*$Q prefix
    Q.(list small_int) (fun xs -> prefix [] xs)
    Q.(list small_int) (fun xs -> prefix xs xs)
    Q.(list small_int) (fun xs -> xs = [] || prefix (init xs) xs)
  *)


  (** {1:lists_reducing Reducing Lists (Folds)} *)

  (** [foldl] is [List.fold_left]. *)
  let foldl = List.fold_left
  (*$Q foldl
    Q.(list small_int) (fun xs -> foldl snoc [] xs = List.fold_left snoc [] xs)
  *)

  (** [(foldr f z xs)] is [(List.fold_right f xs z)].

      This order of parameters is much more useful in conjunction with
      the [(|>)]-operator.  *)
  let foldr f z xs = List.fold_right f xs z
  (*$Q foldr
    Q.(list small_int) (fun xs -> foldr cons [] xs = List.fold_right cons xs [])
  *)

  (** [foldl1] is {!foldl} with no initial accumulator value, and thus
      must be applied to non-empty lists.
      @raise Invalid_argument if the list is empty.
  *)
  let foldl1 f = function [] -> invalid_arg "foldl1" | x::xs -> foldl f x xs
  (*$T foldl1
    try foldl1 (+) [] |> ignore; false with Invalid_argument _ -> true
  *)
  (*$= foldl1
    1  (foldl1 (+) [1])
    10 (foldl1 max (1--10))
    1  (foldl1 min (1--10))
    55 (foldl1 (+) (1--10))
    1  (foldl1 (-) [1])
    ~-1 (foldl1 (-) [1;2])
    ~-4 (foldl1 (-) [1;2;3])
  *)

  (** [foldr1] is {!foldr} with no initial accumulator value, and thus
      must be applied to non-empty lists.
      @raise Invalid_argument if the list is empty.
  *)
  let foldr1 f = function [] -> invalid_arg "foldr1" | x::xs -> foldr f x xs
  (*$T foldr1
    try foldr1 (+) [] |> ignore; false with Invalid_argument _ -> true
  *)
  (*$= foldr1
    1  (foldr1 (+) [1])
    10 (foldr1 max (1--10))
    1  (foldr1 min (1--10))
    55 (foldr1 (+) (1--10))
    1  (foldr1 (-) [1])
    1  (foldr1 (-) [1;2])
    0  (foldr1 (-) [1;2;3])
  *)

  (** [foldl2] is [List.fold_left2]. *)
  let foldl2 = List.fold_left2
  (* the catch is because unequal length lists raise an exception *)
  (*$Q foldl2
    Q.(pair (list small_int) (list small_int)) (fun (xs,ys) -> catch (foldl2 (fun acc x y -> (x,y)::acc) [] xs) ys = catch (List.fold_left2 (fun acc x y -> (x,y)::acc) [] xs) ys)
  *)

  (** [foldr2] is [List.fold_right2]. *)
  let foldr2 = List.fold_right2
  (*$Q foldr2
    Q.(pair (list small_int) (list small_int)) (fun (xs,ys) -> catch (foldr2 (fun x y acc -> (x,y)::acc) xs ys) [] = catch (List.fold_right2 (fun x y acc -> (x,y)::acc) xs ys) [])
  *)

  (*$inject
    let genpair =
      QCheck.(
        make ~print:Print.(pair int (list int))
          Gen.(map succ small_nat >>= fun n -> pair (int_range 1 n) (list_size (return n) small_int))) *)

  (** [(foldwise ?skip n f init xs)] left-fold [f] across sequential
      {i non-overlapping} [n]-wise subsequences of [xs]; tail-recursive.

      For example, [(foldwise 2)] is a pairwise fold:

      {[ foldwise 2 (snocwith rev) [] (1--12) |> rev
= [[1; 2]; [3; 4]; [5; 6]; [7; 8]; [9; 10]; [11; 12]] ]}

      [f] is given an accumulator and a sublist of length [n].  For
      efficiency, the sublists passed to [f] will be in the reverse of
      the order in which they occur in [xs]; [f] can reverse them if
      necessary.

      Unless [skip] is [true], [f] must be prepared to receive a final
      sublist of length less than [n] if the length of [xs] is not
      evenly divisible by [n].

      If [(skip = true)], only the initial [(len xs - ((len xs) mod n))]
      elements of [xs] will be processed.

      Invariants:
      - [(foldwise n (snocwith rev) [] xs |> rev |> flatten) = xs]
      - [(foldwise n ~skip:false f init (take (len xs - len xs mod n) xs)) = (foldwise n ~skip:true f init xs)]

  *)
  let foldwise ?(skip=false) size f acc list =
    let each (i,g,acc) x =
      if i = size-1
      then 0, [], f acc (x::g)
      else i+1, (x::g), acc
    in
    match List.fold_left each (0,[],acc) list with
    | _,[],acc -> acc
    | _,g, acc -> if skip then acc else f acc g
  (*$Q foldwise
    genpair (fun (n,xs) -> foldwise n (snocwith rev) [] xs |> rev |> flatten = xs)
    genpair (fun (n,xs) -> (foldwise n ~skip:false snoc [] (take (len xs - len xs mod n) xs)) = (foldwise n ~skip:true snoc [] xs))
  *)

  (** {2 Fold Helpers}
      Functions useful with {!foldl} and {!foldr}. *)

  (** [(conswith f)] is [(cons <? f)] and also [(fun x xs -> f x :: xs)]. *)
  let conswith f x acc  = f x :: acc
  (*$= conswith
    [0] (conswith id 0 [])
    [1] (conswith succ 0 [])
  *)
  (*$Q conswith
    Q.(list small_int) (fun xs -> xs = [] || conswith id (hd xs) (tl xs) = ((fun x xs -> id x :: xs) (hd xs) (tl xs)))
    Q.(list small_int) (fun xs -> xs = [] || conswith succ (hd xs) (tl xs) = ((fun x xs -> succ x :: xs) (hd xs) (tl xs)))
  *)

  (** [(conswhen p x xs)] is [(cons x xs)] when [p x = true] and otherwise is [xs].

      {[ foldr (conswhen Int.even) [] (1--10) = [2; 4; 6; 8; 10] ]}
  *)
  let conswhen p x acc = if p x then x :: acc else acc
  (*$Q conswhen
    Q.(list small_int) (fun xs -> xs = [] || conswhen (k false) (hd xs) (tl xs) = (tl xs))
    Q.(list small_int) (fun xs -> xs = [] || conswhen (k true) (hd xs) (tl xs) = xs)
  *)

  (** [(snocwith f)] is [(flip (conswith f))].  Suitable for [foldl].

      [(fun f -> foldl (snocwith f) [] $ rev)] is a tail-recursive [map].
  *)
  let snocwith f acc x = conswith f x acc
  (*$= snocwith
    [0] (snocwith id [] 0)
    [1] (snocwith succ [] 0)
  *)
  (*$Q snocwith
    Q.(list small_int) (fun xs -> xs = [] || snocwith id (tl xs) (hd xs) = conswith id (hd xs) (tl xs))
    Q.(list small_int) (fun xs -> xs = [] || snocwith succ (tl xs) (hd xs) = conswith succ (hd xs) (tl xs))
  *)

  (** [(snocwhen p)] is [(flip (conswhen p))].  Suitable for [foldl]. *)
  let snocwhen p acc x = conswhen p x acc
  (*$Q snocwhen
    Q.(list small_int) (fun xs -> xs = [] || snocwhen (k false) (tl xs) (hd xs) = (tl xs))
    Q.(list small_int) (fun xs -> xs = [] || snocwhen (k true) (tl xs) (hd xs) = xs)
  *)

  (** {2 Special Folds} *)

  (** [concat] is [List.concat]. *)
  let concat = List.concat
  (*$inject let makexs n = let rand _ = Random.int 100 in make n rand *)
  (*$QR concat
    Q.(pair (int_bound 20) (int_bound 20))
    (fun (n,m) ->
     let xs = make n (fun _ -> makexs m) in
       concat xs = List.concat xs)
  *)

  (** [(anded bools)] is the conjunction of the Booleans in [bools]. *)
  let anded = foldl (&&) true
  (*$T anded
    anded []
    anded [true]
    anded [true;true]
    anded [true;true;true]
    not (anded [false])
    not (anded [false;true])
    not (anded [false;true;true])
    not (anded [true;false])
    not (anded [true;true;false])
    not (anded [true;false;true])
  *)

  (** [(ored bools)] is the disjunction of the Booleans in [bools]. *)
  let ored  = foldl (||) false
  (*$T ored
    not (ored [])
    (ored [true])
    (ored [true;true])
    (ored [true;true;true])
    (ored [false;true])
    (ored [false;true;true])
    (ored [true;false])
    (ored [true;true;false])
    (ored [true;false;true])
    not (ored [false])
    not (ored [false;false])
    not (ored [false;false;false])
  *)

  (** [(conjunction ps x)] is the conjunction of the predicates in [ps]. *)
  let conjunction ps x = foldl (fun r p -> r && p x) true  ps
  (*$T conjunction
    conjunction [] ()
    conjunction [k true] ()
    conjunction [k true;k true] ()
    conjunction [k true;k true;k true] ()
    not (conjunction [k false] ())
    not (conjunction [k false] ())
    not (conjunction [k true; k false] ())
    not (conjunction [k false; k true] ())
    not (conjunction [k false; k true; k true] ())
    not (conjunction [k false; k true; k true] ())
  *)

  (** [(disjunction ps x)] is the disjunction of the predicates in [ps]. *)
  let disjunction ps x = foldl (fun r p -> r || p x) false ps
  (*$T disjunction
    not (disjunction [] ())
    not (disjunction [k false] ())
    not (disjunction [k false;k false] ())
    not (disjunction [k false;k false;k false] ())
    disjunction [k false;k false;k false;k true] ()
    disjunction [k false;k false;k true;k false] ()
    disjunction [k true] ()
    disjunction [k true;k true] ()
    disjunction [k true;k true;k true] ()
  *)

  (* let (|||) f g = disjunction [f;g] (\* TODO OPTIMIZE *\) *)

  (* let (&&&) f g = conjunction [f;g] (\* TODO RENAME & OPTIMIZE conflicts with Pair.(&&&) *\) *)

  (** [(all p xs)] is [true] if [(p x)] for all of the [xs].

      Invariant:
      - [(all p xs) = (map p xs |> anded)]
  *)
  let all p = foldl (fun t x -> t && p x) true
  (*$T all
    all (k true) []
    all (k false) []
    all even []
    all even [2;4;6;100]
    not (all even [1;2;4;6;100])
    not (all even [2;1;4;6;100])
    not (all even [2;4;6;100;1])
  *)
  (*$Q all
    Q.(list small_int) (fun xs -> all (k true) xs = (map (k true) xs |> anded))
    Q.(list small_int) (fun xs -> all (k false) xs = (map (k false) xs |> anded))
  *)

  (** [(any p xs)] is [true] if [(p x)] for any of the [xs].

      Invariant:
      - ∀p : ∀xs : [(any p xs) = (map p xs |> ored)]
  *)
  let any p = foldl (fun t x -> t || p x) false
  (*$T any
    any even [2;4;6;100]
    any even [6;1;3;5;7]
    any even [1;3;6;5;7]
    any even [1;3;5;7;6]
    any even [2;4;6;100;1]
    any even [2;1;4;6;100]
    any even [1;2;4;6;100]
    not (any even [1;3;5;7])
    not (any (k true) [])
    not (any even [])
    not (any (k false) [])
  *)
  (*$Q any
    Q.(list small_int) (fun xs -> any (k true) xs = (map (k true) xs |> ored))
    Q.(list small_int) (fun xs -> any (k false) xs = (map (k false) xs |> ored))
  *)

  (** [(sum xs)] is the sum of the ints in [xs]. *)
  let sum = foldl (+) 0
  (*$= sum
    0 (sum [])
    654 (sum [654])
  *)
  (*$Q sum
    Q.small_int (fun n -> n < 1 || sum (1--n) = n * (n+1) / 2)
  *)

  (** [(product xs)] is the product of the ints in [xs]. *)
  let product = foldl ( * ) 1
  (*$= product
    1 (product [])
    654 (product [654])
  *)
  (*$Q product
    Q.small_int (fun n -> n < 1 || product (1--n) = fact n)
  *)

  (** [(maximumBy compare xs)] is the maximum of the [xs] according to [compare].

      Examples:
      - {[ maximumBy compare (1--10) = 10 ]}
      - {[ maximumBy compare ["zap";"foo"] = "zap" ]}
      - {[ maximumBy compare ["ZAP";"foo"] = "foo" ]}
      - {[ maximumBy AIString.compare ["ZAP";"foo"] = "ZAP" ]}
      - {[ maximumBy AIString.compare ["zap";"foo"] = "zap" ]}

      @raise Invalid_argument if the list is empty.
  *)
  let maximumBy ?compare xs = match compare with
    | None         -> reraise (Invalid_argument "maximum") (foldl1 max) xs
    | Some compare -> reraise (Invalid_argument "maximum")
                        (foldl1 (fun acc x -> if compare acc x = 1 then acc else x)) xs
  (* TODO TESTS *)

  (** [maximum] is [(maximumBy compare)].

      Examples:
      - {[ maximum (1--10) = 10 ]}
      - {[ maximum ["zap";"foo"] = "zap" ]}
      - {[ maximum  ["ZAP";"foo"] = "foo" ]}

      @raise Invalid_argument if the list is empty.
  *)
  let maximum xs = foldl1 (fun acc x -> if acc > x then acc else x) xs
  (*$Q maximum
    Q.small_int (fun n -> n < 1 || maximum (map ((-) 0) (1--n) @ (1--n)) = n)
  *)

  (** [(minimum ?compare xs)] is the minimum of the [xs].  If provided,
      [compare] is used for comparison (default: polymorphic minimum
      via [min]).

      Examples:
      - {[ minimum (1--10) = 1 ]}
      - {[ minimum ["zap";"foo"] = "foo" ]}
      - {[ minimum  ["ZAP";"foo"] = "ZAP" ]}
      - {[ minimum ~compare:AIString.compare ["ZAP";"foo"] = "foo" ]}
      - {[ minimum ~compare:AIString.compare ["zap";"foo"] = "foo" ]}

      @raise Invalid_argument if the list is empty.
  *)
  let minimum ?compare xs = match compare with
    | None     -> reraise (Invalid_argument "minimium") (foldl1 min) xs
    | Some compare -> reraise (Invalid_argument "minimium")
                        (foldl1 (fun acc x -> if compare acc x = -1 then acc else x)) xs
  (*$Q minimum
    Q.small_int (fun n -> n < 1 || minimum (map ((-) 0) (1--n) @ (1--n)) = ~-n)
  *)

  (** [(break eq list)] does control-break processing on the (typically
      sorted) list, grouping together consecutive subsequences of elements which
      are equal under [eq].

      {i N.B.} For the sake of efficiency, the result is in reverse
      order, as are all the sublists.  [(List.rev_map rev)] will
      restore order.

      Example: this function:
      - [(break (fun p x -> p.[0] = x.[0]))]

      will group a sorted list of words into lists of words that begin
      with the same letter.

      {[ ["copulatives"; "cumulative"; "deliverable"; "denial"; "drowning";
 "folklorists"; "gradualism"; "lowliest"; "orangeade"; "oxen"]
 |> break (fun p x -> p#!0 = x#!0) |> List.rev_map rev
 = [["copulatives"; "cumulative"]; ["deliverable"; "denial"; "drowning"];
 ["folklorists"]; ["gradualism"]; ["lowliest"]; ["orangeade"; "oxen"]] ]}

  *)
  let break eq list =
    let each (prev,xs,acc) x = match prev with
      | None                       -> Some x, x :: xs,     acc
      | Some p as prev when eq p x -> prev,   x :: xs,     acc
      | Some _     (* otherwise *) -> Some x, x :: [], xs::acc
    in
    match foldl each (None,[],[]) list with _, [], acc -> acc | _, xs, acc -> xs :: acc
  (*$Q break
    Q.(list small_printable_string) (fun xs -> filter (not $. String.null) xs |> sort compare |> break (fun p x -> p.[0] = x.[0]) |> List.rev_map rev |> fun xs -> sorted compare xs && all (sorted compare) xs)
  *)

  (** {1:lists_transformations List Transformations} *)

  (** [rev] is [List.rev]. *)
  let rev = List.rev
  (*$Q rev
    Q.(list small_int) (fun xs -> rev xs = List.rev xs)
  *)

  (** [map] is [List.map]. *)
  let map = List.map
  (*$Q map
    Q.(list small_int) (fun xs -> map id xs = List.map id xs)
    Q.(list small_int) (fun xs -> map (k 0) xs = List.map (k 0) xs)
    Q.(list small_int) (fun xs -> map succ xs = List.map succ xs)
  *)

  (** [trmap] is a tail-recursive map. *)
  let trmap f xs = foldl (snocwith f) [] xs |> rev
  (*$Q trmap
    Q.(list small_int) (fun xs -> trmap id xs = List.map id xs)
    Q.(list small_int) (fun xs -> trmap (k 0) xs = List.map (k 0) xs)
    Q.(list small_int) (fun xs -> trmap succ xs = List.map succ xs)
  *)

  (** [mapi] is [List.mapi]. *)
  let mapi = List.mapi
  (*$Q mapi
    Q.(list small_int) (fun xs -> mapi (fun _ x -> x) xs = List.mapi (fun _ x -> x) xs)
    Q.(list small_int) (fun xs -> mapi (fun x _ -> x) xs = List.mapi (fun x _ -> x) xs)
    Q.(list small_int) (fun xs -> mapi (fun _ x -> succ x) xs = List.mapi (fun _ x -> succ x) xs)
    Q.(list small_int) (fun xs -> mapi (fun x _ -> succ x) xs = List.mapi (fun x _ -> succ x) xs)
  *)

  (** [map2] is [List.map2]. *)
  let map2 = List.map2
  (*$Q map2
    Q.(pair (list small_int) (list small_int)) (fun (xs,ys) -> len xs <> len ys || map2 Pair.up xs ys = List.map2 Pair.up xs ys)
  *)

  (** [(flatmap f)] is a tail-recursive [(map f $ concat)].

      @author Matt Teichman *)
  let flatmap f xs =
    let[@tail_mod_cons] rec flatmap k = function
      | [] -> []
      | x :: xs -> append_flatmap k (k x) xs
    and[@tail_mod_cons] append_flatmap k kx xs =
      match kx with
      | [] -> flatmap k xs
      | y :: ys -> y :: append_flatmap k ys xs
    in
    flatmap f xs
  (*$Q flatmap
    Q.small_int (fun n -> let xs = makexs n in flatmap consup xs = xs)
    Q.small_int (fun n -> let xs = makexs n in flatmap (fun x -> 1--x) xs = (map (fun x -> 1--x) $ concat) xs)
  *)

  (** [filter] is [List.filter]. *)
  let filter = List.filter
  (*$Q filter
    Q.(list small_int) (fun xs -> filter (k true) xs = List.filter (k true) xs)
    Q.(list small_int) (fun xs -> filter (k false) xs = List.filter (k false) xs)
    Q.(list small_int) (fun xs -> filter even xs = List.filter even xs)
  *)

  (** [(delete ?eq x xs)] removes the first occurrence of [x] from [xs]; tail recursive. *)
  let delete ?eq x xs =
    let eq = match eq with Some f -> f | None -> (=) in
    let rec loop acc = function
      | []                    -> rev acc
      | y::ys when eq y x     -> rev_append acc ys
      | y::ys (* otherwise *) -> loop (y::acc) ys
    in
    loop [] xs
  (*$Q delete
    Q.int (fun n -> delete n [] = [])
    Q.int (fun n -> delete n [n] = [])
    Q.(pair int (list int)) (fun (n,xs) -> delete n xs = takewhile (not << (=) n) xs @ (dropwhile (not << (=) n) xs |> drop 1))
 *)

  (** [(replace sub rep xs)] replaces each occurrence of [sub] in [xs] with [rep].

      Examples:
      - [(replace 1 10 (1--6))] = [[10; 2; 3; 4; 5; 6]]
      - [(replace 10 20 (1--6))] = [[1; 2; 3; 4; 5; 6]]
      - [((1--6) |> map (flip (mod) 2) |> replace 0 2)] = [[1; 2; 1; 2; 1; 2]]
  *)
  let replace sub rep xs =
    let rec loop acc = function
    | []                 -> rev acc
    | x::xs when x = sub -> loop (rep::acc) xs
    | x::xs (* else *)   -> loop (x  ::acc) xs
    in
    loop [] xs
  (*$= replace
    (10 :: 2--20) (replace 1 10 (1--20))
    (1--19 @ [100]) (replace 20 100 (1--20))
    [1;2;3;10;5;6] (replace 4 10 [1;2;3;4;5;6])
  *)

  (** [(behead prefix list)] is the remainder of [list] when the leading [prefix is removed].

      [prefix] must be an exact prefix of [list] for anything to be removed.

      Invariants:
      - [(behead [] xs) = xs]
      - [(behead xs []) = []]
      - [(behead xs xs) = []]
      - [(behead (x::xs) xs) = xs]
      - [(len (behead xs ys)) <= (len ys)]
      - [(prefix xs ys) && (behead xs ys) <> ys]
   *)
  let behead pre list =
    let rec loop pre list = match pre, list with
      | p::ps, x::xs when p=x -> loop ps xs
      | p::ps,    xs (*else*) -> xs
      |     _,    xs          -> xs
    in
    loop pre list
  (*$Q behead
    Q.(list int) (fun xs -> behead [] xs = xs)
    Q.(list int) (fun xs -> behead xs [] = [])
    Q.(list int) (fun xs -> behead xs xs = [])
    Q.(list int) (fun xs -> prefixes xs |> tl |> all (flip behead xs >> (<>) xs))
    Q.(pair int (list int)) (fun (x,xs) -> behead (x::xs) xs = xs)
    Q.(pair (list int) (list int)) (fun (xs,ys) -> len (behead xs ys) <= len ys)
   *)

  (** [(prefixes xs)] is the list of all prefixes of xs.

      {[ (prefixes [1;2;3]) = [[]; [1]; [1; 2]; [1; 2; 3]] ]}

      Invariants:
      - [(prefixes xs) = (scanl snoc [] xs |> map rev)]
      - [(prefixes xs |> map (flip prefix xs) |> anded) = true]
      - [(prefixes xs |> hd) = []]
  *)
  let prefixes xs =
    let each acc x = match acc with
    | []    -> [x] :: acc
    | y::ys -> ([x] @ y) :: acc
    in
    foldl each [] xs |> map rev |> rev |> cons []
  (*$Q prefixes
    Q.(int_bound 10) (fun n -> let xs = makexs n in (prefixes xs) = (scanl snoc [] xs |> map rev))
    Q.(int_bound 10) (fun n -> let xs = makexs n in prefixes xs |> map (flip prefix xs) |> anded)
    Q.(int_bound 10) (fun n -> let xs = makexs n in (prefixes xs |> hd) = [])
  *)

  (** [(suffixes xs)] is the list of all suffixes of xs.

      {[ (suffixes [1;2;3]) = [[]; [3]; [2; 3]; [1; 2; 3]]  ]}

      Invariant: [(suffixes xs |> hd) = []]
  *)
  let suffixes xs =
    let each x acc = match acc with
    | []    -> [x] :: acc
    | y::ys -> ([x] @ y) :: acc
    in
    foldr each [] xs |> rev |> cons []
  (*$= suffixes
    [[]] (suffixes [])
    [[]; [3]; [2; 3]; [1; 2; 3]] (suffixes [1;2;3])
  *)
  (*$Q suffixes
    Q.(list small_int) (fun xs -> (suffixes xs |> hd) = [])
  *)

  (** [(intersperse x xs)] inserts [x] between the elements of [xs].

      Example:
      [String.(explode "123456" |> intersperse ',' |> implode) = "1,2,3,4,5,6"]

      Invariants:
      - [(intersperse x []) = []]
      - [(intersperse x [y]) = [y]]
  *)
  let intersperse x = function
  | []    -> []
  | y::[] -> y::[]
  | y::ys -> foldl (fun acc z -> z::x::acc) [y] ys |> rev
  (*$= intersperse
    [] (intersperse 100 [])
    "1,2,3,4,5,6" (String.explode "123456" |> intersperse ',' |> String.implode)
  *)
  (*$Q intersperse
    Q.int (fun n -> (intersperse 100 [n]) = [n])
    Q.small_int (fun n -> intersperse 0 (1--n) |> evens = (1--n))
  *)

  (** [(pad ?(left=false) ~def n xs)] pads [xs] on the right (or on
      the left, if [(left = true)]) with enough instances of [def]
      such that the length of the result is {i at least} [n].

      If you need the result list to be {i exactly} of length [n] (never longer), use:
      - [(take n $ pad ~def n)]

      Invariant: [(len (pad ~left ~def n xs) = max n (len xs))]
   *)
  let pad ?(left=false) ~def n xs =
    if len xs < n
    then if left
         then append (repeat (n - len xs) def) xs
         else repeat (n - len xs) def |> append xs
    else xs
  (*$Q
     Q.(pair int small_int)              (fun (def,n)    -> len (pad             ~def n []) = max 0 n)
     Q.(triple int small_nat (list int)) (fun (def,n,xs) -> len (pad             ~def n xs) = max (len xs) n)
     Q.(triple int small_nat (list int)) (fun (def,n,xs) -> len (pad ~left:false ~def n xs) = max (len xs) n)
     Q.(triple int small_nat (list int)) (fun (def,n,xs) -> len (pad ~left:true  ~def n xs) = max (len xs) n)
     Q.(pair int (list int)) (fun (def,xs) -> pad ~def 0 xs = xs)
     Q.small_nat (fun n -> let xs = pad            ~def:0 (n*2) (repeat n 1) in drop n xs = repeat n 0 && take n xs = repeat n 1)
     Q.small_nat (fun n -> let xs = pad ~left:true ~def:0 (n*2) (repeat n 1) in take n xs = repeat n 0 && drop n xs = repeat n 1)
   *)

  (** [(transpose ?def xss)] is the transposition of the rows and columns of
      [xss]; tail recursive.

      Unless [def] is provided, if any sublist is shorter than the
      first sublist, [(Invalid_argument _)] is raised.  Otherwise, the
      resulting sublists will all be of length [(len (nth xss 0))].

      If [def] is provided, it serves as a default element that is
      appended as needed to bring all lists to the length of the
      longest list in [xss]; in this case, the "matrix" is guaranteed
      to be rectangular and no exception will be raised.

      Examples:
      - [transpose [1--3;4--6] = [[1; 4]; [2; 5]; [3; 6]]]
      - [transpose [[1];[1;2]] = [[1; 1]]]
      - [catch transpose [[1;2];[1]] = None]
      - [transpose ~def:0 [[1;2];[1]] = [[1; 1]; [2; 0]]]
   *)
  let transpose ?def xss =
    let hd xs = match hd xs with
      | r -> r
      | exception _ -> match def with
                       | Some d -> d
                       | None -> invalid_arg "transpose" in
    let rec transpose' acc = function
      | []    -> acc
      | []::_ -> acc
      | m     -> transpose' ((map hd m)::acc) ((map (default [] tl)) m)
    in
    match def, xss with
    | None,     _  -> rev (transpose' [] xss)
    | Some _,   [] -> []
    | Some def, _  -> map (pad ~def (foldl (fun m xs -> len xs |> max m) 0 xss)) xss |> transpose' [] |> rev
  (* TODO TESTS ~def *)
  (*$Q transpose
    Q.unit (fun () -> transpose [] = [])
    Q.(pair (int_range 1 10) int) (fun (n,x) -> make n (k [x]) |> transpose |> hd |> len = n)
    Q.(list_of_size Gen.small_nat (list_of_size Gen.(return 10) small_nat)) (fun xs -> xs = transpose (transpose xs))
   *)

  (** [(evens xs)] is all the elements of [xs] at even indexes.

      - [ evens [1; 3; 5] = [1; 5] ]
      - [ evens ["a"; "b"; "c"] = ["a"; "c"] ]
  *)
  let rec evens = function
    |  []   -> []
    | x::xs -> x :: odds xs
    and odds = function
      |  []   -> []
      | x::xs -> evens xs
  (** [(odds xs)] is all the elements of [xs] at odd indexes.

    - [ odds [0; 2; 4] = [2] ]
    - [ odds ["a"; "b"; "c"] = ["b"] ]
  *)
  (*$= evens;odds
    (evens []) (odds [])
    (evens (1--10) |> len) (odds (1--10) |> len)
  *)
  (*$Q evens;odds
    Q.small_int (fun n -> odd n || map2 (fun x y -> x::[y]) (evens (1--n)) (odds (1--n)) |> concat = 1--n)
  *)

  (** {1:lists_sublists Sublists} *)

  (** [(splitat n xs)] is [(a,b)] such that [a] is the length-[n]
      prefix of [xs], and [b] is the remainder of the [xs].

      It is equivalent to [(take n xs, drop n xs)], but makes only one
      pass.

      Invariant:
      - [let a,b = splitat n xs in prefix a xs && (a @ b) = xs]

      Examples:
      - [splitat 3   [1;2;3;4;5] = [1;2;3], [4;5]]
      - [splitat 1   [1;2;3]     = [1],     [2;3]]
      - [splitat 3   [1;2;3]     = [1;2;3], []]
      - [splitat 4   [1;2;3]     = [1;2;3], []]
      - [splitat 0   [1;2;3]     = [],      [1;2;3]]
      - [splitat ~-1 [1;2;3]     = [],      [1;2;3]]

  *)
  let splitat n list =
    let rec loop acc n = function
      | x::xs when n > 0    -> loop (x::acc) (n-1) xs
      | list (* otherwise*) -> rev acc, list
    in
    loop [] n list
  (*$QR splitat
    Q.(list small_int)
    (fun xs -> xs = [] ||
     let n = Random.int (len xs) in
     let a,b = splitat n xs in prefix a xs && prefix (rev b) (rev xs) && (a @ b) = xs)
  *)
  (*$QR splitat;take;drop
    Q.(list small_int)
    (fun xs -> xs = [] ||
     let n = Random.int (len xs) in
     splitat n xs = (take n xs, drop n xs))
  *)

  (** [(everyother xs)] is the list consisting of every other element
     of [xs], starting with the first; {m O(n)}, 1-pass algorithm.

     Invariants:
     - [(len (everyother xs) = if even (len xs) then len xs / 2 else len xs / 2 + 1)]
     - [(hd (everyother xs) = hd xs)]
   *)
  let rec everyother = function _::[] as xs -> xs | x::_::xs -> x::everyother xs | _ -> []
  (*$Q everyother
    Q.unit (fun () -> everyother [] = [])
    Q.unit (fun () -> everyother [1] = [1])
    Q.unit (fun () -> everyother [1;2] = [1])
    Q.unit (fun () -> everyother [1;2;3] = [1;3])
    Q.(list int) (fun xs -> map2 (fun x y -> x::[y]) (take (len xs / 2) (everyother (0::xs))) (take (len xs / 2) (revcons 0 (everyother xs))) |> concat |> drop 1 |> flip prefix xs)
    Q.(list int) (fun xs -> len (everyother xs) = if even (len xs) then len xs / 2 else len xs / 2 + 1)
    Q.(list int) (fun xs -> catch hd (everyother xs) = catch hd xs)
   *)

  (** [(take n xs)] is [(splitat n >> fst)].

      Invariant: [(take n xs) = (drop (len xs - n) (rev xs) |> rev)]
  *)
  let take n xs = splitat n xs |> fst

  (** [(drop n xs)] is [(splitat n >> snd)].

      Invariant: [(drop n xs) = (take (len xs - n) (rev xs) |> rev)]
  *)
  let drop n xs = splitat n xs |> snd
  (*$QR take;drop
    Q.(list small_int)
    (fun xs ->
     xs = [] ||
     let n = Random.int (len xs) in
     (take n xs) = (drop (len xs - n) (rev xs) |> rev)
     && (drop n xs) = (take (len xs - n) (rev xs) |> rev))
  *)

  (** [(takeall n list)] returns the sequential sublists of length [n] in [list].

      If [n] <= 0, the empty list is returned.

      Invariant:
      - ∀n . n>0 : [(takeall n xs |> concat) = xs]

      Example:
      - [(takeall 3 (1--10)) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]]]
  *)
  let takeall n list =
    let rec loop acc = function
    | []   -> rev acc
    | xs   -> match splitat n xs with
    | a,[] -> rev (a::acc)
    | a,b  -> loop (a :: acc) b
    in
    if n > 0 then loop [] list else []
  (*$= takeall
    5 (takeall 2 (1--10) |> len)
   *)
  (*$Q takeall
    Q.small_int (fun n -> n <= 0 || let xs = makexs 10 in takeall n xs |> concat = xs)
    Q.small_int (fun n -> odd n || n < 0 || n < 2 || takeall 2 (1--n) |> len = n / 2)
    Q.small_int (fun n -> odd n || n < 0 || n < 10 || takeall 10 (1--n) |> len = n / 10 + (compare (n mod 10) 0))
   *)

  (** [(splitwhile p xs)] is [(a,b)] where [a] is the longest prefix
      of [xs] of elements that satisfy [p] and [b] is the remaining suffix..

      Invariant:
      - [splitwhile p xs |> fun (a,b) -> a@b = xs]
  *)
  let splitwhile p xs =
    let rec loop acc = function
    |    []                 -> rev acc, []
    | x::xs when p x        -> loop (x::acc) xs
    |    xs (* otherwise *) -> rev acc, xs
    in
    loop [] xs
  (*$Q splitwhile
    Q.small_int (fun n -> let xs = makexs n in map (fun p -> splitwhile p xs |> uncurry (@) = xs) [k true; k false; even; odd] |> all id)
   *)

  (** [(takewhile p)] is [(splitwhile p >> fst)]

      Invariant:
      - [(takewhile p xs @ dropwhile p xs) = xs]
  *)
  let takewhile p = splitwhile p >> fst
  (* test: see dropwhile *)

  (** [(dropwhile p xs)] is [(splitwhile p >> snd)].

      Invariant:
      - ∀xs : ∀p : [(takewhile p xs @ dropwhile p xs) = xs]
  *)
  let dropwhile p = splitwhile p $ snd
  (*$Q takewhile;dropwhile
    Q.small_int (fun n -> let xs = makexs n in map (fun p -> (takewhile p xs @ dropwhile p xs) = xs) [k true; k false; even; odd] |> all id)
   *)

  (** {1:lists_searching Searching Lists} *)

  (** [mem] is [List.mem]. *)
  let mem = mem

  (** [one_of] is [(flip List.mem)]. *)
  let one_of xs x = mem x xs

  (** [partition] is [Lists.partition]. *)
  let partition = partition

  (** {1:lists_zipping Zipping and Unzipping Lists} *)
  (* TODO do these belong in Assoc (and then exported)? *)

  (** [(zip xs ys)] takes two lists and returns a list of
      corresponding pairs; tail-recursive.

      This is the same as [List.combine], except that if one input
      list is short, excess elements of the longer list are discarded,
      rather than raising an exception.

      Examples:
      - [ (zip (1--4) (5--8)) = [(1, 5); (2, 6); (3, 7); (4, 8)] ]
      - [ (zip (1--2) (5--8)) = [(1, 5); (2, 6)] ]
  *)
  let zip xs ys =
    let rec loop acc xs ys = match xs, ys with
    | x::xs, y::ys -> loop ((x,y)::acc) xs ys
    | _,     _     -> rev acc
    in
    loop [] xs ys
  (*$QR zip;
    Q.(list (pair small_int small_int))
    (fun xs ->
     Assoc.(zip (keys xs) (values xs)) = xs)
  *)

  (** [(zipwith f xs ys)] is [zip xs ys |> map (uncurry f)]. *)
  let zipwith f xs ys = zip xs ys |> map (uncurry f)
  (* TODO TESTS *)

  (** [(unzip xs)] is [List.split]: it transforms a list of pairs into
      a list of first components and a list of second components.

      Invariant:
      - [(zip xs ys |> unzip) = (xs,ys)] iff [(len xs = len ys)]
  *)
  let unzip = List.split
  (*$QR zip;unzip
    Q.(list (pair small_int small_int))
    (fun xs ->
     xs = (Assoc.(zip (keys xs) (values xs)) |> unzip |> uncurry zip))
  *)

  (** {1:lists_association Association Lists} *)

  (** Functions for working with association lists (alists). *)
  module Assoc = struct (*$< Assoc *)
    (** {1 Constants} *)

    (** [empty] is the empty alist. *)
    let empty = []
    (*$= empty
      [] empty
    *)

    (** {1 Creation} *)

    (** [(random ?size ~key ~value ())] is a random alist of size
        [(size ())] (default: [< 100]), and each binding is given by [(key (), value ())]. *)
    let random ?size ~key ~value () =
      let n = match size with None -> Random.int 100 | Some f -> f () in
      random ~size:(k n) value () |> zip (random ~size:(k n) key ())
    (* TODO TESTS *)

    (** {1 Accessors} *)

    (** [assoc] is {!assoc}. *)
    let assoc = assoc

    (** [find] is {!assoc}. *)
    let find = assoc
    (*$QR find
      Q.(pair small_int (list (pair small_int small_int)))
      (fun (x,xs) ->
       catch (find x) xs = catch (assoc x) xs)
    *)

    (** [(bindings k alist)] is the list of all bindings of key [k] in [alist]; tail recursive.

        The bindings are returned in the order in which they occur.

        Example:
        - [(bindings 2 [1,1;2,2;3,3;2,4]) = [2,2; 2,4]]

        Invariants:
        - [(bindings k [] = [])]
        - [(assocs k alist |> map (cons k)) = (bindings k alist)]
        - [(len (assocs k) = len (bindings k))]
    *)
    (*$Q bindings
      Q.int (fun k -> bindings k [] = [])
      Q.(small_list (pair int int)) (fun xs -> xs = [] || let k,v = hd xs in bindings k xs |> hd = (k,v))
      Q.(small_list (pair int int)) (fun xs -> match xs with [] -> true | (k,_)::_ -> (assocs k xs |> map (Pair.up k)) = bindings k xs)
      Q.(small_list (pair int int)) (fun xs -> match xs with [] -> true | (k,_)::_ -> (assocs k xs |> map (Pair.up k)) = bindings k xs)
      Q.(small_list (pair small_int small_int)) (fun xs -> xs = [] || let k = choose 1 xs |> hd |> fst in len (assocs k xs) = len (bindings k xs))
    *)
    let bindings k alist =
      foldl (fun bs ((x,y) as b) -> if k=x then b::bs else bs) [] alist |> rev

    (** [(assocs k alist)] is the list of all the values in [alist]
        associated with key [k]; tail-recursive.

        The values are in the order in which they occur in [alist].

        Example:
        - [(assocs 2 [1,1;2,2;3,3;2,4]) = [2; 4]]

        See also {!coalesce} for an efficient way to do [assocs] for
        each key in an alist. *)
    let assocs x = foldl (fun vs (k,v) -> if k=x then v::vs else vs) [] $ rev
    (*$QR assocs
      Q.(pair small_int (list (pair small_int small_int)))
      (fun (x,xs) ->
       let each ys (k,v) = if x=k then v :: ys else ys in
       assocs x xs = rev (foldl each [] xs)
      )
    *)

    (** [(keys alist)] is the list of all the keys in [alist], order preserved; tail-recursive. *)
    let keys alist = foldl (snocwith fst) [] alist |> rev
    (*$Q keys
      QCheck.(make Gen.(small_nat >>= fun n -> pair (list_size (return n) int) (list_size (return n) int))) (fun (xs,ys) -> zip xs ys |> keys = xs)
    *)

    (** [(values alist)] is the list of all the values in [alist], order preserved; tail-recursive. *)
    let values alist = foldl (snocwith snd) [] alist |> rev
    (*$Q values
      QCheck.(make Gen.(small_nat >>= fun n -> pair (list_size (return n) int) (list_size (return n) int))) (fun (xs,ys) -> zip xs ys |> values = ys)
    *)

    (** [(project keys alist)] is the subset of [alist] that contains only pairs with keys in [keys]. *)
    let project fields alist =
      let each acc ((k,v) as pair) = if mem k fields then pair :: acc else acc in
      foldl each [] alist |> rev
    (*$Q project
      Q.(list (pair int int)) (fun alist -> project [] alist = [])
      Q.(list (pair int int)) (fun alist -> len alist < 2 || project [choose 1 alist |> hd |> fst] alist |> keys |> nub <> nub (keys alist))
      Q.(list (pair int int)) (fun alist -> sort compare (project (keys alist) alist) = sort compare alist)
     *)

    (** {1 Predicates} *)

    (** [mem] is {!mem_assoc}. *)
    let mem = mem_assoc
    (*$QR mem
      Q.(pair small_int (list (pair small_int small_int)))
      (fun (x,xs) ->
       catch (mem x) xs = catch (mem_assoc x) xs)
    *)

    (** [(has ?kf vf xs)] is [true] if [(kf k && vf v)] for any [(k,v)] pair in alist [xs] and [false] otherwise.

        The default value for [kf] is [(k true)].
     *)
    let has ?kf vf xs =
      let rec loop = function
        | []        -> false
        | (k,v)::xs -> if Option.call true kf k && vf v then true else loop xs
      in
      loop xs
    (*$Q has
      Q.(pair pos_int (list (pair pos_int pos_int))) (fun (x,xs) -> has ((=) ~-x) xs |> not)
      Q.(pair pos_int (list (pair pos_int pos_int))) (fun (x,xs) -> has ~kf:((=) ~-x) (k true) xs |> not)
      Q.(pair pos_int (list (pair pos_int pos_int))) (fun (x,xs) -> xs = [] || has ((=) (hd xs |> snd)) xs)
      Q.(pair pos_int (list (pair pos_int pos_int))) (fun (x,xs) -> xs = [] || has ((=) (last xs |> snd)) xs)
      Q.(pair pos_int (list (pair pos_int pos_int))) (fun (x,xs) -> xs = [] || has ((=) (choose 1 xs |> hd |> snd)) xs)
      Q.(pair pos_int (list (pair pos_int pos_int))) (fun (x,xs) -> xs = [] || has ~kf:((=) (hd xs |> fst)) (k true) xs)
      Q.(pair pos_int (list (pair pos_int pos_int))) (fun (x,xs) -> xs = [] || has ~kf:((=) (last xs |> fst)) (k true) xs)
      Q.(pair pos_int (list (pair pos_int pos_int))) (fun (x,xs) -> xs = [] || has ~kf:((=) (choose 1 xs |> hd |> fst)) (k true) xs)
      Q.(pair pos_int (list (pair pos_int pos_int))) (fun (x,xs) -> xs = [] || has ~kf:(k false) ((=) (hd xs |> snd)) xs |> not)
      Q.(pair small_int (list (pair small_int small_int))) (fun (x,xs) -> has ((=) x) xs = succeeds (List.find (fun (_,v) -> v=x)) xs)
    *)

    (** {1 Modifiers} *)

    (** [(add k v alist)] is [((k,v) :: alist)]. *)
    let add k v alist = (k,v) :: alist
    (*$= add
      [1,2] (add 1 2 [])
    *)
    (*$QR add
      Q.(pair (list (pair small_int small_int)) (pair small_int small_int))
      (fun (xs,(k,v)) ->
       (add k v xs |> find k) = v)
    *)

    (** [remove_assoc] is [List.remove_assoc]. *)
    let remove_assoc = remove_assoc
    (*$QR remove_assoc
      Q.(pair small_int (list (pair small_int small_int)))
      (fun (x,xs) ->
       catch (remove_assoc x) xs = catch (List.remove_assoc x) xs)
    *)

    (** [(remove k alist)] is like [remove_assoc] except: 1. it
        removes {i ALL} matching pairs (not just the first) and
        2. [remove] is tail-recursive.*)
    let remove k alist = filter (fun (k',_) -> k' <> k) alist
    (*$T remove
      let a,b = (1--10), (2--10) in zip a a @ zip a a |> remove (nth a 0) = zip b b @ zip b b
    *)
    (*$QR remove
      Q.(list (pair small_int small_int))
      (fun xs ->
       xs = [] ||
       let x = fst @@ hd xs in
       remove x xs = y (fun f xs -> let ys = remove_assoc x xs in if xs = ys then ys else f ys) xs)
    *)

    (** [remove_keys igns alist] is [alist] minus all associations whose keys are in [igns]. *)
    let remove_keys igns = foldr (fun (k,v) acc -> if mem k igns then acc else (k,v)::acc) []
    (* TODO TESTS *)

    (** [(replace ?all (k,v) alist)] replaces some or all of the
        bindings for [k] in [alist] with [(k,v)]; tail recursive.

        If [(all = false)] (the default) then only the first binding is
        replaced.

        The order of the bindings in [alist] is always preserved.

        Invariants:
        - [(replace (k,v) [] = [])]
        - [(replace (k,v) >> replace (k,v) = replace (k,v))]
        - [(replace (k,v) alist |> keys = keys alist)]
    *)
    let replace ?(all=false) ((k,v) as r) alist =
      let rec loop acc = function
      |         []            -> rev acc
      | (k',_)::xs when k=k'  -> if all then loop (r::acc) xs else rev (r::acc) @ xs
      |      x::xs (* else *) -> loop (x::acc) xs
      in
      loop [] alist
    (*$Q replace
      Q.(pair int int) (fun (k,v) -> replace ~all:false (k,v) [] = [])
      Q.(pair int int) (fun (k,v) -> replace ~all:true  (k,v) [] = [])

      Q.(pair (pair small_int int) (list (pair small_int int))) (fun ((k,v),xs) -> replace (k,v) xs |> replace (k,v) = replace (k,v) xs)
      Q.(pair (pair small_int int) (list (pair small_int int))) (fun ((k,v),xs) -> let r = replace ~all:true in r (k,v) xs |> r (k,v) = r (k,v) xs)

      Q.(pair (pair small_int int) (list (pair small_int int))) (fun ((k,v),xs) -> remove k xs |> replace ~all:false (k,v) = remove k xs)
      Q.(pair (pair small_int int) (list (pair small_int int))) (fun ((k,v),xs) -> remove k xs |> replace ~all:true  (k,v) = remove k xs)

      Q.(pair (pair small_int int) (list (pair small_int int))) (fun ((k,v),xs) -> replace ~all:false (k,v) xs |> keys = keys xs)
      Q.(pair (pair small_int int) (list (pair small_int int))) (fun ((k,v),xs) -> replace ~all:true  (k,v) xs |> keys = keys xs)

      Q.(pair (pair small_int int) (small_list (pair small_int int))) (fun ((k,v),xs) -> not (mem k xs) || replace ~all:false (k,v) xs |> assoc  k = v)
      Q.(pair (pair small_int int) (small_list (pair small_int int))) (fun ((k,v),xs) -> not (mem k xs) || replace ~all:true  (k,v) xs |> assocs k |> nub = [v])
    *)

    (** [(coalesce alist)] converts an alist with repeating keys into one with unique keys bound to lists of values.

        The order of elements in the result is sorted as by {!Stdlib.compare}.

        Invariants:
        - [(coalesce xs |> flatmap (fun (k,vs) -> map (Pair.up k) vs) |> sort compare) = (sort compare xs)]
        - [(keys xs |> List.sort_uniq compare |> map (id *** flip assocs xs) |> sort compare) = (coalesce xs |> sort compare)]

        @author Matt Teichman
    *)
    let coalesce : ('a * 'b) list -> ('a * 'b list) list =
      fun (type a) alist ->
      let module Key = struct
          type t = a
          let compare = Stdlib.compare
        end in
      let module M = Map.Make (Key) in
      let initial = M.empty in
      let reducer tally (k, new_v) =
        match M.find_opt k tally with
        | Some old_v -> M.add k (new_v :: old_v) tally
        | None -> M.add k [new_v] tally
      in
      foldl reducer initial alist |> flip (M.fold (fun k v acc -> (k,rev v) :: acc)) []
    (*$Q coalesce
      Q.(list (pair small_int small_int)) (fun xs -> (coalesce xs |> flatmap (fun (k,vs) -> map (Pair.up k) vs) |> sort compare) = (sort compare xs))
      Q.(list (pair small_int small_int)) (fun xs -> (keys xs |> List.sort_uniq compare |> map (id *** flip assocs xs) |> sort compare) = (coalesce xs |> sort compare))
    *)

    (** [(adjoin vadd empty)] returns a function [(f alist k v)] that
        adjoins [v] to a collection of values associated with key [k] in
        association list [alist]; {m O(n)}.

        See {!Map.Make.adjoin} (which is more efficient) for a discussion and example.
    *)
    let adjoin vadd z k v m = match partition (fun (k',_) -> k' = k) m with
    | [], m'            -> add k (vadd v z)  m'
    | ((_,v') :: _), m' -> add k (vadd v v') m'
    (*$inject let mad,mz,mlist = let module M = Map.Make (Int) in M.(adjoin cons [],empty,to_list) *)
    (*$Q adjoin
      Q.(list small_int) (fun xs -> (foldl (fun a x -> adjoin cons [] x x a) [] xs) |> sort compare = (foldl (fun a x -> mad x x a) mz xs |> mlist |> sort compare))
     *)

    (** [(classify f add z x alist)] returns a classifying adjoiner.

        [add] could be [cons] and [z], [[]], or [add] could be [S.add]
        and [z], [S.empty] for some {!Set} [S].

        [f] is a function that classifies an ['a] by mapping it to
        some equivalence class; [f] could be {!even}, or
        {!String.lowercase_ascii}.

        For example, [foldr (classify even cons []) []] applied to an
        [int list] returns an alist of length 2 (the size of the
        codomain of [even]) mapping [true] to a list of even integers
        and [false] to a list of odd integers.
     *)
    let classify f vadd z = fun (x : 'a) m -> adjoin vadd z (f x) x m
    (*$Q classify
      Q.(list small_int) (fun xs -> foldl (fun a x -> classify even cons [] x a) [] xs |> all (fun (x,ys) -> all (even $ (=) x) ys))
      Q.(list small_int) (fun xs -> foldl (fun a x -> classify id   cons [] x a) [] xs |> all (fun (x,ys) -> all ((=) x) ys))
     *)

    (** [(incr ?z ?i k alist)] is [(adjoin (+) z k i alist)], i.e. [incr]
        generates a function for counting instances of items ([k]) in
        an [alist].

        [~z] is the "zero" for the counters (default: [0]); [~i] is the
        increment (default: [1]).
     *)
    let incr ?(z=0) ?(i=1) k m = adjoin (+) z k i m
    (*$= incr
      [69,1] (incr 69 [])
      [69,11] (incr ~z:10 69 [])
      [69,12] (incr ~z:10 ~i:2 69 [])
      [69,2] (incr 69 [69,1])
     *)

    (** {1 Combining Alists} *)

    (*$inject
      let sortcmp = sort compare
      let (!) = sortcmp
     *)

    (** [(merge xs ys)] merges the two alists; bindings in [xs] trump bindings in [ys].

        The order of the results is undefined.
     *)
    let merge xs ys =
      let rec loop acc = function
        | []        -> acc
        | (k,v)::ys -> match assoc k xs with
                       | exception Not_found -> loop ((k,v) :: acc) ys
                       | v'                  -> loop            acc ys
      in
      loop xs ys
    (*$Q merge
      Q.unit (fun () -> merge [] [] = [])
      Q.unit (fun () -> sortcmp (merge [1,10] [2,2]) = ![1,10;2,2])
      Q.unit (fun () -> sortcmp (merge [1,10] [1,1;2,2]) = ![1,10;2,2])
      Q.unit (fun () -> sortcmp (merge [1,1;2,2] [1,10]) = ![1,1;2,2])
      Q.(small_list (pair int int)) (fun xs -> sortcmp (merge [] xs) = !xs)
      Q.(small_list (pair int int)) (fun xs -> sortcmp (merge xs []) = !xs)
      Q.(small_list (pair int int)) (fun xs -> sortcmp (merge xs xs) = !xs)
      Q.(small_list (pair int int)) (fun xs -> xs = [] || let ys = hd xs :: xs in sortcmp (merge [] ys) = !ys)
      Q.(small_list (pair int int)) (fun xs -> xs = [] || let ys = hd xs :: xs in sortcmp (merge [] ys) = !ys)
      Q.(small_list (pair small_int small_int)) (fun ys -> let xs = (10_000,1) :: ys in sortcmp (merge xs ys) = !xs)
      Q.(small_list (pair small_int small_int)) (fun ys -> let xs = (10_000,1) :: ys in sortcmp (merge ys xs) = !xs)
      Q.(small_list (pair small_int small_int)) (fun ys -> ys = [] || let xs = (fst (hd ys), 10_000) :: ys in sortcmp (merge ys xs) = !ys)
      Q.(small_list (pair small_int small_int)) (fun ys -> ys = [] || let xs = (fst (hd ys), 10_000) :: ys in sortcmp (merge xs ys) = !xs)
    *)

    (** [(mergelong xs ys)] is exactly the same as {!merge}, but is
        significantly faster (up to 27 times) for long alists (100+).
     *)
    let mergelong (type a b) : (a * b) list -> (a * b) list -> (a * b) list = fun xs ys ->
      let module M = Map.Make (struct type t = a let compare = compare end) in
      (* TODO adjoin, of_list should be from Prelude.Map, which isn't yet defined! *)
      let adjoin vadd empty k v m = default empty (M.find k) m |> vadd v |> flip (M.add k) m in
      let of_list m list = List.fold_left (fun (acc : (b list) M.t) (k,v) -> adjoin cons [] k v acc) m list in
      let u = M.union (fun k x _ -> Some x) (of_list M.empty xs) (of_list M.empty ys) in
      M.fold (fun k vs acc -> foldl (fun vlist v -> (k,v) :: vlist) acc vs) u []
    (*$Q mergelong
      Q.unit (fun () -> mergelong [] [] = [])
      Q.unit (fun () -> sortcmp (mergelong [1,10] [2,2]) = ![1,10;2,2])
      Q.unit (fun () -> sortcmp (mergelong [1,10] [1,1;2,2]) = ![1,10;2,2])
      Q.unit (fun () -> sortcmp (mergelong [1,1;2,2] [1,10]) = ![1,1;2,2])
      Q.(small_list (pair int int)) (fun xs -> sortcmp (mergelong [] xs) = !xs)
      Q.(small_list (pair int int)) (fun xs -> sortcmp (mergelong xs []) = !xs)
      Q.(small_list (pair int int)) (fun xs -> sortcmp (mergelong xs xs) = !xs)
      Q.(small_list (pair int int)) (fun xs -> xs = [] || let ys = hd xs :: xs in sortcmp (mergelong [] ys) = !ys)
      Q.(small_list (pair int int)) (fun xs -> xs = [] || let ys = hd xs :: xs in sortcmp (mergelong [] ys) = !ys)
      Q.(small_list (pair small_int small_int)) (fun ys -> let xs = (10_000,1) :: ys in sortcmp (mergelong xs ys) = !xs)
      Q.(small_list (pair small_int small_int)) (fun ys -> let xs = (10_000,1) :: ys in sortcmp (mergelong ys xs) = !xs)
      Q.(small_list (pair small_int small_int)) (fun ys -> ys = [] || let xs = (fst (hd ys), 10_000) :: ys in sortcmp (mergelong ys xs) = !ys)
      Q.(small_list (pair small_int small_int)) (fun ys -> ys = [] || let xs = (fst (hd ys), 10_000) :: ys in sortcmp (mergelong xs ys) = !xs)
    *)

    (** [(to_string ?left ?sep ?right f g xs)] returns a string representation of [xs].

        [to_string] is [(List.to_string ?left ?sep ?right (Pair.to_string f g))].
     *)
    let to_string ?left ?sep ?right f g xs =
      to_string ?left ?sep ?right (Pair.to_string f g) xs

    (** [(print ?left ?sep ?right f g xs)] is [(to_string ?left ?sep ?right f g xs >> print_endline)].*)
    let print ?left ?sep ?right f g xs = to_string ?left ?sep ?right f g xs |> print_endline

    (**/**)
    (* TODO UNFINISHED *)
    module Validate (K : sig type t val to_string : t -> string end) = struct
      type presence   = REQ | OPT
      type numeration = UNIQ | REP
      type pn = presence * numeration
      type 'v scheme = {
          k : K.t;
          p : presence;
          n : numeration;
          v : 'v -> ('v,string) result }
      let make k = { k; p = OPT; n = REP; v = fun v -> Ok v }
      let req  = function (* TODO BUG *) { p=OPT } as x -> { x with p = REQ  } | _ -> assert false (* TODO ASSERT *)
      let opt  = function { p=OPT } as x -> { x with p = OPT  } | _ -> assert false (* TODO ASSERT *)
      let uniq = function { n=REP } as x -> { x with n = UNIQ } | _ -> assert false (* TODO ASSERT *)
      let rep  = function { n=REP } as x -> { x with n = REP  } | _ -> assert false (* TODO ASSERT *)
      let valid v scheme = { scheme with v }
      type 'a schema = 'a scheme list
      (** [(validate schema alist)] validates ONE alist *)
      let validate schema alist =
        let rec loop fs alist =
          match alist with
          | []         -> fs
          | (k,v)::kvs -> loop (incr k fs) kvs
        in
        let valid counts schema =
          let reqtest x errs =
            if x.p = REQ
            then if mem_assoc x.k counts
                 then errs
                 else (x.k, `REQ) :: errs
            else errs
          in
          let uniqtest x errs =
            if x.n = UNIQ
            then if default 0 (assoc x.k) counts > 1
                 then (x.k, `UNIQ) :: errs
                 else errs
            else errs
          in
          let valuetest x errs = match assoc x.k alist with
            | exception Not_found -> errs
            | v -> match x.v v with
                   | Ok _      -> errs
                   | Error msg -> (x.k, `VALUE (v,msg)) :: errs
          in
          let rec loop errs = function
            | []    -> rev errs
            | x::xs -> loop (errs |> reqtest x |> uniqtest x |> valuetest x) xs
          in
          loop [] schema
        in
        let counts = loop [] alist in
        valid counts schema
      let format_error = function
        | (f, `REQ)         -> K.to_string f |> sprintf "%%%s: REQUIRED field missing"
        | (f, `UNIQ)        -> K.to_string f |> sprintf "%%%s: UNIQ field occurs multiple times"
        | (f, `KEY)         -> K.to_string f |> sprintf "%%%s: KEY field has multiple values"
        | (f, `VALUE (v,e)) -> K.to_string f |> sprintf "%%%s: %s: NOT %s" v e
      let keycheck key kvss =
        let schema = [make key |> req; make key |> uniq] in
        let eachkvs (vs, errs) kvs = match validate schema kvs with
          | []   -> assert (succeeds (assoc key) kvs); incr (assoc key kvs) vs, errs
          | errs -> vs, errs
        in
        let eachvalue errs (v,n) =
          if n > 1
          then (key, `KEY) :: errs
          else errs
        in
        match foldl eachkvs ([], []) kvss with
        | counts, errs -> foldl eachvalue errs counts
    end
    (**/**)

  end (*$>*)

  (** {1:lists_sorting Sorting and Such}

      See also {!on} for a nice way to make comparison functions.
  *)

  (** [(sorted cmp list)] is [true] iff [(sort cmp list = list)] and
      is [false] otherwise; {m O(n)}; tail-recursive, constant space.

      [sorted] terminates at the first out-of-order value (if any), so
      it's much faster than [(sort cmp list = list)], especially for
      unsorted lists.  *)
  let sorted cmp list =
    let rec loop = function
    | x::y::xs when cmp x y <= 0 -> loop (y::xs)
    | x::y::xs (* otherwise *)   -> false
    |    y::[]                   -> true
    |       []                   -> true
    in
    loop list
  (*$Q sorted
    Q.(small_list int) (fun xs -> if sort Int.compare xs = xs then sorted Int.compare xs else not (sorted Int.compare xs))
  *)

  (** [(uniq ?compare list)] is the list of representative elements of the identical subsequences of [xs]; tail-recursive.

      It's like {!sort_uniq} but gives a different result for unsorted
      inputs.

      Example:
      - [(uniq [1;1;2;3;3;3;3;2;2;3;3]) = [1; 2; 3; 2; 3]]

      Invariant:
      - ∀xs : [(sort compare xs |> uniq ~compare) = (sort_uniq compare xs)]

      @param compare the comparison function (default: [compare])
  *)
  let uniq ?compare list =
    let compare = Option.default Stdlib.compare compare in
    let rec uniq' l acc = match l,acc with
    |   []  ,     acc                            -> acc
    | hd::tl,     []                             -> uniq' tl [hd]
    | hd::tl, prev::acc when compare hd prev = 0 -> uniq' tl (prev::acc)
    | hd::tl, prev::acc                          -> uniq' tl (hd::prev::acc)
    in
    uniq' list [] |> rev
  (*$= uniq
    [] (uniq [])
    [] (uniq ~compare:compare [])
    [1] (uniq [1])
    [1] (uniq ~compare:compare [1])
    [1;2] (uniq [1;2])
    [1;2] (uniq ~compare:compare [1;2])
    [1; 2; 3; 2; 3] (uniq [1;1;2;3;3;3;3;2;2;3;3])
    (uniq (repeat 10 1)) [1]
    (uniq ~compare:(fun a b -> 1) (repeat 10 1)) (repeat 10 1)
  *)
  (*$Q uniq
    Q.(list small_int) (fun xs -> uniq xs = (uniq xs |> uniq))
    Q.(list small_int) (fun xs -> uniq ~compare:compare xs = (uniq ~compare:compare xs |> uniq ~compare:compare))
    Q.(list small_int) (fun xs -> (sort compare xs |> uniq) = (sort_uniq compare xs))
    Q.(list small_int) (fun xs -> (sort compare xs |> uniq ~compare:compare) = (sort_uniq compare xs))
  *)

  (** [(uniqc ?compare xs)] is like [(uniq xs)] but each element is paired with
      the size of the original subsequence; tail-recursive.

      Example:
      - [(uniqc [1;1;2;3;3;3;3]) = [(2, 1); (1, 2); (4, 3)]]

      @param compare the comparison function (default: [compare])
   *)
  let uniqc ?compare list =
    let compare = Option.default Stdlib.compare compare in
    let rec uniq' = function
    |   []  ,        acc                             -> acc
    | hd::tl,         []                             -> uniq' (tl, [1,hd])
    | hd::tl, (n,prev)::acc when compare hd prev = 0 -> uniq' (tl, ((n+1,prev)::acc))
    | hd::tl,     prev::acc                          -> uniq' (tl, ((1,hd)::prev::acc))
    in
    uniq' (list, []) |> rev
  (*$= uniqc
    [] (uniqc [])
    [] (uniqc ~compare:compare [])
    [1,1] (uniqc [1])
    [1,1] (uniqc ~compare:compare [1])
    [(2, 1); (1, 2); (4, 3)] (uniqc [1;1;2;3;3;3;3])
    [(2, 1); (1, 2); (4, 3)] (uniqc ~compare:compare [1;1;2;3;3;3;3])
  *)
  (*$Q uniqc
    Q.(list small_int) (fun xs -> (uniqc xs |> map snd) = uniq xs)
    Q.(list small_int) (fun xs -> (uniqc ~compare:compare xs |> map snd) = uniq ~compare:compare xs)
    Q.(list small_int) (fun xs -> sort_uniq compare xs |> uniqc |> map fst |> all ((=) 1))
    Q.(list small_int) (fun xs -> let xs' = sort_uniq compare xs in (uniqc xs' |> len) = len xs')
    Q.(list small_int) (fun xs -> (sort_uniq compare xs |> uniqc |> len) <= len xs)
  *)

  (** {1:lists_indexing Indexing and Projection} *)

  (** [(index ?z list)] is an [alist] whose keys are successive
      integers (starting from [z] (default: [0]) and whose values are
      the elements of [list]; tail-recursive.

      Examples:
      - [(index      (1--4)) = [(0, 1); (1, 2); (2, 3); (3, 4)]]
      - [(index ~z:1 (1--4)) = [(1, 1); (2, 2); (3, 3); (4, 4)]]
  *)
  let index ?(z=0) list =
    let rec loop i acc = function
      |  []   -> rev acc
      | x::xs -> loop (succ i) ((i,x)::acc) xs
    in
    loop z [] list
  (*$= index
    [] (index [])
  *)
  (*$Q index
    Q.int (fun z -> [] = index ~z [])
    Q.(pair int (list int)) (fun (z,xs) -> index ~z xs = zip (z -- (z + (len xs - 1))) xs)
    Q.(list int) (fun xs -> index ~z:0 xs = index xs)
  *)

  (** [(pos x xs)] is the position (index) of the first [x] in [xs],
      or [-1] if [x] is {i NOT} in [xs]; tail-recursive. *)
  (* TODO TEST ~eq *)
  let pos ?eq x xs =
    let eq = Option.default (=) eq in
    let rec loop i = function
    | []                    -> ~-1
    | y::ys when eq y x     -> i
    | _::ys (* otherwise *) -> loop ~++i ys
    in
    loop 0 xs
  (*$Q pos
    Q.(pair small_int (list small_int)) (fun (x,xs) -> pos x xs = (index xs |> catch (find (fun (i,y) -> y=x) $ fst) |> Option.either id  ~-1))
  *)

  (** [(project ?relaxed is xs)] projects the elements of [xs] indexed by the
      indices in [is]; tail-recursive in the length of [is].

      The result is in the order of the occurrence of the indices in [is].

      If any index in [is] [>= (len xs)] or [< 0], [Not_found] is raised,
      unless [~relaxed:true], in which case any bad indices are
      ignored, and the length of the result may be less than the length of [is].

      Examples:
      - [(project [10] (1--20)) = [11]]
      - [(project [3;2;1;0] (1--4)) = [4; 3; 2; 1]]
      - [(project [4;4;4] (1--10)) = [5; 5; 5]]

      Invariants:
      - [((project ~relaxed:false is xs) |> len) = (len is)]
      - [((project ~relaxed:false is []) && is <> []) = ⊥]
      - [(project ~relaxed:true  is []) = []]
      - [(project [] xs) = []]
      - ∀n . n < 1 [let xs = 1--n in (project (map pred xs) xs) = xs]
      - [(project is xs) = (map (List.nth xs) is)]

      @raise Not_found if [not relaxed] and any index [>= (len xs)] or [< 0]
  *)
  let project ?(relaxed=false) is xs =
    if relaxed
    then flatmap (Pre.default [] (consup $. (reraise Not_found nth xs))) is
    else map (reraise Not_found (nth xs)) is
  (*$= project
    [] (project [] [])
    [] (project [] (1--10))
    [1] (project [0] (1--10))
    (1--10) (project (0--9) (1--10))
    [] (project ~relaxed:true [] [])
    [] (project ~relaxed:true [] (1--10))
    [1] (project ~relaxed:true [0] (1--10))
    (1--10) (project ~relaxed:true (0--9) (1--10))
    [] (project ~relaxed:false [] [])
    [] (project ~relaxed:false [] (1--10))
    [1] (project ~relaxed:false [0] (1--10))
    (1--10) (project ~relaxed:false (0--9) (1--10))
   *)
  (*$Q project
    Q.(list int) (fun xs -> project [] xs = [])
    Q.(list int) (fun xs -> project ~relaxed:true [] xs = [])
    Q.(list int) (fun xs -> project ~relaxed:false [] xs = [])
    Q.(list int) (fun xs -> project [] xs = [])
    Q.(list int) (fun xs -> project ~relaxed:true [] xs = [])
    Q.(list int) (fun xs -> project ~relaxed:false [] xs = [])
    Q.(list int) (fun xs -> let js = 0 -- (len xs - 1) in project js xs = map (nth xs) js)
    Q.(list int) (fun xs -> project (0--(len xs - 1)) xs = xs)
    Q.(list int) (fun xs -> project (rev (0--(len xs - 1))) xs = rev xs)
    Q.small_nat (fun n -> let xs = 1--n in project (map pred xs) xs = xs)
    Q.small_nat (fun n -> let xs = 1--n in project ~relaxed:true (map pred xs) xs = xs)
    Q.small_nat (fun n -> let xs = 1--n in project ~relaxed:false (map pred xs) xs = xs)
    Q.(list int) (fun xs -> project ~relaxed:true xs [] = [])
   *)

  (** {1:lists_as_sets Lists as Sets}

      Many of these functions are extremely inefficient compared to
      using {!Set}.  For use on lists guaranteed to be extremely
      short, or for playing in the top-level.

      In most cases, in the spirit of "lists as sets", the order of
      elements is not preserved.
  *)

  (** [(nub ?compare xs)] is the list [xs] with equal elements
      removed; tail-recursive; {m O(n log n)}.

      Equality is determined by [compare] (default: [Stdlib.compare]).

      The order of the elements in [xs] is not preserved.

      Runs in constant heap space (in addition to the size of the
      result list) and logarithmic stack space.

      [nub (1--5 @ 1--3 @ 4--6) = [1; 2; 3; 4; 5; 6]]
  *)
  let nub ?compare:cmp xs =
    let compare = match cmp with Some f -> f | _ -> Stdlib.compare in
    sort_uniq compare xs
  (*$= nub
    [] (nub [])
    [] (let compare = on Stdlib.compare (flip (mod) 2) in nub ~compare [])
    (1--10) (nub (1--10))
    (1--10) (nub (1--10 @ 1--10))
   *)
  (*$Q nub
    Q.(small_list small_int) (fun xs -> nub xs = nub (nub xs))
    Q.(small_list small_int) (fun xs -> nub ~compare xs = nub ~compare (nub ~compare xs))
    Q.(small_list small_int) (fun xs -> let compare = on Stdlib.compare (flip (mod) 2) in nub ~compare xs = nub ~compare (nub ~compare xs))
    Q.(small_list string)    (fun xs -> xs = [] || let xs' = hd xs :: xs in nub xs' = nub xs)
  *)

  (** [nub2] is [nub] but the order of the elements of the list {i is}
      preserved; {i not} tail-recursive; {m O(n^2)}.)

      The {i 2} in [nub2] is a reminder that it's {m O(n^2)}.
   *)
  let nub2 ?compare:cmp lst =
    let compare = match cmp with Some f -> f | _ -> Stdlib.compare in
    let mem x = succeeds (find (compare x >> eq 0)) in
    let rec nub' acc = function
      | [] -> []
      | x::xs ->
         if mem x acc
         then nub' acc xs
         else x :: (nub' (x::acc) xs)
    in nub' [] lst
  (*$Q nub;nub2
    Q.(small_list small_int) (fun xs -> nub2 xs |> sort compare = nub xs)
  *)

  (** [(union ?compare xs ys)] returns the list union of the two lists (with no
      duplicates); {i not} tail-recursive (length of the first argument).

      Equality is determined by [compare] (default: [Stdlib.compare]).

      The order of the elements in [xs] and [ys] is not preserved.

      - [(union xs ys) = (xs @ ys |> nub)]
  *)
  let union ?compare xs ys = trappend xs ys |> nub ?compare
  (*$= union
    [] (union [] [])
   *)
  (*$Q union
    Q.(pair (small_list small_int) (small_list small_int)) (fun (xs,ys) -> sort compare (union xs ys) = sort compare (nub (xs@ys)))
    Q.(pair (small_list small_int) (small_list small_int)) (fun (xs,ys) -> sort compare (union xs ys) = sort compare (union ys xs))
    Q.(small_list small_int) (fun xs -> union [] xs = nub xs && union xs [] = nub xs)
    Q.(small_list small_int) (fun xs -> union xs xs = nub xs)
   *)

  (** [(intersect ?compare xs ys)] returns the list intersection of the two
      lists; tail-recursive; {m O(n^2)}.

      Equality is determined by [compare] (default: [Stdlib.compare]).

      The order of the elements in [xs] and [ys] is not preserved.
   *)
  let intersect ?compare:cmp xs ys =
    let compare = match cmp with Some f -> f | _ -> Stdlib.compare in
    let mem x = succeeds (find (compare x >> eq 0)) in
    foldl (snocwhen (fun v -> mem v ys)) [] xs |> nub
  (*$= intersect
    [] (intersect [] [])
    [] (intersect (1--10) (11--20))
    [2] (intersect [1;2;3] [2;4;5])
    (1--10) (intersect (11::1--10) (1--10))
    (1--10) (intersect (1--10) (11::1--10))
    (1--10) (intersect (1--10@[11]) (1--10))
    (1--10) (intersect (1--10) (1--10@[11]))
   *)
  (*$Q intersect
    Q.(small_list int) (fun xs -> intersect [] xs = [])
    Q.(small_list int) (fun xs -> intersect xs [] = [])
    Q.(small_list int) (fun xs -> intersect xs xs = nub xs)
   *)

  (** [(subset ?compare xs ys)] is [true] if [xs] is a subset of [ys]; {m O(n^2)}.

      Equality is determined by [compare] (default: [Stdlib.compare]).

      The order of the elements in [xs] and [ys] is not preserved.
   *)
  let subset ?compare xs ys = intersect ?compare xs ys |> nub ?compare = nub ?compare xs
  (*$Q subset
    Q.unit (fun () -> subset [] [])
    Q.(small_list int) (fun xs -> xs = [] || not (subset xs []))
    Q.(small_list int) (fun xs -> subset [] xs)
    Q.(small_list int) (fun xs -> subset xs xs)
    Q.(small_list int) (fun xs -> xs = [] || maximum xs = max_int || not (subset (maximum xs + 1 :: xs) xs))
    Q.(small_list int) (fun xs -> xs = [] || maximum xs = max_int || subset xs (maximum xs + 1 :: xs))
  *)

  (** [(diff ?compare xs ys)]: list difference: the elements of [xs] minus all
      of the elements in [ys]; tail-recursive, {m O(n^2)}.

      Equality is determined by [compare] (default: [Stdlib.compare]).

      The order of the elements in [xs] and [ys] is not preserved.
   *)
  let diff ?compare:cmp xs ys =
    let compare = match cmp with Some f -> f | _ -> Stdlib.compare in
    let mem x = succeeds (find (compare x >> eq 0)) in
    let each acc x = if mem x ys then acc else x::acc in
    foldl each [] xs |> rev
  (*$Q diff
    Q.(small_list int) (fun xs -> diff [] xs = [])
    Q.(small_list int) (fun xs -> diff xs xs = [])
    Q.(small_list int) (fun xs -> diff xs [] = xs)
    Q.(pair (small_list int) (small_list int)) (fun (c,a) -> diff c (diff c a) = intersect c a)
   *)

  (** [(cartesian_product xss)] is the Cartesian product of the list of
      lists [xss]; not tail-recursive.

      Deck of cards:
      {[ (cartesian_product [["♠";"♥";"♦";"♣"];("A"::map string_of_int (2--10)@["J";"Q";"K"])]) ]}
  *)
  let rec cartesian_product = function
  | [] -> [[]]
  | h :: t ->
      let rest = cartesian_product t in
      List.concat (List.map (fun i -> List.map (fun r -> i :: r) rest) h)
  (*$Q cartesian_product
    Q.(pair (small_list int) (small_list int)) (fun (a,b) -> cartesian_product [a;b] |> len = (len a * len b))
   *)

  (** [(powerset xs)] is the list of all sublists of [xs], including [[]] and [xs] itself;
      {i not} tail-recursive. *)
  let powerset xs =
    let rec powerset' = function
      |  []  -> [[]]
      | x::xs -> let sl = powerset' xs in sl @ (map (cons x) sl)
    in
    powerset' (nub xs)
  (*$Q powerset
    Q.unit (fun () -> powerset [] = [[]])
   *)
  (* these tests consume too much space
    Q.(pair int (small_list int)) (fun (n,xs) -> let p = xs :: powerset xs in mem (choose 1 p |> hd) p)
    Q.(pair int (small_list int)) (fun (n,xs) -> let p = xs :: powerset xs in mem (choose 1 p |> hd) p)
    Q.(small_list int) (fun xs -> powerset xs |> len = (2. ** (len xs |> float) |> int_of_float))
   *)

  (** [(combinations n lst)] is the list of all subset lists of [lst]
      consisting of [n] elements; {i not} tail-recursive.

      @author Matt Teichman
   *)
  let rec combinations n lst =
    match n, nub lst with
    | 0, _ -> [[]]
    | _, []-> []
    | n, x :: xs ->
       (List.map (fun xs -> x :: xs) @@ combinations (n-1) xs) @ (combinations n xs)
  (*$Q combinations
    Q.int (fun n -> combinations n [] = [])
    Q.(pair (int_range 0 10) (list_of_size Gen.(int_range 0 10) int)) \
      (fun (n,xs) -> combinations n xs |> all (len >> eq n))
    Q.(list_of_size Gen.(int_range 0 15) int) (fun xs -> combinations (nub xs |> len) xs = [nub xs])
    Q.(pair (int_range 0 10) (list_of_size Gen.(int_range 0 10) int)) \
      (fun (n,xs) -> powerset xs |> filter (fun xs -> len xs = n) |> map nub |> sort compare = (combinations n xs |> sort compare))
   *)

  (** {1:lists_iteration List Iteration} *)

  (** [iter] is [iter]. *)
  let iter = iter

  (** [iteri] is [iteri]. *)
  let iteri = iteri

  (** [iter2] is [iter2]. *)
  let iter2 = iter2

  (** {1:lists_monad The List Monad}
      These functions can be used to hand-translate Haskell-style list
      comprehensions.  For example:

      {@haskell[
      [ (n,c) | n <- [1,2], c <- ['a','b'] ] = [(1, 'a'); (1, 'b'); (2, 'a'); (2, 'b')]
      ]}

      This can be expressed as:

      {[ [1;2] >>= fun n -> ['a';'b'] >>= fun c -> return (n,c) ]} *)

  (** [(bind list f)] is [(flatten (map f list))]. *)
  let bind list f = flatmap f list
  (*$inject
    let (>>=) = bind
   *)
  (*$Q (>>=); return
    Q.int (fun a -> (return a >>= fun x -> [succ x]) = [succ a])
    Q.int (fun m -> ([m] >>= return) = [m])
    Q.(list int) (fun xs -> (xs >>= (return << succ)) = map succ xs)
   *)
  (*$Q (>>=)
    Q.int (fun m -> ([m] >>= (fun x -> (fun y -> [succ y]) x >>= (fun y -> [y*y]))) = (([m] >>= (fun y -> [succ y])) >>= (fun y -> [y*y])))
   *)

  (** [(return n)] is [[n]]. *)
  let return n = [n]
  (*$Q return
    Q.int (fun x -> return x = consup x)
    Q.int (fun x -> return x = [x])
   *)

  (** {2:lists_ops Ops} *)

  (** Infix and prefix operators. *)
  module Ops = struct
    (** [( * )] is {!cartesian_product}. *)
    let ( * ) a b = cartesian_product [a;b]

    (** [(--)] is {!upto}. *)
    let (--) = upto

    (** [(>>=)] is {!bind}. *)
    let (>>=) = bind
    let ( let* ) = bind
  end
end (*$>*)

(** This is Ocaml's standard [List] with the additional functions from {!Lists}. *)

module List = struct
  (** @closed *)
  include List
  let compare = compare  (* see "What's the deal with compare?" above *)

  (** @closed *)
  include Lists
end

(** [List] is opened. *)

(** @closed *)
include List

(** [assocs] is {!List.Assoc.assocs}. *)
let assocs = Assoc.assocs

(** [(--)] is {!List.Ops.(--)}. *)
let (--) = Ops.(--)

(** {1:list1 Non-Empty Lists} *)

(** Non-Empty Lists

    This is a type of lists that excludes the possibility of the empty
    list.  Thus, [hd] cannot raise an exception and you never need to
    check for the possibility of the empty list.
 *)
module List1 : sig
  type 'a t = 'a * 'a list
  val singleton : 'a -> 'a t
  val make : int -> (int -> 'a) -> 'a t
  val length : 'a t -> int
  val len : 'a t -> int
  val rev : 'a t -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a list
  val last : 'a t -> 'a
  val iter : ('a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val foldr : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
end = struct (*$< List1 *)
  (* generate a random List1 *)
  (*$inject
      let genl1 =
        let open QCheck.Gen in
        let gen = list_size (int_range 1 100) small_int >>= function [] -> assert false | x::xs -> return (x,xs) in
        let print = to_list $ List.to_string string_of_int in
        QCheck.make ~print gen
   *)
  type 'a t = 'a * 'a list
  let singleton x = x,[]
  (*$Q singleton
    Q.int (fun n -> singleton n |> hd = n)
    Q.int (fun n -> singleton n |> tl = [])
   *)
  let make n f =
    if n < 1
    then invalid_arg "make"
    else f 0, foldir (conswith f ) [] (1,~--n)
  (*$Q make
    Q.unit (fun () -> not @@ succeeds (make 0) id)
    Q.neg_int (fun n -> not @@ succeeds (make n) id)
    Q.small_int (fun n -> n < 1 || len (make n id) = n)
    Q.small_int (fun n -> n < 1 || let r = Random.int n in make n id |> to_list |> List.get r = r)
    Q.small_int (fun n -> n < 1 || make n id |> to_list = List.make n id)
    Q.(pair int small_int) (fun (x,n) -> n < 1 || make n (k x) |> to_list |> List.get (Random.int n) = x)
   *)
  let of_list = function [] -> failwith "of_list" | x::xs -> x, xs
  (*$Q of_list
    Q.unit (fun () -> not @@ succeeds of_list [])
    Q.(list int) (fun xs -> xs = [] || of_list xs |> len > 0)
   *)
  let to_list = function (x, xs) -> x :: xs
  (*$Q to_list
    genl1 (fun xs -> to_list xs |> List.length > 0)
    Q.(list int) (fun xs -> xs = [] || of_list xs |> to_list = xs)
   *)
  let rev (x,xs) =
    let rec loop acc = function
      | []    -> acc
      | y::ys -> loop (y::acc) ys
    in
    loop [x] xs |> of_list
  (*$Q rev
    Q.int (fun n -> singleton n |> rev = singleton n)
    genl1 (fun xs -> rev xs |> rev = xs)
   *)
  let length (_,xs)= 1 + List.length xs
  (*$Q length
    Q.int (fun n -> singleton n |> length = 1)
    genl1 (fun xs -> to_list xs |> List.len = length xs )
    genl1 (fun xs -> length xs > 0)
   *)
  let len = length
  (*$Q len
    genl1 (fun xs -> len xs = length xs)
   *)
  let cons x = function (y,ys) ->  (x, (y::ys))
  (*$Q cons
    Q.(pair int int) (fun (n,m) -> singleton n |> cons m |> len = 2)
    Q.(pair int genl1) (fun (n,xs) -> cons n xs |> hd = n)
    Q.(pair int genl1) (fun (n,xs) -> cons n xs |> len = 1 + len xs)
   *)
  let append (x,xs) (y,ys) = List.append (x::xs) (y::ys) |> of_list
  (*$Q append
    Q.(pair int genl1) (fun (n,xs) -> let ys = append (singleton n) xs in hd ys = n && tl ys |> of_list = xs)
    Q.(pair genl1 genl1) (fun (xs,ys) -> let (!) = to_list in append xs ys |> to_list = List.append !xs !ys)
   *)
  let hd = function (x,_) -> x
  (*$Q hd
    Q.int (fun n -> singleton n |> hd = n)
    genl1 (fun xs -> to_list xs |> List.hd = hd xs)
    Q.(pair int genl1) (fun (n,xs) -> cons n xs |> hd = n)
   *)
  let tl = function (_,xs) -> xs
  (*$Q tl
    genl1 (fun xs -> to_list xs |> List.tl = tl xs)
   *)

  (* TODO DOC *)
  let rec last = function
    | x,[] -> x
    | _,x::xs -> last (x,xs)
  (* TODO TESTS *)

  let iter f (x,xs) = f x; List.iter f xs
  (*$Q iter
    genl1  (fun xs -> let r = ref 0 in iter (fun _ -> incr r) xs; !r = len xs)
    genl1  (fun xs -> let r = ref [] in iter (fun x -> r @:= x) xs; List.rev !r = to_list xs)
   *)
  let map f = function (x,xs) ->  (f x, List.map f xs)
  (*$Q map
    genl1 (fun xs -> map id xs |> len = len xs)
    genl1 (fun xs -> map id xs |> hd = hd xs)
    genl1 (fun xs -> map (k 0) xs |> to_list = repeat (len xs) 0)
    genl1 (fun xs -> map succ xs |> to_list |> sum = len xs + sum (to_list xs))
   *)
  let foldl f init = function (x,xs) -> List.foldl f init (x::xs)
  (*$Q foldl
    genl1 (fun xs -> foldl (-) 0 xs = List.foldl (-) 0 (to_list xs))
   *)
  let foldr f init = function (x,xs) -> List.foldr f init (x::xs)
  (*$Q foldr
    genl1 (fun xs -> foldr (-) 0 xs = List.foldr (-) 0 (to_list xs))
   *)
end (*$>*)

(** {1:random Random} *)

(** Additional random functions. *)
module Random = struct (*$< Random *)
  (* TODO TEST need a Student's T-test or the like to really test this... *)

  (** @closed *)
  include Random

  (**/**)
  module A = Array
  (**/**)

  (** Random functions for arrays. *)
  module Array = struct (*$< Array *)

    (** [(shuffle ?state a)] performs a {i destructive} in-place Fisher–Yates shuffle on the array [a]; {m O(n)}. *)
    let shuffle ?state a =
      let state = match state with
      | Some s -> s
      | None   -> State.make_self_init () in
      for i = Array.length a - 1 downto 1 do
        let j = State.int state ~++i in let aj = a.(j) in a.(j) <- a.(i); a.(i) <- aj
      done
      (* TODO so what about the highly unlikely case that this single shuffle is identical to the original? *)
      (*$T shuffle
        let a = A.of_list (1--100) in let c = A.copy a in shuffle c; c <> a && c |> A.to_list |> sort compare = A.to_list a
      *)
    (*$= shuffle
      [||]  (let a = [||]  in shuffle a; a)
      [|1|] (let a = [|1|] in shuffle a; a)
     *)
    (*$Q shuffle
      Q.(array_of_size Gen.small_nat int) (fun a -> let a' = Stdlib.Array.copy a in shuffle a'; Stdlib.Array.sort Int.compare a'; a' = (Stdlib.Array.sort Int.compare a; a))
     *)

    (** [(sample_with ?state n a)] returns a random sample, {i with}
        replacement, of size [n] of the array [a]; tail-recursive, {m O(n)}.

        @raise Invalid_argument if [a] is empty
    *)
    let sample_with ?state n a =
      let state = match state with
      | Some s -> s
      | None   -> State.make_self_init () in
      let m = A.length a in
      let each acc i =
        A.get a (State.int state m) :: acc
      in
      if a = [||] then invalid_arg "empty array"
      else foldil each [] (1,n)
    (*$Q sample_with
      Q.small_int (fun n -> not (succeeds (sample_with 1) (A.make 0 0)))
      Q.small_int (fun n -> n = 0 || let a = A.init n (fun _ -> Random.(int maxbound)) in sample_with 1 a |> hd |> flip A.mem a)
    *)

    (* TODO ALGORITHM why not just Array.shuffle a |> Array.to_seq |> Seq.take n? *)
    (* without replacement *)
    (** [(sample_without ?state n a)] returns a random sample, {i without}
        replacement, of size [n] of the array [a]; tail-recursive, {m O(n)}.

        @raise Invalid_argument if [a] is empty or if [n > Array.length a]
    *)
    let sample_without ?state n a =
      let module S = Set.Make (Int) in
      let state = match state with
      | Some s -> s
      | None   -> State.make_self_init () in
      let m = A.length a in
      let each (used,acc) _ =
        let i = State.(whilst (flip S.mem used) (fun _ -> int state m) (int state m)) in
        S.add i used, A.get a i :: acc
      in
      if n > m then invalid_arg (sprintf "%d > %d" n m)
      else if a = [||] then invalid_arg "empty array"
      else foldil each (S.empty,[]) (1,n) |> snd
      (* TODO TEST test with ~state *)
      (*$Q sample_without
        Q.small_int (fun n -> not @@ succeeds (sample_without @@ n+1) (A.make n 0))
        Q.small_int (fun n -> n = 0 || let a = A.init n (fun _ -> Random.(int maxbound)) in sample_without 1 a |> hd |> flip A.mem a)
        Q.small_int (fun n -> n = 0 || let a = A.init n (fun _ -> Random.(int maxbound)) in sample_without n a |> fast_sort compare= fast_sort compare (A.to_list a))
      *)
  end (*$>*)

  (** [maxbound] is the maximum integer than can be passed to [Random.int]. *)
  let maxbound = Pre.Random.maxbound

  (** @deprecated Use {!Array.shuffle}. *)
  let fisher_yates a = Array.shuffle a; a

  (** [(shuffle ?state list)] returns a list, via a Fisher–Yates shuffle,
      with the same contents as [list] but in a random order;
      tail-recursive, {m O(n)}.

      {i N.B.} this convenience function uses {!Array.shuffle} and
      hence [list] makes a round-trip to and from a temporary array.
      To avoid this cost, you should do your own type conversion
      whenever possible.
  *)
  let shuffle ?state list = let a = A.of_list list in Array.shuffle ?state a;  A.to_list a
  (* TODO so what about the highly unlikely case that this single shuffle is identical to the original? *)
  (*$T shuffle
    shuffle [] = []
    let a = 1--100 in shuffle a <> a && shuffle a |> sort compare = a
  *)

  (** [choose ?state ?n m l]: choose [m] random elements {i without replacement} from a list of length [n].

      If [state] is provided, it is used as the random state.

      1-pass algorithm due to Knuth; if you don't provide [n], requires 2 passes.*)
  let choose ?state ?n m lst =
    let state = match state with
    | Some s -> s
    | None   -> Random.State.make_self_init () in
    let rec rc acc m n = function
    |   []   -> acc
    | h :: t ->
        if Random.State.int state n < m
        then rc (h :: acc) (m - 1) (n - 1) t
        else rc acc m (n - 1) t
    in
    let n = match n with Some n -> n | None -> List.length lst in
    rc [] m n lst
  (* TODO TEST with ~state *)
  (*$Q choose
    Q.(list small_int) (fun l -> choose 0 l = [])
    Q.(list small_int) (fun l -> choose ~n:(len l) 0 l = [])
    Q.(pair small_int (list small_int)) (fun (i,l) -> i >= len l || choose i l |> len = i)
    Q.(list small_int) (fun l -> l = [] || List.mem (choose 1 l |> hd) l)
  *)
end (*$>*)

(** [shuffle] is {!Random.shuffle}. *)
let shuffle = Random.shuffle

(** [choose] is {!Random.choose}. *)
let choose = Random.choose

(** {1:hashtables Hash Tables} *)

(** This is Ocaml's standard [Hashtbl] with some additional functions. *)
module Hashtbl = struct (*$< Hashtbl*)
  (** @closed *)
  include Hashtbl

  (** [(of_list alist)] is the hash table that's equivalent to the alist. *)
  let of_list list =
    let ht = Hashtbl.create (len list) in
    List.iter (uncurry (Hashtbl.add ht)) list;
    ht
  (*$= of_list
    0 (of_list [] |> Hashtbl.length)
   *)
  (*$Q of_list
    Q.small_int (fun n -> n < 0 || 1--n |> map (id *** id) |> of_list |> Hashtbl.length = n)
    Q.(pair int int) (fun (k,v) -> of_list [k,v] |> flip Hashtbl.find k = v)
  *)

  (** [(to_list ht)] is the alist that's equivalent to the hash table.

      Invariant:
      - [(of_list xs |> to_list |> sort compare) = (sort compare xs)]
  *)
  let to_list ht = fold (curry cons) ht []
  (*$Q to_list
    Q.small_int (fun n -> n < 0 || 1--n |> map (id *** id) |> of_list |> to_list |> sort compare = (1--n |> map (id *** id)))
    Q.(pair (small_list int) (small_list int)) (fun (ks,vs) -> zip ks vs |> of_list |> to_list |> sort compare = (zip ks vs |> sort compare))
  *)

  (** [(adjoin vadd empty)] returns a function [(f ht k v)] that
      adjoins [v] to a collection of values associated with key [k] in
      hash table [ht].

      See {!Map.Make.adjoin} for a discussion and example.
  *)
  let adjoin = Pre.Hashtbl.adjoin
  (* test: see Pre.Hashtbl.adjoin *)

  (** [(classify f vadd empty)] is a classifying adjoiner. *)
  let classify f vadd empty = fun x m -> adjoin vadd empty (f x) x m
  (* TODO TEST *)

  (** [(random ?size ~key ~value ())] is a random hash table of size
      [(size ())] (default: [< 100]), and each binding is given by [(key (), value ())]. *)
  let random ?size ~key ~value () =
    let n = match size with None -> Random.int 100 | Some f -> f () in
    let ht = create n in
    foreach (fun _ -> add ht (key ()) (value ())) (1,n);
    ht
  (* TODO TESTS *)
end (*$>*)

(** {1:arrays Arrays} *)

(** OCaml's [Array] module with some extra functions. *)
module Array = struct (*$< Array *)
  (** @closed *)
  include Array

  (** [len] is {!Array.length}. *)
  let len = Array.length
  (*$Q len
    Q.(array int) (fun a -> length a = len a)
  *)

  (** [foldl] is {!Array.fold_left}. *)
  let foldl = fold_left
  (*$Q foldl
    Q.(array small_int) (fun xs -> foldl snoc [] xs = Array.fold_left snoc [] xs)
  *)

  (** [(foldr f init a)] is [(Array.fold_right f a init)]. *)
  let foldr f init a = fold_right f a init
  (*$Q foldr
    Q.(array small_int) (fun xs -> foldr cons [] xs = Array.fold_right cons xs [])
  *)

  (** [(random ?size r ())] is a random array of size [(size ())]
      (default: [< 100]) and each element is given by [(r ())]. *)
  let random ?size r () =         (* TODO this code is duplicated in ia.ml AND a1.ml! *)
    let n = match size with None -> Random.int 100 | Some f -> f () in
    Array.init n (fun _ -> r ())
  (* TODO TESTS *)
end (*$>*)

(** {1:ia Immutable Arrays} *)

(** {1 Immutable Arrays}

    These arrays are exactly OCaml's ['a array] type except that we
    use a private type and signature to exclude any functions that
    could mutate them.
*)
module Ia = Ia

(** {1:a1 1-Based Arrays} *)

(** OCaml's [Array] module with 1-based indexing.

    These arrays can help avoid off-by-one errors when translating
    algorithms expressed with 1-based indexing.

    A private row type is used so that you can't accidentally use
    [Array]'s 0-based accessers and setters on them. *)
module A1 = A1

(** {1:generators Generators} *)

(** Trivial generators ("lazy" sequences / streams).  NOT thread-safe.

    An ['a t] {i generator} is just a thunk that, called repeatedly,
    returns [(Some 'a)] for each element of the sequence it represents
    until the sequence is exhausted, when it returns [None].

    Example:

    - [Gen.(catch readline >> to_list) = ] {!readlines}

    Terminology:

    - an {i exhaustive} function (like {!Gen.fold}) is one that calls its
    generator repeatedly until the generator returns [None]

    - a {i lazy} function (like {!map}) is one that returns a new
    generator without calling the original generator at all (or at
    least, without exhausting all of it)

    @see <https://github.com/c-cube/gen> Simon Cruanes's [Gen]
    module for a much more powerful and sophisticated library.

    @see <https://github.com/c-cube/iter> Simon Cruanes's [Iter]
    module for a very powerful {i internal} interator.
*)
module Gen = struct (*$< Gen *)

  (** {1 Types} *)

  (** The type of generators. *)
  type 'a t = unit -> 'a option

  (** [empty] is the empty generator; immediately returns [None]. *)
  let empty = fun () -> None
  (*$= empty
    (empty ()) None
  *)

  (** {1 Functions to Create Generators} *)

  (** [(catch f x)] creates a generator from a function [f] that,
      after repeated application to [x], eventually raises an exception.

      Example: [(catch input_line chan)] is a generator of the lines
      on [chan].

      Any exception will indicate the end of the sequence; if you want
      more precision, specify the exception with [~this] -- in this
      case, any other exception will be re-raised: e.g. [(catch ~this:End_of_file input_line chan)]

  *)
  let catch ?this f x : 'a t = fun () -> try Some (f x) with e -> match this with
    | None              -> None
    | Some t when t = e -> None
    | Some _ (* else *) -> raise e
  (*$T catch
    not (succeeds (catch ~this:End_of_file (fun () -> raise Not_found) ()) ())
  *)
  (*$= catch
    (catch (fun () -> raise End_of_file) () ()) None
    (catch ~this:End_of_file (fun () -> raise End_of_file) () ()) None
    (catch (fun () -> raise Not_found) () ()) None
    (catch ~this:Not_found (fun () -> raise Not_found) () ()) None
  *)

  (** [(optional f x)] creates a generator from a function [f] that,
      after repeated application to [x], eventually returns [None]. *)
  let optional (f : 'a -> 'b option) x : 'b t = fun () -> f x
  (*$= optional
    (optional (k None) () ()) None
    (optional (k (Some 1)) () ()) (Some 1)
  *)

  (** [(cons x g)] is the the generator [g] with [x] at its head.

      {[ (cons 3 empty |> cons 2 |> cons 1 |> to_list) = [1;2;3] ]}
  *)
  let cons x (g : 'a t) : 'a t = let r = ref (Some x) in fun () -> match !r with
    | Some _ as x -> r := None; x | None -> g ()
  (*$= cons
    (cons 1 empty ()) (Some 1)
    ((cons 1 empty |> cons 2) ()) (Some 2)
  *)

  (** [(singleton x)] is the generator producing [x] and only [x]. *)
  let singleton x : 'a t = cons x empty
  (*$Q singleton
    Q.int (fun n -> let g = singleton n in let x = g () in let y = g () in x = Some n && y = None)
   *)

  (** [(append g1 g2)] is the generator that produces all the items in
      [g1] followed by those in [g2].  *)
  let append (g1 : 'a t) (g2 : 'a t) : 'a t = fun () ->
    match g1 () with Some _ as x -> x | None -> g2 ()
  (*$QR append
    Q.(pair (small_list int) (small_list int))
    (fun (xs,ys) ->
      append (of_list xs) (of_list ys) |> to_list = xs @ ys)
   *)

  (** [(to_list list)] is the generator that produces all the elements of [list].

      {[ (of_list [1;2;3] |> to_list) = [1;2;3] ]}
  *)
  let of_list l : 'a t = let r = ref l in fun () -> match !r with | x::xs -> r := xs; Some x | [] -> None
  (*$Q of_list;to_list
    Q.(small_list int) (fun xs -> of_list xs |> to_list = xs)
  *)

  (** [(chars_of_string str)] is the generator that produces all the characters of [str]. *)
  let chars_of_string str =
    let r = ref (Pre.Strings.explode str) in
    fun () -> match !r with [] -> None | c::cs -> r := cs; Some c
  (*$Q chars_of_string
    Q.string (fun s -> chars_of_string s |> to_list |> String.implode = s)
  *)

  (** [(lines chan)] is the generator that produces all the lines on
     the input channel [chan]. *)
  let lines chan : 'a t = catch ~this:End_of_file input_line chan
  (*$= lines
    (within (fun c -> lines c |> to_list) "/dev/null") []
    (within (fun c -> lines c |> to_list) "prelude.ml") (within readlines "prelude.ml")
   *)
  (** [(random ~size elt ())] is a generator of [(size ())] (default: [> 100]) random elements;
      each element is the result of a call to [(elt ())]. *)
  let random ~size elt () = List.random ~size elt () |> of_list
  (* TODO TESTS *)

  (** {1 Exhaustive Functions to Consume Generators} *)

  (** [(fold f acc g)] folds [f] across all the values of the generator [g]; exhaustive. *)
  let rec fold f acc (g : 'a t) = match g () with
    | Some x -> fold f (f x acc) g
    | None   -> acc
  (*$= fold
    (fold List.cons [] empty) []
  *)
  (*$Q fold
    Q.string     (fun s -> chars_of_string s |> fold List.cons [] |> rev |> String.implode = s)
    Q.(list int) (fun xs -> of_list xs |> fold List.cons [] |> rev = xs)
  *)

  (** [(len g)] is the length of the generator [g]; exhaustive. *)
  let len (g : 'a t) = fold (fun _ -> succ) 0 g
  (*$Q len
    Q.(list int) (fun xs -> of_list xs |> len = List.len xs)
   *)

  (** [(iter f g)] iterates [f] across all the values of the generator [g]; exhaustive.*)
  let iter f (g : 'a t) = fold (fun x () -> f x) () g
  (*$QR iter
    Q.(list int)
    (fun xs ->
      let r = ref [] in
        of_list xs |> iter (fun x -> r := x :: !r);
        rev !r = xs)
  *)

  (** [(to_list g)] returns the list of all the values of the generator [g]; exhaustive. *)
  let to_list (g : 'a t) = fold List.cons [] g |> rev
  (* test: see of_list above *)

  (** [(to_string g)] returns the string of all the characters of the generator [g]; exhaustive. *)
  let to_string (g : 'a t) = withbuf 10 (fun b -> iter (Buffer.add_char b) g)
  (*$Q to_string;of_list
    Q.(list char) (fun cs -> to_string (of_list cs) = String.implode cs)
  *)

  (** {1 Functions to Transform Generators} *)

  (** [(map f g)] maps [f] across all the value of the generator [g]; lazy. *)
  let map f (g : 'a t) = fun () -> g () |> Option.map f
  (*$= map
    (map succ empty |> to_list) []
   *)
  (*$Q map
    Q.(small_list int) (fun xs -> of_list xs |> map succ |> to_list = List.map succ xs)
   *)

  (** [(mapi f g)] maps [(fun x -> f i x)] across all the values of the
      generator [g], where [i] is the 0-based index of each value; lazy. *)
  let mapi f (g : 'a t) = let i = ref 0 in fun () -> let i' = !i in i +:= 1; g () |> Option.map (f i')
  (*$= mapi
    (mapi Pair.up empty |> to_list) []
   *)
  (*$Q mapi
    Q.(small_list int) (fun xs -> of_list xs |> mapi Pair.up |> to_list = List.mapi Pair.up xs)
   *)

  (** [(take n g)] is the generator consisting of the first [n] elements of [g]; lazy. *)
  let take n (g : 'a t) : 'a t =
    let rec loop n g' = match g () with
      | Some x when n > 0 -> loop ~--n (append g' (singleton x))
      | Some _ | None     -> g'
    in
    loop n empty
  (*$Q take
    Q.(list int) (fun xs -> of_list xs |> take 0 |> len = 0)
    Q.(list int) (fun xs -> let n = List.len xs in of_list xs |> take n |> len = n)
    Q.(pair pos_int (list int)) (fun (i,xs) -> let n = List.len xs in of_list xs |> take (min i n) |> len = min i n)
   *)

  (** [(drop n g)] is the generator consisting of all the elements of [g], excluding the first [n]; lazy. *)
  let drop n (g : 'a t) : 'a t =
    let rec loop n = if n > 0 then (g () |> ignore; loop ~--n) else g in
    loop n
  (*$Q drop
    Q.(list int) (fun xs -> of_list xs |> drop 0 |> len = List.len xs)
    Q.(list int) (fun xs -> let n = List.len xs in of_list xs |> drop n |> len = 0)
    Q.(pair pos_int (list int)) (fun (i,xs) -> let n = List.len xs in of_list xs |> drop (min i n) |> len = n - (min i n))
   *)

  (** [(splitwhile p g)] is [(a,b)] where [a] is the generator that
      produces the longest prefix of [g] of elements that satisfy [p]
      and [b] is the generator that produces the remaining suffix.

      Exhaustive in the prefix, lazy in the suffix.
  *)
  let splitwhile p (g : 'a t) : 'a t * 'a t =
    let rec loop g' = match g () with
      | Some x when p x   -> loop (append g' (singleton x))
      | Some x (* else *) -> g', cons x g
      | None              -> g', empty
    in
    loop empty
  (*$= splitwhile
    (splitwhile even empty |> Pair.map to_list) ([],[])
   *)
  (*$Q splitwhile
    Q.(small_list int) (fun xs -> of_list xs |> splitwhile even |> Pair.map to_list = Lists.splitwhile even xs)
   *)

  (** [(takewhile p g)] is the generator that produces (only) the
      leading elements of [g] for which [p] is true. *)
  let takewhile p (g : 'a t) : 'a t =
    let each () = match g () with
      | Some x when p x   -> Some x
      | Some x (* else *) -> None
      | None              -> None
    in
    fun () -> each ()
  (*$= takewhile
    (takewhile even empty |> to_list) []
    (takewhile odd empty |> to_list) []
    (takewhile (k true) empty |> to_list) []
    (takewhile (k false) empty |> to_list) []
   *)
  (*$Q takewhile
    Q.(small_list int) (fun xs -> of_list xs |> takewhile even |> to_list = Lists.takewhile even xs)
   *)

  (** [(dropwhile p g)] is the generator that produces (only) the
      suffix of [g] after discarding the leading elements for which
      [p] is true. *)
  let dropwhile p (g : 'a t) : 'a t =
    let rec loop () = match g () with
      | Some x when p x   -> loop ()
      | Some x (* else *) -> cons x g
      | None              -> empty
    in
    loop ()
  (*$= dropwhile
    (dropwhile even empty |> to_list) []
    (dropwhile odd empty |> to_list) []
    (dropwhile (k true) empty |> to_list) []
    (dropwhile (k false) empty |> to_list) []
   *)
  (*$Q dropwhile
    Q.(small_list int)     (fun xs -> of_list xs |> dropwhile even |> to_list = Lists.dropwhile even xs)
   *)
end (*$>*)

(** {1:chars Chars} *)

(** Additional char functions.

    {i N.B.} These functions pertain only to chars as ASCII bytes.

    @see <http://erratique.ch/software/uutf> Daniel Bünzli's excellent
    Unicode modules.
*)
module Chars = struct (*$< Chars *)
  open Char

  (** [to_int] is {!Char.code}. *)
  let to_int = code
  (*$Q to_int
    Q.char (fun c -> to_int c = Char.code c)
   *)

  (** [of_int] is {!Char.chr}. *)
  let of_int = chr
  (*$Q of_int
    Q.(int_bound 255) (fun c -> catch of_int c = catch Char.chr c)
   *)

  (** [(upto a b)] is the list of the chars between [a] and [b]
      inclusive; {i not} tail-recursive.

      - [(upto '0' '9') = ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']]
  *)
  let upto lo hi = foldir (conswith chr) [] (code lo, code hi)
  (*$= upto
    (upto '0' '9') (map Char.(fun n -> to_int '0' + n |> of_int) (List.upto 0 9))
   *)

  (** [(--)] is {!upto}. *)
  let (--) = upto
  (*$Q (--)
    Q.(pair char char) (fun (a,z) -> a -- z = (upto a z))
   *)

  (** {1 Uppercase ASCII Letters} *)
  module Uppercase = struct (*$<Uppercase*)
    (**/**)
    let min = to_int 'A'
    let max = to_int 'Z'
    (**/**)

    (** [(is c)] is [true] iff [c] is an uppercase ASCII letter. *)
    let is = contains (of_int min, of_int max)
    (*$T is
             String.foldr (fun c r -> is c && r) true String.majuscules
      not @@ String.foldr (fun c r -> is c && r) true String.miniscules
    *)
  end (*$>*)

  (** {1 Lowercase ASCII Letters} *)
  module Lowercase = struct (*$<Lowercase*)
    (**/**)
    let min = to_int 'a'
    let max = to_int 'z'
    (**/**)

    (** [(is c)] is [true] iff [c] is an lowercase ASCII letter. *)
    let is = contains (of_int min, of_int max)
    (*$T is
             String.foldr (fun c r -> is c && r) true String.miniscules
      not @@ String.foldr (fun c r -> is c && r) true String.majuscules
    *)
  end (*$>*)

  (** {1 ASCII Alphabetic Characters} *)
  module Alphabetic = struct (*$<Alphabetic*)
    (** [(is c)] is [true] iff [c] is an upper- or lowercase ASCII letter. *)
    let is c = Uppercase.is c || Lowercase.is c
    (*$T is
             String.foldr (fun c r -> is c && r) true String.miniscules
             String.foldr (fun c r -> is c && r) true String.majuscules
             String.foldr (fun c r -> is c && r) true String.alphabet
      not @@ String.foldr (fun c r -> is c && r) true String.digits
    *)
    (*$Q is
      Q.char (fun c -> not (is c) || (Uppercase.is c || Lowercase.is c))
    *)
  end (*$>*)

  (** {1 Decimal (Base 10) Digits} *)
  module Decimal = struct
    (**/**)
    let min = to_int '0'
    let max = to_int '9'
    (**/**)

    (** [(is c)] is [true] iff [c] is an ASCII decimal digit. *)
    let is = contains (of_int min, of_int max)

    (** [(digit i)] is the character representing the the number [i].

        @raise Invalid_argument if [i] is not in the range [[0,9]].
    *)
    let digit i = if contains (0,9) i then min + i |> of_int else invalid_arg "digit"

    (** [(int c)] is the integer represented by the digit character [c].

        @raise Invalid_argument if [c] is not in the range [['0','9']].
    *)
    let int c =
      let c = to_int c in
      if c < min || c > max then invalid_arg "int" else c - min
  end

  (** {1 Octal (Base 8) Digits} *)
  module Octal = struct
    (**/**)
    let min = to_int '0'
    let max = to_int '7'
    (**/**)

    (** [(is c)] is [true] iff [c] is an ASCII octal digit. *)
    let is = contains (of_int min, of_int max)

    (** [(digit i)] is the character representing the the number [i].

        @raise Invalid_argument if [i] is not in the range [[0,7]].
    *)
    let digit i = if contains (0,7) i then min + i |> of_int else invalid_arg "digit"

    (** [(int c)] is the integer represented by the digit character [c].

        @raise Invalid_argument if [c] is not in the range [['0','7']].
    *)
    let int c =
      let c = to_int c in
      if c < min || c > max then invalid_arg "int" else c - min
  end

  (** {1 Hexadecimal (Base 16) Digits} *)
  module Hex = struct
    (**/**)
    let min1 = to_int '0'
    let max1 = to_int '9'
    let min2 = to_int 'A'
    let max2 = to_int 'F'
    (**/**)

    (** [(is c)] is [true] iff [c] is an ASCII hexadecimal digit. *)
    let is c =
      let i = to_int c in contains (min1,max1) i || contains (min2,max2) i

    (** [(digit i)] is the character representing the the number [i].

        @raise Invalid_argument if [i] is not in the range [[0,15]].
    *)
    let digit i =
      if contains (0,9) i
      then min1 + i |> of_int
      else if contains (10,15) i
      then min2 + (i-10) |> of_int
      else invalid_arg "digit"

    (** [(int c)] is the integer represented by the digit character [c].

        @raise Invalid_argument if [c] is not in the range [['0','9']], [['A','F']], or [['a','f']].
    *)
    let int c =
      let c' = to_int c in
      if c' <= Decimal.max
      then c' - Decimal.min
      else match c with
      | 'A' -> 10 | 'B' -> 11 | 'C' -> 12 | 'D' -> 13 | 'E' -> 14 | 'F' -> 15
      | 'a' -> 10 | 'b' -> 11 | 'c' -> 12 | 'd' -> 13 | 'e' -> 14 | 'f' -> 15
      | _   -> invalid_arg "int"
  end

  (** {1 Development Utils} *)

  (** [(to_string c)] is the string containing just the character [c].

      Invariants:
      - [String.len (to_string c) = 1]
      - [String.get (to_string c) 0 = c]

   *)
  let to_string = String.make 1
  (*$Q to_string
    Q.char (fun c -> String.len (to_string c) = 1)
    Q.char (fun c -> String.get (to_string c) 0 = c)
   *)

  (** [print] is [(to_string >> print_endline)]. *)
  let print = to_string >> print_endline

  (**/**)
  let printables = upto ' ' '~' |> Array.of_list
  let allchars   = Array.init 128 of_int
  (**/**)

  (** [(random ?(printable=true) ?charset ())] is a random printable ASCII character in the range [[0x20 -- 0x7E]].

      If [(printable = false)], the result is a random ASCII character in the range [[0x00 -- 0x7E]].

      If [charset] is provided, it trumps [printable] and the characters will be chosen from this list.

      Partially apply for efficiency.

      @raise Invalid_argument if [(Some charset)] is empty.
   *)
  let random ?(printable=true) ?charset =
    let charset = match charset with
      | Some cs -> Array.of_list cs
      | None    -> if printable then printables else allchars
    in
    let n = Array.len charset in
    fun () -> charset.(Random.int n)
  (*$Q random
    Q.unit (fun () -> random                  () |> to_int |> Interval.Int.contains (0x20,0x7E))
    Q.unit (fun () -> random ~printable:true  () |> to_int |> Interval.Int.contains (0x20,0x7E))
    Q.unit (fun () -> random ~printable:false () |> to_int |> Interval.Int.contains (0x00,0x7F))
    Q.char (fun c  -> random ~charset:(upto c c) () = c)
    Q.unit (fun () -> let charset = upto '0' '9' in random ~charset () |> one_of charset)
    Q.unit (fun () -> not @@ succeeds (random ~charset:[]) ())
   *)

  (** {2 Ops} *)

  (** {1 Infix and prefix operators} *)
  module Ops = struct
    (** [(--)] is {!(--)}. *)
    let (--) = (--)
  end
  (*$Q (--)
    Q.(pair char char) (fun (a,z) -> a -- z = Ops.(a -- z))
   *)
end (*$>*)

(** This is Ocaml's standard [Char] with the additional functions from {!Chars}. *)
module Char = struct
  (** @closed *)
  include Char

  (** @closed *)
  include Chars
end

(** {1:format Format} *)

(** This is Ocaml's standard [Format] with some additional functions and values. *)
module Format = struct
  (** @closed *)
  include Format

  (** [stdout] is [Format.std_formatter] i.e. output goes to [stdout]. *)
  let stdout = std_formatter

  (** [stderr] is [Format.err_formatter] i.e. output goes to [stderr]. *)
  let stderr = err_formatter

  (** [null] is the /dev/null of formatters; no output. *)
  let null = make_formatter (fun _ _ _ -> ()) ignore

  (** [(of_chan chan)] is [formatter_of_out_channel]; it returns a new formatter writing to [chan]. *)
  let of_chan c = formatter_of_out_channel c

  (** [(of_buffer buf)] is [formatter_of_buffer]; it returns a new
      formatter writing to [buf].  At the end of pretty-printing, the
      formatter must be flushed using [Format.pp_print_flush] or
      [Format.pp_print_newline], to print all the pending material into the
      buffer. *)
  let of_buffer b = formatter_of_buffer b
end

(** {1:strings Strings} *)

(** Additional string functions.*)
module Strings = struct (*$< Strings *)
  (*$inject open String *)
  open String

  (** {1:strings_basic Basic Functions} *)

  (** [len] = [String.length]. *)
  let len = String.length
  (*$Q len
    Q.string (fun s -> len s = String.length s)
   *)

  (** [(to_string s)] = [s]. *)
  let to_string s = s

  (* TODO COMPARE implementations; author: "sanette" *)
  (* let rev3 x =
   *   let len = String.length x in
   *   let y = Bytes.of_string x in
   *   for i = 0 to len - 1 do
   *     Bytes.set y i (String.get x (len - i - 1))
   *   done;
   *   Bytes.to_string y *)

  (** [(rev str)] is the reverse of [str].

      Invariant:
      - [rev = (explode >> List.rev >> implode)]
   *)
  let rev str =
    let rec fold buf i = if i < 0 then () else (Buffer.add_char buf str.[i]; fold buf ~--i) in
    let n = len str in
    withbuf n (fun buf -> fold buf ~--n)
  (*$Q rev
    Q.string (fun s -> rev s = (explode $ List.rev $ implode) s)
   *)

  (** [(random ?(printable=true) ?charset ?size ())] returns a random string of length
      [(size ())] (default: [< 64]).  Each of the characters in the string is the
      result of calling ({!Char.random} [?printable ()] [?printable ?charset ()]).
   *)
  let random ?printable ?charset ?size () =
    let n = match size with None -> Random.int 64 | Some f -> f () in
    make n 'x' |> map (fun _ -> Char.random ?printable ?charset ())
  (*$Q random
    Q.unit (fun () -> random                  () |> len < 64)
    Q.unit (fun () -> random ~printable:true  () |> len < 64)
    Q.unit (fun () -> random ~printable:false () |> len < 64)

    Q.unit (fun () -> random                  () |> explode |> List.all Chars.(to_int >> Interval.Int.contains (0x20,0x7E)))
    Q.unit (fun () -> random ~printable:true  () |> explode |> List.all Chars.(to_int >> Interval.Int.contains (0x20,0x7E)))
    Q.unit (fun () -> random ~printable:false () |> explode |> List.all Chars.(to_int >> Interval.Int.contains (0x00,0x7F)))

    Q.(int_bound 50) (fun n -> random                  ~size:(k n) () |> len <= n)
    Q.(int_bound 50) (fun n -> random ~printable:true  ~size:(k n) () |> len <= n)
    Q.(int_bound 50) (fun n -> random ~printable:false ~size:(k n) () |> len <= n)

    Q.(int_bound 50) (fun n -> random                  ~size:(k n) () |> explode |> List.all Chars.(to_int >> Interval.Int.contains (0x20,0x7E)))
    Q.(int_bound 50) (fun n -> random ~printable:true  ~size:(k n) () |> explode |> List.all Chars.(to_int >> Interval.Int.contains (0x20,0x7E)))
    Q.(int_bound 50) (fun n -> random ~printable:false ~size:(k n) () |> explode |> List.all Chars.(to_int >> Interval.Int.contains (0x00,0x7F)))
   *)

  (** {1:strings_indexing Indexing and Slicing}

      {!slice} and the infix operators {!(#.)} and {!(#!)} do string
      indexing and slicing modeled on Python.  There is currently no
      support for "steps".

      Negative indexes are as in Python.  Indexes "missing" in
      Python's slice notation are replaced with [0].

      Indexes that are out-of-range are treated as the max or min
      string index as appropriate, so no exceptions are ever raised,
      e.g.:

      {[ (slice "" (0,100)) = "" ]}

      Analogues:
      - Python: ["str"[i]]   = Ocaml: ["str"#!i]
      - Python: ["str"[i:j]] = Ocaml: ["str"#.(i,j)]
      - Python: ["str"[:j]]  = Ocaml: ["str"#.(0,j)]
      - Python: ["str"[i:]]  = Ocaml: ["str"#.(i,0)]
      - Python: ["str"[:]]   = Ocaml: ["str"#.(0,0)]
  *)

  (**/**)
  let pyget str i = if i < 0 then str.[length str + i] else str.[i]
  (**/**)
  (** [(str #! i)] is like [(str.[i])] with Python-like support for negative indexes.

      @raise Invalid_argument if the string is empty.
   *)
  let (#!) = pyget
  (*$Q pyget
    Q.(pair pos_int string) (fun (i,s) -> catch (pyget s) i = catch (String.get s) i)
    Q.(pair neg_int string) (fun (i,s) -> i = 0 || catch (pyget s) i = catch String.(get (rev s)) (abs @@ succ i))
   *)

  (** [(slice str (l,u))] is Python string slicing.

      Example: [ (slice "0123456789" (2,4)) = "23" ] *)
  let slice str (l,u) =
    let len = length str in
    let l = if l <  0 then length str + l else l in
    let u = if u <= 0 then length str + u else min len u in
    if str = "" then "" else if l < len then sub str l (u-l) else ""
  (*$Q slice
    Q.small_string (fun s -> foldir (fun i r -> slice s (i,len s) :: r) [] (0,len s) = (explode s |> List.rev |> prefixes |> List.rev |> List.(map (rev $ implode))))
    Q.string (fun s -> len s < 1 || let i = Random.int (len s) in slice s (i,i+1) = String.make 1 (pyget s i))
    Q.small_string (fun s -> foldir (fun i r -> slice s (-i,0) = rev @@ slice (rev s) (0,i)) true (0,len s))
   *)

  (** [(str #. (i,j))] is [(slice str (i,j))]. *)
  let (#.) = slice

  (** {1:strings_predicates Predicates} *)

  (** [(eq a b)] is type-specialized and inlined string equality. *)
  let [@inline] eq (a:string) (b:string) : bool = compare a b = 0
  (*$Q eq
    Q.(pair string string) (fun (a,b) -> eq a b = (compare a b = 0))
   *)

  (** [null] is [((=) "")]. *)
  let null str = str = ""
  (*$Q null
    Q.string (fun s -> null s = (s = ""))
   *)

  (** [(any p str)] is [true] iff [(p x)] for any of the characters of [str].

      Invariant: [(any p str) = (explode str |> map p |> ored)]
  *)
  let any p = Pre.Strings.foldl (fun t x -> t || p x) false
  (*$T any
    any ((=) ' ') " xxxx"
    any ((=) ' ') "xxxx "
    any ((=) ' ') "xx xx"
    any ((=) ' ') "xx   xx"
    any ((=) ' ') "x x x x"
    any ((=) ' ') "   xxxx   "
    not (any ((=) ' ') "djhgdkhgdkhgdkhg")
    not (any (k true)  "")
    not (any ((=) ' ') "")
    not (any (k false) "")
  *)

  (** [(all p str)] is [true] iff [(p c)] for all of the characters of [str].

      Invariant: [(all p str) = (explode str |> map p |> anded)]
  *)
  let all p = Pre.Strings.foldl (fun t x -> t && p x) true
  (*$T all
    all (k true) ""
    all (k false) ""
    all ((=) ' ') ""
    all ((=) ' ') "    "
    not (all ((=) ' ') "  x  ")
    not (all ((=) ' ') "  x")
    not (all ((=) ' ') "x  ")
    not (all ((=) ' ') "x")
    not (all ((=) ' ') "xdjhgdhgfd")
   *)
  (*$Q all
    Q.small_string (fun s -> let p = k true in all p s = (explode s |> List.map p |> anded))
    Q.small_string (fun s -> let p = k false in all p s = (explode s |> List.map p |> anded))
    Q.small_string (fun s -> let p = (<>) ' ' in all p s = (explode s |> List.map p |> anded))
   *)

  (** [mem] is [(flip String.contains)]. *)
  let mem = flip contains
  (*$Q mem
    Q.(pair char string) (fun (c,s) -> mem c s = String.contains s c)
   *)

  (** [(only ~these str)] is [true] iff [str] consists of only characters in [these]; tail recursive.

      Partially apply for efficiency. *)
  let only ~these =
    let module S = Set.Make (Char) in
    let set = Pre.Strings.explode these |> S.of_list in
    fun str -> match these, str with
    | "", "" -> true
    | "", _  -> false
    | _,  "" -> false
    | _,  _  -> all (flip S.mem set) str
  (*$Q only
    Q.unit (fun () -> only ~these:"" "")
    Q.string (fun s -> s = "" || not (only ~these:"" s))
    Q.small_string (fun these -> these = "" || not (only ~these ""))
    Q.small_string (fun these -> only ~these these)
    Q.small_string (fun these -> only ~these (rev these))
   *)

  (** [(anyof ~these str)] is [true] iff [str] contains any of the
      characters in [these]; tail recursive.

      Partially apply for efficiency. *)
  let anyof ~these =
    let module S = Set.Make (Char) in
    let set = Pre.Strings.explode these |> S.of_list in
    fun str -> Pre.Strings.foldl (fun r c -> S.mem c set || r) false str
  (*$Q anyof
    Q.string (fun s -> anyof ~these:"" s |> not)
    Q.small_string (fun these -> anyof ~these "" |> not)
    Q.small_string (fun these -> these = "" || anyof ~these these)
    Q.(triple small_string small_string small_string) (fun (these,a,b) -> these = "" || anyof ~these (a^these^b))
    Q.(pair small_string string) (fun (these,s) -> these = "" || anyof ~these s = (explode these |> List.map mem |> flip disjunction s))
   *)

  (** [(allof ~these str)] is [true] iff every character in [these] is contained in [str]; tail recursive.

      Partially apply for efficiency. *)
  let allof ~these str = only ~these:str these
  (* TODO TESTS *)

  (** [(prefix s str)] is [true] iff [s] is a prefix of [str].

      Invariants:
      - [(prefix "" s) = true]
      - [(prefix s s) = true]
  *)
  let prefix s str = s = "" || slice str (0,len s) = s
  (*$Q prefix
    Q.small_string (fun s -> explode s |> prefixes |> List.map implode |> List.all (flip prefix s))
    Q.string (fun s -> prefix "" s)
    Q.string (fun s -> prefix s s)
    Q.(pair char string) (fun (c,s) -> not @@ prefix ((String.make 1 c) ^ s) s)
   *)

  (** [(suffix s str)] is [true] iff [s] is a suffix of [str].

      Invariants:
      - [(suffix "" s) = true]
      - [(suffix s s) = true]
  *)
  let suffix s str =
    let ls, lstr = len s, len str in
    if ls > lstr
    then false
    else s = "" || s = sub str (lstr - ls) ls
  (*$Q suffix
    Q.small_string (fun s -> explode s |> suffixes |> List.map implode |> List.all (flip suffix s))
    Q.string (fun s -> suffix "" s)
    Q.string (fun s -> suffix s s)
    Q.(pair char string) (fun (c,s) -> not @@ suffix ((String.make 1 c) ^ s) s)
   *)

  (** {1:strings_substrings Substrings} *)

  (** [(substr sub str)] is [(Some i)] where [i] is the index of the
      first occurrence of [sub] in [str], or [None] if [sub] does not
      occur in [str].

      Examples:
      - [(substr "x" "") = None]
      - [(substr "x" "x") = Some 0]
      - [(substr "x" "xs") = Some 0]
      - [(substr "x" "zxs") = Some 1]
      - [(substr "x" "zs") = None]
  *)
  let substr sub str =
    let len = length str in
    let n = length sub in
    let rec loop i =
      if i+n > len
      then None
      else if String.sub str i n = sub
      then Some i
      else loop ~++i
    in
    loop 0
  (*$Q substr
    Q.string (fun s -> substr "" s = Some 0)
    Q.string (fun s -> substr s s = Some 0)
    Q.(pair string string) (fun (s,t) -> s = "" || t = "" || substr (s^t) s = None)
   *)
  (*$T substr
    let s = alphabet in foldil (fun r i -> r && substr (drop i s) s = Some i) true (0,len s - 1)
   *)

  (** [(take n str)] is the prefix of [str] of length [n], or [str] itself if [(n < 0 || n > len str)].*)
  let take n str =
    let len = String.length str in
    if n < 0 || n > len then str else sub str 0 n
  (*$Q take;drop
    Q.string (fun s -> foldil (fun r i -> take i s = (drop (len s-i) (rev s) |> rev)) true (0,len s))
   *)
  (*$Q take
    Q.small_string (fun s -> foldil (fun r i -> take i s :: r) [] (0,len s) = (explode $ prefixes $ List.rev $ List.map implode) s)
   *)

  (** [(drop n str)] is the suffix of [str] after the first [n] elements, or [] if [(n < 0 || n > len str)].*)
  let drop n str =
    let len = length str in
    if n < 0 || n > len then "" else sub str n (length str - n)
  (*$Q drop
     Q.small_string (fun s -> foldil (fun r i -> drop i s :: r) [] (0,len s) = (explode $ suffixes $ List.map implode) s)
   *)

  (** [(splitat n str)] is [(a,b)] such that [a] is the [(min n (len str))]-byte prefix of [str],
      and [b] is the remainder of the string.

      {i N.B.} if [n > (len str)] then [(len @@ fst @@ splitat n str = len str)].

      [(splitat n str)] is equivalent to [(take n str, drop n str)]. *)
  let splitat n str = take n str, drop n str

  (** [(takeall n str)] returns the sequential substrings of length [n] in [str].

      - [(takeall 3 digits) = ["012"; "345"; "678"; "9"]]

      Invariant: [(takeall n str |> String.concat "") = str]

      @raise Invalid_argument if [n] < 1
  *)
  let takeall n str =
    let rec loop acc str' = match splitat n str' with
    | a,"" -> List.rev (a::acc)
    | a,b  -> loop (a :: acc) b
    in
    if n < 1 then invalid_arg "takeall" else loop [] str
  (*$T takeall
    not @@ succeeds (takeall 0) ""
   *)
  (*$Q takeall
    Q.string (fun s -> foldil (fun r i -> r && takeall i s |> String.concat "" = s) true (1,len s))
   *)

  (** [(takewhile p str)] is the leading string of chars of [str] for which [p] is true.

      - [(takewhile (contains digits) "7654abc123") = "7654"]
      - [(takewhile (contains digits) "x7654abc") = ""]
  *)
  let takewhile p str =
    let n = len str in
    let rec loop i = if i < n && p str.[i] then loop ~++i else i in
    let i = loop 0 in
    if i = 0 then "" else slice str (0,i)
  (*$Q takewhile;dropwhile
    Q.string (fun s -> List.map (fun p -> (takewhile p s ^ dropwhile p s) = s) [k true; k false; String.contains digits; (=) 'x'] |> List.all id)
   *)

  (** [(dropwhile p str)] is [str] trimmed of leading chars for which [p] is true.

      - [(dropwhile (contains digits) "7654abc") = "abc"]
      - [(dropwhile (contains digits) "x7654abc") =  "x7654abc"]
  *)
  let dropwhile p str =
    let n = len str in
    let rec loop i = if i < n && p str.[i] then loop ~++i else i in
    slice str (loop 0,0)
  (* test: see takewhile above *)

  (** [(splitwhile p str)] is [(takewhile p str, dropwhile p str)]. *)
  let splitwhile p str =
    let n = len str in
    let rec loop i = if i < n && p str.[i] then loop ~++i else i in
    let i = loop 0 in
    if i = 0 then "", str else slice str (0,i), slice str (i,0)
  (*$Q splitwhile
    Q.string (fun s -> List.map (fun p -> splitwhile p s |> uncurry (^) = s) [k true; k false; String.contains digits; (=) 'x'] |> List.all id)
   *)

  (** {1:strings_folding Folding Over Strings} *)

  (** [(foldl f acc s)] is a left-fold over a string;
      [(f (...(f (f acc s.[0]) s.[1])...) s.[m])] with [m = String.length s - 1].

      Tail-recursive. *)
  let foldl = Pre.Strings.foldl
  (*$Q foldl
    Q.string (fun s -> foldl snoc [] s = Pre.Strings.foldl snoc [] s)
  *)

  (** [(foldr f acc s)] is a right-fold over a string;
      [(f s.[0] (f s.[1] (...(f s.[m] acc) )...))] with [m = String.length s - 1].

      {i Not} tail-recursive.*)
  let foldr f a str =
    let rec fold i acc = if i = len str then acc else f str.[i] (fold (i+1) acc) in
    fold 0 a
  (*$Q foldr
    Q.string (fun s -> foldr cons [] s = explode s)
    Q.string (fun s -> foldr (fun _ n -> succ n) 0 s = String.length s)
  *)

  (** [(maps f str)] is like {!String.map} except [f] returns a [string] instead of a [char]. *)
  let maps f str = withbuf (len str) (fun buf -> iter (fun c -> Buffer.add_string buf (f c)) str)

  (** [(filter p str)] is all the characters of the string [str] that satisfy the predicate p.

      The order of the characters in the input string is preserved.
      @author Matt Teichman *)
  let filter pred str =
    withbuf (length str) @@ fun b ->
      iter (iff pred (Buffer.add_char b) (k ())) str

  (** {1:strings_iterating Iterating Over Strings} *)

  (** [(iter f str)] calls [(f c)] for each character [c] in [str] (for side-effects). *)
  let iter = String.iter
  (*$Q iter
    Q.string (fun s -> let r = ref 0 in iter (fun _ -> incr r) s; !r = String.length s)
    Q.int (fun n -> let r = ref n in iter (fun _ -> r := n+1) ""; !r = n)
  *)

  (** [(iteri f str)] calls [(f i c)] for each character [c] and
      zero-based index [i] in [str] (for side-effects). *)
  let iteri = String.iteri
  (*$Q iteri
    Q.string (fun s -> let r = ref 0 in iteri (fun i _ -> r +:= ~++i) s; !r = let m = String.length s in m * (m+1) / 2)
    Q.int (fun n -> let r = ref n in iteri (fun _ _ -> r := n+1) ""; !r = n)
  *)

  (** {1:strings_charlists Strings and Character Lists} *)

  (** Infix version of {!Chars.upto}. *)
  let (--) = Char.upto
  (*$Q (--)
    Q.(pair char char) (fun (a,z) -> a -- z = (Char.upto a z))
   *)

  (** [(explode s)] is the list of characters comprising the string [s].

      Example: [ explode "abc" = ['a'; 'b'; 'c'] ]
  *)
  let explode = Pre.Strings.explode
  (*$Q explode
    Q.string (fun s -> explode s = Pre.Strings.explode s)
   *)

  (** [to_list] is {!explode}. *)
  let to_list = explode
  (*$Q to_list
    Q.string (fun s -> explode s = to_list s)
  *)

  (** [implode] is the inverse of {!explode}.

      Example: [(explode $ implode) "abc" = "abc"]
  *)
  let implode cs = withbuf (List.len cs) (fun b -> List.iter (Buffer.add_char b) cs)
  (* above is 25% faster than: let implode1 list = String.of_seq (List.to_seq list) *)
  (*$Q implode;explode
    Q.string (fun s -> (explode $ implode) s = s)
  *)

  (** [of_list] is {!implode}. *)
  let of_list = implode
  (*$Q of_list;to_list
    Q.string (fun s -> (to_list $ of_list) s = s)
  *)

  (** {1:strings_chararrays Strings and Character Arrays} *)

  (** [(of_array a)] is the string consisting of all the characters of [a]. *)
  let of_array a = withbuf (Array.len a) (fun b -> Array.iter Buffer.(add_char b) a)
  (* test: see to_array below *)

  (** [(to_array s)] is the array consisting of all the characters of [s]. *)
  let to_array s = Array.init (len s) (get s)
  (*$Q of_array;to_array
    Q.(array char) (fun a -> of_array a |> to_array = a)
    Q.string       (fun s -> to_array s |> of_array = s)
  *)
  (*$Q to_array
    Q.string (fun s -> (Array.length (to_array s)) = len s)
  *)

  (** {1:strings_predefined Predefined Strings}

      All the characters in these predefined strings occur in lexicographic order.
  *)
  (*$inject
    open Ctypes
    open Foreign
    let isspace = foreign "isspace" (char @-> returning int)
    let isdigit = foreign "isdigit" (char @-> returning int)
    let isalpha = foreign "isalpha" (char @-> returning int)
  *)

  (** [whitespace] is a string consisting of the ASCII whitespace characters:

      ["\t\n\x0B\x0C\r "]
  *)
  let whitespace = "\t\n\x0B\x0C\r "
  (*$T whitespace
    not (foldl (fun r c -> r && isspace c <> 0) true (whitespace ^ "x"))
    to_list whitespace |> sorted Char.compare
  *)
  (*$T whitespace
    foldl (fun r c -> r && isspace c <> 0) true whitespace
  *)

  (** [digits] is a string consisting of the ASCII decimal digits:

      ["0123456789"]
  *)
  (* I prefer this code to a hard-wired string that may have or someday acquire a typo. *)
  let digits = Char.upto '0' '9' |> implode
  (*$T digits
    foldl (fun r c -> r && isdigit c <> 0) true digits
    to_list digits |> sorted Char.compare
  *)

  (** [alphabet] is a string consisting of the lowercase ASCII alphabetic characters:

      ["abcdefghijklmnopqrstuvwxyz"]
  *)
  (* I prefer this code to a hard-wired string that may have or someday acquire a typo. *)
  let alphabet = Char.upto 'a' 'z' |> implode
  (*$T alphabet
    foldl (fun r c -> r && isalpha c <> 0) true alphabet
    to_list alphabet |> sorted Char.compare
  *)

  (** [miniscules] is [alphabet]. *)
  let miniscules = alphabet
  (*$T miniscules
    to_list miniscules |> sorted Char.compare
  *)
  (* test see majuscules below *)

  (** [majuscules] is [(uppercase_ascii alphabet)]:

      ["ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
  *)
  let majuscules = uppercase_ascii alphabet
  (*$T majuscules
    to_list majuscules |> sorted Char.compare
  *)
  (*$= miniscules;majuscules
    miniscules (lowercase_ascii majuscules)
    majuscules (uppercase_ascii miniscules)
  *)

  (** {1:strings_splitting Splitting and Joining} *)

  (** [(split ?elide ?complement ?sep str)] is the list of (possibly
      empty) substrings that are delimited by any of the set of chars
      contained in the string [sep] (default: {!whitespace}).  If
      [complement] is [true] (default: [false]), the separator
      characters are any that are {i not} contained in [sep].  Empty
      substrings are omitted in the list if [elide] is [true] (the
      default).

      The [sep] string is treated as US-ASCII.

      {[ (split "foo \t bar\nbaz") = ["foo"; "bar"; "baz"] ]}
      {[ (split ~complement:true ~sep:String.(miniscules ^ majuscules) "foo BAR1765 isn't") = ["foo"; "BAR"; "isn"; "t"] ]}

      [(split ~elide:false ~sep str)] is the parser for lines in
      simple delimited file formats such as [/etc/passwd]'s
      [:]-delimited format, tab-delimited formats, etc.

      {[ (split ~elide:false ~sep:":" ":foo:bar::baz:") = [""; "foo"; "bar"; ""; "baz"; ""] ]}
      {[ (split ~elide:true  ~sep:":" ":foo:bar::baz:") = ["foo"; "bar"; "baz"] ]}
  *)
  let split ?(elide=true) ?(complement=false) ?(sep=whitespace) str =
    let splitting = if complement then not $. contains sep else contains sep in
    let buf = Buffer.create 64 in
    let rec loop acc i = match str.[i] with
      | c when splitting c ->
         let w = Buffer.contents buf in
         Buffer.reset buf;
         if elide && w="" then loop acc ~++i else loop (w::acc) ~++i
      | c (* otherwise *) -> Buffer.add_char buf c; loop acc ~++i
      | exception Invalid_argument _ ->
         let w = Buffer.contents buf in
         if elide && w="" then List.rev acc else List.rev (w::acc)
    in
    loop [] 0
  (*$= split
    (split "foo \t bar\nbaz")                      ["foo"; "bar"; "baz"]
    (split ~elide:false ~sep:":" ":foo:bar::baz:") [""; "foo"; "bar"; ""; "baz"; ""]
    (split ~elide:true  ~sep:":" ":foo:bar::baz:") ["foo"; "bar"; "baz"]
    (split "") []
    (split ~elide:true "") []
    (split ~elide:false "") [""]
  *)
  (*$Q split
    Q.string (fun s -> split s = split ~elide:true s)
    Q.string (fun s -> split ~sep:s s = [])
    Q.string (fun s -> split ~sep:whitespace s = split s)
  *)

  (** [(join ?(elide=false) ?sep list)] is [String.concat sep]; the default [sep] is [" "].

      If [(elide = true)], empty strings in [list] are ignored (requires an additional pass).
   *)
  let join ?(elide=false) ?(sep=" ") ss =
    if elide
    then List.filter (not << null) ss |> String.concat sep
    else String.concat sep ss
  (*$Q join
    Q.(pair char string) (fun (c,s) -> (split ~elide:false ~sep:(String.make 1 c) $ join ~sep:(String.make 1 c)) s = s)
    Q.(pair string (list string)) (fun (sep,xs) -> join ~sep xs = String.concat sep xs)
    Q.(list string)               (fun xs -> join xs = String.concat " " xs)
    Q.small_int                   (fun n -> List.make n (k "") |> join ~elide:true = "")
    Q.small_int                   (fun n -> n = 0 || List.make n (k "") |> join ~elide:false = String.make ~--n ' ')
  *)

  (** [(cut ~sep str)] divides [str] into two parts [(left, Some right)]
      which are delimited by the leftmost occurrence of [sep].
      If there is no occurrence of [sep] in [str], then the result is
      [(str, None)].

      Examples:
      - [(cut ~sep:"--" "")     = ("", None)]
      - [(cut ~sep:"--" "x")    = ("x",None)]
      - [(cut ~sep:"--" "x--")  = ("x",Some "")]
      - [(cut ~sep:"--" "x--y") = ("x",Some "y")]
      @raise Invalid_argument if [~sep] is [""].
  *)
  let cut ~sep s =
    if sep = ""
    then invalid_arg "cut"
    else
    let n = len s in
    match substr sep s with
    | None   -> s, None
    | Some i ->
        let b = Buffer.create (n) (* TODO *) in
        let rec loop i =
          Buffer.(if i < n then (add_char b s.[i]; loop ~++i) else contents b)
        in
        sub s 0 i, Some (loop (i+len sep))
  (*$Q cut
    Q.(pair small_string string) (fun (sep,s) -> sep = "" || cut ~sep (sep^s) = ("", Some s))
    Q.small_string (fun s -> s = "" || cut ~sep:s s = ("", Some ""))
    Q.(pair small_string string) (fun (sep,s) -> sep = "" || Option.something (substr sep s) || cut ~sep s = (s,None))
    Q.(pair small_string string) (fun (sep,s) -> sep = "" || match cut ~sep s with l,None -> l=s | l,Some r -> l^sep^r = s)
    Q.string (fun s -> foldil (fun r i -> r && succeeds (cut ~sep:(sub s i 1)) s) true (0,len s - 1))
    Q.small_string (fun s -> foldil (fun r i -> foldil (fun r j -> succeeds (cut ~sep:(sub s i j)) s) r (i,len s - i)) true (0,len s - 1))
  *)

  (** [cuts ~sep str] is the list of substrings of [str] that are
      delimited by occurrences of [sep].

      Note the differences between these:
      - [(cuts               ~sep:"--" "1--2---3") = ["1"; "2"; "-3"]]
      - [(split ~elide:false ~sep:"--" "1--2---3") = ["1"; ""; "2"; ""; ""; "3"]]
      - [(split ~elide:false ~sep:"-"  "1--2---3") = ["1"; ""; "2"; ""; ""; "3"]]
      - [(split ~elide:true  ~sep:"--" "1--2---3") = ["1"; "2"; "3"]]
      - [(split ~elide:true  ~sep:"-"  "1--2---3") = ["1"; "2"; "3"]]
      @raise Invalid_argument if [~sep] is [""].
  *)
  let cuts ~sep str =
    let rec loop = function
      | xs,(x,None)    -> List.rev (x::xs)
      | xs,(x,Some "") -> List.rev (""::x::xs)
      | xs,(x,Some s)  -> loop (x::xs, cut ~sep s)
    in
    loop ([],cut ~sep str)
  (*$Q cuts
    Q.(pair string string) (fun (sep,s) -> sep = "" || cuts ~sep s |> join ~sep = s)
    Q.(pair string string) (fun (sep,s) -> sep = "" || match substr sep s with None -> cuts ~sep s |> hd = s | Some _ -> cuts ~sep s |> List.len > 1)
  *)

  (** {1:strings_modifying Modifying and Formatting Strings} *)

  (** [(replace subj rep str)] replaces all occurences of [subj] in [str] with [rep].

      {[ (replace subj rep) = (cuts ~sep:subj $ concat rep) ]}
      @raise Invalid_argument if [subj] is [""].
  *)
  let replace sep rep = cuts ~sep $ concat rep
  (*$T replace
    let xs = List.(upto 1 10 |> map string_of_int) in zip (repeat 10 ":") xs |> List.map (fun (a,b) -> a^b) |> String.concat "" |> replace ":" "" = concat "" xs
  *)
  (*$Q replace
    Q.(pair small_string string) (fun (sep,s) -> sep = "" || replace sep "" s |> substr sep = None)
  *)

  (** [(pad ?(left=false) ?(def=' ') n str)] pads [xs] on the right (or on
      the left, if [(left = true)]) with enough instances of [def] such that
      the length of the result is at least [n].

      If you need the result list to be {i exactly} of length [n] (never longer), use:
      - [(take n $ pad ~def n)]

      Invariant: [(len (pad ~left ~def n xs) = max n (len xs))]
   *)
  let pad ?(left=false) ?(def=' ') n str = explode str |> List.pad ~left ~def n |> implode (* TODO OPTIMIZE *)
  (*$Q pad
     Q.string (fun str -> pad 0 str = str)
     Q.small_int             (fun      n  -> n < 0 || pad      n "" = String.make n ' ')
     Q.(pair char small_int) (fun (def,n) -> n < 0 || pad ~def n "" = String.make n def)
     Q.(pair char small_int) (fun (def,n) -> n < 0 || pad ~def n "" = String.make n def)
     Q.(pair char small_int)          (fun (def,n)    -> len (pad                  n "") = max 0 n)
     Q.(pair char small_int)          (fun (def,n)    -> len (pad             ~def n "") = max 0 n)
     Q.(triple char small_nat string) (fun (def,n,xs) -> len (pad             ~def n xs) = max (len xs) n)
     Q.(triple char small_nat string) (fun (def,n,xs) -> len (pad ~left:false ~def n xs) = max (len xs) n)
     Q.(triple char small_nat string) (fun (def,n,xs) -> len (pad ~left:true  ~def n xs) = max (len xs) n)
     Q.(pair char string) (fun (def,xs) -> pad ~def 0 xs = xs)
     Q.small_nat (fun n -> let xs = pad            ~def:'0' (n*2) (String.make n '1') in drop n xs = String.make n '0' && take n xs = String.make n '1')
     Q.small_nat (fun n -> let xs = pad ~left:true ~def:'0' (n*2) (String.make n '1') in take n xs = String.make n '0' && drop n xs = String.make n '1')
   *)

  (** [(translate xs ys str)] transliterate the characters in [str]
      which appear in [xs] to the corresponding character in [ys].

      If [(len ys < len xs)], [ys] will be padded on the right with
      [ys#!(-1)].  For example:
      - [(translate majuscules "!" "FooBar" = "!oo!ar")]

      If [(xs = "")], the returned value is [str].  Raises [Failure]
      if [(len ys = 0 || len xs < len ys)].

      Example:
      - [(translate miniscules majuscules "foobar") = "FOOBAR"]
  *)
  let translate xs ys str =
    if xs = ""
    then str
    else if len ys = 0 || len xs < len ys
    then failwith "translate"
    else let ys = if len xs > len ys then pad ~def:(ys#!(-1)) (len xs) ys else ys in
         let open Buffer in
         let buf = create (len str) in
         let each i c = match String.index xs c with
           | exception Not_found -> add_char buf c
           | j                   -> add_char buf ys.[j]
         in
         iteri each str; contents buf
  (*$Q translate
    Q.string (fun str -> translate "" "" str = str)
    Q.(pair small_string string) (fun (ys,str) -> translate "" ys str = str)
    Q.string (fun s -> translate miniscules majuscules s = uppercase_ascii s)
    Q.string (fun s -> translate majuscules miniscules s = lowercase_ascii s)
    Q.numeral_string (let xs = String.make (len digits) 'X' in fun s -> translate digits xs s |> foldl (fun r c -> r && not @@ contains digits c) true)
    Q.numeral_string (fun ds -> translate majuscules miniscules ds = ds)
    Q.(pair char string) (fun (c,str) -> mem c majuscules || translate majuscules (String.make 1 c) str |> anyof ~these:majuscules |> not)
  *)

  (** [(prepend prefix str)] is [(prefix ^ str)].*)
  let prepend prefix str = prefix ^ str
  (*$Q prepend
    Q.(pair string string) (fun (a,b) -> prepend a b = a^b)
    Q.(pair string string) (fun (a,b) -> prepend a b |> prefix a)
    Q.(pair string string) (fun (a,b) -> prepend a b |> suffix b)
   *)

  (** [(postpend suffix str)] is [(str ^ suffix)].*)
  let postpend suffix str = str ^ suffix
  (*$Q postpend
    Q.(pair string string) (fun (a,b) -> postpend b a = a^b)
    Q.(pair string string) (fun (a,b) -> postpend b a |> prefix a)
    Q.(pair string string) (fun (a,b) -> postpend b a |> suffix b)
   *)

  (** @deprecated Poor name choice; use {!postpend}. *)
  let append[@deprecated] = postpend

  (** [(trimleft cs)] is [(dropwhile (contains cs))]. *)
  let trimleft cs = dropwhile (contains cs)
  (*$= trimleft
    (trimleft "" "") ""
   *)
  (*$Q trimleft
    Q.(pair small_string string) (fun (cs,s) -> let s' = trimleft cs s in trimleft cs s' = s')
    Q.(int_bound (len alphabet)) (fun n -> n = 0 || trimleft (slice alphabet (0,n)) alphabet = slice alphabet (n,0))
    Q.string (fun s -> trimleft "" s = s)
    Q.string (fun s -> trimleft s s = "")
    Q.string (fun s -> trimleft (explode s |> nub |> implode) s = "")
   *)

  (** [trimright] is [(rev >> trimleft cs >> rev)]. *)
  let trimright cs str =
    let rec loop i =
      if i < 0
      then ""
      else if contains cs str.[i]
      then loop ~--i
      else sub str 0 ~++i
    in
    len str - 1 |> loop
  (*$Q trimright
    Q.(pair string string) (fun (cs,s) -> trimright cs s |> len <= len s)
    Q.(pair small_string string) (fun (cs,s) -> let s' = trimright cs s in trimright cs s' = s')
    Q.(int_bound (len alphabet)) (fun n -> n = 0 || trimright (slice alphabet (n,0)) alphabet = slice alphabet (0,n))
    Q.string (fun s -> trimright s "" = "")
    Q.string (fun s -> trimright "" s = s)
    Q.string (fun s -> trimright s s = "")
    Q.string (fun s -> trimright (explode s |> nub |> implode) s = "")
   *)

  (** [(trim cs)] is [(trimleft cs >> trimright cs)]. *)
  let trim cs = trimleft cs $ trimright cs
  (*$Q trim
    Q.(pair small_string string) (fun (cs,s) -> trim cs s = (trimleft cs s |> trimright cs))
    Q.(pair small_string string) (fun (cs,s) -> trim cs s = (trimright cs s |> trimleft cs))
    Q.string (fun s -> trim "" s = s)
    Q.string (fun s -> trim s s = "")
    Q.string (fun s -> trim (explode s |> nub |> implode) s = "")
   *)

  (** [(commas ?comma num)] formats the (presumed) numeric string [num]
      with commas in the conventional manner.

      For example, [(commas "3628800") = "3,628,800"].

      All OCaml integer and float literals are accepted, but only
      decimal integers are commafied; hex, octal, and binary integers,
      and all floating point numbers are returned unmodified.

      Leading zeros, a leading '+'-sign, and all underscore spacers are
      elided in the commafied number.

      Other non-numeric strings will have commas inserted into them,
      but you shouldn't rely on this behavior due to all the special
      cases above.  *)
  let commas ?(comma=',') num =
    let addthem neg digits =
      let open List in
      filter ((<>) '_') digits
      |> rev
      |> takeall 3
      |> intersperse [comma]
      |> concat
      |> rev
      |> conswhen (k neg) '-'
      |> implode
    in
    let rec commas' neg n = match n with
    | '_'::rest                       -> commas' neg  rest
    | '-'::rest                       -> commas' true rest
    | '+'::rest                       -> commas' neg  rest
    | '0'::'x'::rest | '0'::'X'::rest
    | '0'::'o'::rest | '0'::'O'::rest
    | '0'::'b'::rest | '0'::'B'::rest -> num
    | '0'::rest                       -> commas' neg rest
    | digits when List.mem '.' digits -> num
    | digits (* otherwise *)          -> addthem neg digits
    in
    num |> explode |> commas' false
  (*$Q commas
    Q.int (fun n -> let comma = ',' in string_of_int n |> commas ~comma |> cuts ~sep:(String.make 1 comma) |> concat "" |> int_of_string = n)
    Q.numeral_string (fun n -> let comma = ',' in  n |> commas ~comma |> cuts ~sep:(String.make 1 comma) |> concat "" = trimleft "0" n)
    Q.numeral_string (fun n -> n = "" || trimleft "0" n = "" || let comma = ',' in  n |> commas ~comma |> cuts ~sep:(String.make 1 comma) |> List.all (fun s -> len s > 0 && len s < 4))
   *)

  (** [(plural ?reg ?irr n word)] returns the possibly plural form of the
      singular [word] in the context of the number [n].

      Use [~irr] to provide a fixed irregular plural form.  [(plural ~irr:x)]
      is equivalent to [(plural ~reg:(k x))].

      [~reg] is a function that returns the regular plural of a word;
      the default is for English and is [(postpend "s")]; see also
      {!es}.

      Pluralization is done as for English, viz. the singular is used
      for 1 and all other numbers, including 0 and negative numbers,
      use the plural form.

      Examples:
      - [(List.map (id *** flip plural "dog") [-1;0;1;2]) = [(-1, "dogs"); (0, "dogs"); (1, "dog"); (2, "dogs")]]
      - [(List.map (id *** flip (plural ~irr:"oxen") "ox") [-1;0;1;2]) = [(-1, "oxen"); (0, "oxen"); (1, "ox"); (2, "oxen")]]
  *)
  let plural ?reg ?irr n str =
    let reg = Option.default (flip (^) "s") reg in
    if n <> 1 then Option.default (reg str) irr else str

  (** [(es w)] is [(postpend "es")]; it is an alternative pluralization
      function for certain regularly irregular English nouns.

      Example:
      - [(List.map (id *** flip (plural ~reg:es) "wrass") [-1;0;1;2]) = [(-1, "wrasses"); (0, "wrasses"); (1, "wrass"); (2, "wrasses")]]
   *)
  let es w = w ^ "es"

  (** [(ocaml str)] converts its argument to OCaml syntax, wrapping it
  in double quotes and adding escapes as necessary. *)
  let ocaml = sprintf "%S"
  (* TODO TESTS *)

  (** [(parens (l,r) s)] parses the string on [Stream s] into a
      forest, recognizing [l] and [r] as left and right parentheses respectively.

      The forest is a list of nodes; each node is either [(`S s)]
      where [s] is a string, or [(`P f)] where [f] is the (sub)forest
      representing a prenthesized expression.

      If the parsed string contains an unterminated parenthesized
      expression, [Failure] is raised.

      Example:
      - [(Stream.of_string "foo" |> parens ('(',')')) = [`S "foo"]]
      - [(Stream.of_string "(foo)" |> parens ('(',')')) = [`P [`S "foo"]]]
      - [(Stream.of_string "a(foo)z" |> parens ('(',')')) = [`S "a"; `P [`S "foo"]; `S "z"]]
      - [(Stream.of_string "a(foo(bar))z" |> parens ('(',')')) = [`S "a"; `P [`S "foo"; `P [`S "bar"]]; `S "z"]]

      Unmatched right parens are allowed.  Example:
      - [(Stream.of_string "1) one" |> parens ('(',')')) = [`S "1) one"]]
  *)
  let parens (left, right) st =
    let next,peek = Stream.(next,peek) in
    let create,addc,contents = Buffer.(create,add_char,contents) in
    let toss c = assert (next st = c) in
    let rec nodes acc = match peek st with
      | Some c when c = left -> toss left; nodes (paren [] :: acc)
      | Some c               -> nodes (string false (create 64) :: acc)
      | None                 -> List.rev acc
    and paren acc = match peek st with
      | Some c when c = left  -> toss left; paren (paren [] :: acc)
      | Some c when c = right -> toss right; `P (List.rev acc)
      | Some c                -> paren (string true (create 64) :: acc)
      | None                  -> failwith "parens"
    and string p buf  = match p, peek st with
      | _,     Some c when c = left  -> `S (contents buf)
      | true , Some c when c = right -> `S (contents buf)
      | _,     None                  -> `S (contents buf)
      | false, Some c when c = right -> next st |> addc buf; string p buf
      | _,     Some c                -> next st |> addc buf; string p buf
    in
    nodes []

  (** [(string_of_parens (l,r))] converts the parsed forest returned
      by [(parens (l,r) s)] into a string.

      [(string_of_parens (l,r))] is the inverse of [(Stream.of_string >> parens (l,r))].
  *)
  let string_of_parens (left, right) ps =
    let create,addc,adds,contents = Buffer.(create,add_char,add_string,contents) in
    let buf = create 64 in
    let rec loop = function
      | `S s :: ps -> adds buf s; loop ps
      | `P l :: ps -> addc buf left; loop l; addc buf right; loop ps
      |         [] -> ()
    in
    loop ps; contents buf
  (* TODO TESTS *)

  (** [(seq_of_fields ~sep str)] is the sequence of [sep]-separated fields in [str].

      [sep] is as for {!cut}.
   *)
  let rec seq_of_fields ~sep str () =
    let open Seq in
    match cut ~sep str with
    | "", None    -> Nil
    | f,  None    -> Cons (f, fun () -> Nil)
    | f,  Some "" -> Cons (f, fun () -> Cons ("", fun () -> Nil))
    | f,  Some fs -> Cons (f, seq_of_fields ~sep fs)
  (*$Q seq_of_fields
    Q.(pair small_string (small_list (string_of_size Gen.(int_range 1 10)))) (fun (sep,ss) -> sep = "" || ss = [] || let str = concat sep ss in cuts ~sep str = (seq_of_fields ~sep str |> Seq.to_list))
   *)

  (** [(seq_of_lines)] is the sequence of ["\n"]-separated lines in [str] *)
  let seq_of_lines = seq_of_fields ~sep:"\n"
  (*$Q seq_of_lines
    Q.(small_list (string_of_size Gen.(int_range 1 10))) (fun ss -> ss = [] || let sep = "\n" in let str = concat sep ss in cuts ~sep str = (seq_of_lines str |> Seq.to_list))
   *)

  (** {2:strings_ops Ops} *)

  (** Infix and prefix operators. *)
  module Ops = struct (*$< Ops *)

    (** [(#!)] is {!(#!)}. *)
    let (#!) = (#!)

    (** [(#.)] is {!(#.)}. *)
    let (#.) = (#.)

    (** [(--)] is {!(--)}. *)
    let (--) = Chars.(--)
  end (*$>*)
end (*$>*)

(** This is Ocaml's standard [String] with the additional functions from {!Strings}
    and with {!Strings.Ops} opened. *)
module String = struct
  (** @closed *)
  include String

  (** @closed *)
  include Strings

  (** @closed *)
  include Strings.Ops
end

(** [split] is {!Strings.split} *)
let split = Strings.split

(** [join] is {!Strings.join} *)
let join = Strings.join

(** [(#!)] is {!Strings.(#!)}. *)
let (#!) = Strings.(#!)

(** [(#.)] is {!Strings.(#.)}. *)
let (#.) = Strings.(#.)

(** Case-insensitive ASCII strings. *)
module AIString = struct
  (** This is just {!String} with a case-insensitive (ASCII only)
      {!compare}; useful for instantiating {!Map}, {!Set} and the
      like. *)

  (** @closed *)
  include String
  let compare = on compare lowercase_ascii
end

(**/**)
module StringTest = struct      (* TODO move to a test suite *)
  let data = [
  (* string, merged, fielded *)
    "", [], [""];
    "f", ["f"], ["f"];
    "fo", ["fo"], ["fo"];
    "foo", ["foo"], ["foo"];
    ":", [], ["";""];
    "::", [], ["";"";""];
    ":foo", ["foo"], ["";"foo"];
    "foo:", ["foo"], ["foo";""];
    ":foo:", ["foo"], ["";"foo";""];
    "foo:bar:baz", ["foo";"bar";"baz"], ["foo";"bar";"baz"];
    "foo::bar:baz", ["foo";"bar";"baz"], ["foo";"";"bar";"baz"];
    "foo:+bar:baz", ["foo";"+bar";"baz"], ["foo";"+bar";"baz"];
    "foo:+:bar:baz", ["foo";"+";"bar";"baz"], ["foo";"+";"bar";"baz"];
  ]
  let test () =
    ("~sep:\":\"  ~elide:true ERRORS",
     List.map (fun (s,m,f) -> s, split ~sep:":" ~elide:true  s = m) data |> filter (fun (s,b) -> b=false))
      , ("~sep:\":\"  ~elide:false ERRORS"
            , List.map (fun (s,m,f) -> s, split ~sep:":" ~elide:false s = f) data |> filter (fun (s,b) -> b=false))
end
(**/**)

(** {1:show Show} *)

(** Alternate access to Type-Specific string conversions. *)
module Show = struct
  let bool = Bool.to_string
  let n0 = N0.to_string
  let n1 = N1.to_string
  let int = Int.to_string
  let pair = Pair.to_string
  (** [pair] is {!Pair.to_string}. *)

  let list = Lists.to_string
  (** [list] is {!Lists.to_string}. *)

  let string = id
  let interval = Interval.Int.to_string
  (** [interval] is {!Interval.Int.to_string}. *)

  let option = Option.to_string
  (** [option] is {!Option.to_string}. *)

  let result = Result.to_string
  (** [result] is {!Result.to_string}. *)

end

(** {1:print Print} *)

(** Alternate access to type-specific print functions. *)
module Print = struct
  let bool = Bool.print
  let n0 = N0.print
  let n1 = N1.print
  let int = Int.print
  let pair = Pair.print
  (** [pair] is {!Pair.print}. *)

  let list ?left ?sep ?right f = Lists.to_string ?left ?sep ?right f >> print_endline
  (** [list] is ({!Lists.to_string} [>> print)]. *)

  let string = id
  let interval = Interval.Int.print
  (** [interval] is {!Interval.Int.print}. *)

  let option = Option.print
  (** [option] is {!Option.print}. *)

  let result = Result.print
  (** [result] is {!Result.print}. *)

end

(** {1:vectors Vectors} *)

(* https://www.cs.cornell.edu/courses/cs3110/2014sp/recitations/11/functional-arrays.html *)

(** Fast functional arrays.

    {i NOT} thread-safe.

    "A fast functional array data structure using reverse diffs.
    This data structure can be used to translate imperative algorithms
    into pure functional programs without logarithmic overhead
    and without any special type system support (no monads or
    linear types).

    Accesses/updates to the latest version of the array are always
    constant time.  Access to older versions of the array have overhead
    proportional to the age (this can be improved using
    amortized rearrangement of the diff tree and instantiations
    of new array copies)."

    Derived from code originally written around 1990 for SML/NJ.

    Snarfed from: <{{:http://caml.inria.fr/pub/ml-archives/caml-list/2000/07/bf198cc419e4449b856cdb777ff7fb41.en.html}http://caml.inria.fr/pub/ml-archives/caml-list/2000/07/bf198cc419e4449b856cdb777ff7fb41.en.html}>

    Hacked by KW:
    - changed almost all the function names
    - made types abstract
    - changed the order of many parameters
    - added more functions

    @author Thomas M. Breuel
    @author Keith Waclena
*)
module type VECTOR = sig

  (** The type of vectors. *)
  type 'a t

  (** [compare] is the version of [compare] for ['a t]'s. *)
  val compare : 'a -> 'a -> int

  (** [(make n x)] is a vector of size [n] with all elements equal to [x]. *)
  val make : int -> 'a -> 'a t

  (** [(init n f)] is a vector of size [n] whose [i]'th element is [(f i)]. *)
  val init : int -> (int -> 'a) -> 'a t

  (* [(random ?size r ())] is a random vector of size [(size ())]
      (default: [< 100]) elements given by [(r ())]. *)
  val random : ?size:(unit -> int) -> (unit -> 'a) -> unit -> 'a t

  (** [(get i v)] is the [i]'th element of [v]. *)
  val get : int -> 'a t -> 'a

  (** [(v #! i)] is [(get i v)]. *)
  val (#!) : 'a t -> int -> 'a

  (** [(set i x v)] is the vector [v] with the [i]'th element changed to [x]. *)
  val set : int -> 'a -> 'a t -> 'a t

  (** [(len v)] is the length of vector [v]. *)
  val len : 'a t -> int

  (** [(update f i v)] is the vector [v] with the [i]'th element changed to [(f (get i v))]. *)
  val update : ('a -> 'a) -> int -> 'a t -> 'a t

  (** [(fold f acc v)] folds [f] over vector [v].

      [f] is called as [(f acc i)] where [acc] is the accumulator and
      [i] is the current index.
  *)
  val fold : ('a -> int -> 'a) -> 'a -> 'b t -> 'a

  (** [(map f v)] is the vector [v'] such that ∀i: [(get i v') = (get i v |> f)]. *)
  val map : ('a -> 'a) -> 'a t -> 'a t

  (** [(mapi f v)] is the vector [v'] such that ∀i: [(get i v') = (get i v |> f i)]. *)
  val mapi : (int -> 'a -> 'a) -> 'a t -> 'a t

  (** [(of_list l)] is the vector [v] such that ∀i: [(List.get i l) = (get i v)]. *)
  val of_list : 'a list -> 'a t

  (** [(to_list v)] is the list [l] such that ∀i: [(get i v) = (List.get i l)]. *)
  val to_list : 'a t -> 'a list

  (** {2 Ops} *)

  (** Infix operators. *)
  module Ops : sig
    (** [(#!)] is {!(#!)}. *)
    val ( #! ) : 'a t -> int -> 'a
  end
end

(** See {!VECTOR}. *)
module Vector : VECTOR = struct (*$< Vector *)
  type 'a record = Data of int * 'a array | Diff of int * int * 'a * 'a record ref
  type 'a t = 'a record ref
  let make n x = ref (Data (n, Array.make n x))
  (*$Q make; to_list; len
    Q.(pair small_int int) (fun (n,x) -> let v = make n x in len v = n && to_list v |> for_all ((=) x))
  *)
  let init n f = ref (Data (n, Array.init n f))
  (*$Q init; to_list; len
    Q.(pair small_int int) (fun (n,x) -> let v = init n (k x) in len v = n && to_list v |> for_all ((=) x))
    Q.(pair small_int int) (fun (n,x) -> let v = init n id in len v = n && to_list v = (0--(n-1)))
  *)

  (** [(random ?size r ())] is a random vector of size [(size ())]
      (default: [< 100]), and each element is given by [(r ())]. *)
  let random ?size r () =
    let n = match size with None -> Random.int 100 | Some f -> f () in
    init n (fun _ -> r ())
  (* TODO TESTS *)
  let rec get index array =
    match !array with

    | Data (_,a)    -> a.(index)
    | Diff(_,i,v,a) -> if i = index then v else get index a
  (*$Q get
    Q.(pair small_int int) (fun (n,x) -> let v = init n id in List.map (flip get v) (0--(n-1)) = to_list v)
    Q.(pair small_int int) (fun (n,x) -> let v = init n id in List.map (fun i -> v#!i) (0--(n-1)) = List.map (flip get v) (0--(n-1)))
  *)
  let (#!) v i = get i v
  (* test: see get above *)
  let compare = compare
  let set index value array =
    let old_value = get index array in
    match !array with
    | Data(size,a) -> (* for the main branch, create a reverse diff *)
        let result = ref (Data (size,a)) in
        a.(index) <- value;
        array := Diff (size,index,old_value,result);
        result
    | Diff (size,_,_,_) -> (* for branches, just create a forward diff *)
        ref (Diff (size,index,value,array))
  (*$Q set; get
    Q.small_int (fun n -> n = 0 || let v = make n 1 in set (n-1) 2 v |> get (n-1) = 2 && get (n-1) v = 1)
  *)
  let len a = match !a with Data (n,_) -> n | Diff (n,_,_,_) -> n
  (* test: see make and init above *)
  let update f i a = set i (f (get i a)) a
  (*$Q update; get
    Q.small_int (fun n -> n = 0 || let v = make n 1 in update (k 2) (n-1) v |> get (n-1) = 2 && get (n-1) v = 1)
  *)
  let fold f acc a = foldil (fun acc' i -> f acc' i) acc (0, (len a - 1))
  (*$Q fold
    Q.(pair small_int int) (fun (n,x) -> let v = init n id in fold snoc [] v |> rev = to_list v)
  *)
  let map f a = foldil (fun acc i -> update f i acc) a (0, (len a - 1))
  (*$Q map
    Q. small_int (fun n -> let v = init n id in map succ v |> to_list = 1--n)
  *)
  let mapi f a = foldil (fun acc i -> update (f i) i acc) a (0, (len a - 1))
  (*$Q mapi
    Q. small_int (fun n -> let v = init n id in mapi (fun i x -> i+x) v |> to_list = map2 (+) (0--(n-1)) (0--(n-1)))
  *)
  let of_list l = ref (Data (List.length l, Array.of_list l))
  (*$Q of_list
    Q.small_int (fun n -> let v = init n id in v = of_list (0--(n-1)))
  *)
  let to_list v = fold (fun acc i -> (get i v)::acc) [] v |> rev
  (* test: see almost every test above *)
  module Ops = struct
    let (#!) = (#!)
  end
end (*$>*)

(** {1:maps Maps} *)

(** [Map.Make (M)] is the module returned by OCaml's [Map.Make (M)] with some additional functions. *)
module Map = struct        (*$< Map *)

  (** Output signature of the functor {!Make} *)
  module type S = sig
    (** @closed *)
    include Map.S
    val string_of_binding : ?sep:string -> string -> string -> string
    val to_string :
      ?sep:string ->
      key:(key -> string) -> value:('a -> string) -> 'a t -> string
    val print :
      ?sep:string ->
      key:(key -> string) -> value:('a -> string) -> 'a t -> unit
    val random :
      ?size:(unit -> int) ->
      key:(unit -> key) -> value:(unit -> 'a) -> unit -> 'a t
    val cmp : String.t -> String.t -> int
    val size : 'a t -> int
    val lookup : String.t -> 'a t -> key * 'a
    val of_list : 'a t -> (key * 'a) list -> 'a t
    val adjoin : ('a -> 'b -> 'b) -> 'b -> key -> 'a -> 'b t -> 'b t
    val classify :
      ('a -> key) -> ('a -> 'b -> 'b) -> 'b -> 'a -> 'b t -> 'b t
    val incr : ?z:int -> ?i:int -> key -> int t -> int t
    val neighbors :
      String.t -> 'a t -> (key * 'a) option * (key * 'a) option
    val prev : String.t -> 'a t -> (key * 'a) option
    val prev_key : String.t -> 'a t -> key option
    val next : String.t -> 'a t -> (key * 'a) option
    val next_key : String.t -> 'a t -> 'a option
    val to_list : 'a t -> (key * 'a) list
    val keys : 'a t -> key list
    val values : 'a t -> 'a list
    val replace : key -> 'a -> 'a t -> 'a t
  end

  module Make (Ord : OrderedType) = struct
    (*$inject module T = Make (Int) open T *)
    (* generate a random map *)
    (*$inject
      let genmap =
        let open QCheck.Gen in
        let gen = list_size small_nat small_int >>= fun ns -> return T.(of_list empty (List.map (fun x -> x,x) ns)) in
        let print = T.to_list $ List.to_string (uncurry (sprintf "(%d,%d)")) in
        QCheck.make ~print gen
    *)
    module M = Map.Make(Ord)

    (** {1 4.05 Compatibility Functions} *)
    (** These 4.05 functions provide compatibility with earlier
        versions of the standard library.  If [Prelude] is linked with
        4.05 or later, the official functions will be used instead.

        @before 4.05 (compatibility) *)

    (* re 4.05 compatibility tests: these tests are assumed to be run with an ocaml >= 4.05 *)

    let find_opt k m = match M.find k m with exception Not_found -> None | v -> Some v
    (*$Q find_opt
      Q.int (fun n -> find_opt n empty = M.find_opt n empty)
      Q.int (fun n -> singleton n n |> find_opt n = (singleton n n |> M.find_opt n))
      Q.(pair genmap int) (fun (m,n) -> add n n m |> find_opt n = (add n n m |> M.find_opt n))
      Q.(pair genmap int) (fun (m,n) -> find_opt n m = (find_opt n m))
    *)
    let min_binding_opt m = match M.min_binding m with exception Not_found -> None | b -> Some b
    (*$Q min_binding_opt
      Q.unit (fun () -> min_binding_opt empty = M.min_binding_opt empty)
      genmap (fun m -> min_binding_opt m = M.min_binding_opt m)
    *)
    let max_binding_opt m = match M.max_binding m with exception Not_found -> None | b -> Some b

    (*$Q max_binding_opt
      Q.unit (fun () -> max_binding_opt empty = M.max_binding_opt empty)
      genmap (fun m -> max_binding_opt m = M.max_binding_opt m)
    *)
    let choose_opt m = match M.choose m with exception Not_found -> None | b -> Some b
    (*$Q choose_opt
      Q.unit (fun () -> choose_opt empty = M.choose_opt empty)
      genmap (fun m -> choose_opt m = M.choose_opt m)
    *)
    (* iter runs in increasing order of keys; we use Exn.label to short-circuit *)
    let find_first_opt f m =
      Exn.label (fun l -> M.iter (fun k v -> if f k then Exn.return l (Some (k,v))) m; None)
    (*$Q find_first_opt
      Q.int (fun n -> find_first_opt (fun x -> x >= n) empty = M.find_first_opt (fun x -> x >= n) empty)
      Q.(pair genmap int) (fun (m,n) -> find_first_opt (fun x -> x >= n) m = M.find_first_opt (fun x -> x >= n) m)
    *)
    let find_first f m = match find_first_opt f m with Some b -> b | None -> raise Not_found
    (*$Q find_first
      Q.int (fun n -> catch (find_first (fun x -> x >= n)) empty = catch (M.find_first (fun x -> x >= n)) empty)
      Q.(pair genmap int) (fun (m,n) -> catch (find_first (fun x -> x >= n)) m = catch (M.find_first (fun x -> x >= n)) m)
    *)
    let find_last_opt f m = M.fold (fun k v a -> if f k then Some (k,v) else None) m None
    (*$Q find_last_opt
      Q.int (fun n -> find_last_opt (fun x -> x <= n) empty = M.find_last_opt (fun x -> x <= n) empty)
      Q.(pair genmap int) (fun (m,n) -> find_last_opt (fun x -> x <= n) m = M.find_last_opt (fun x -> x <= n) m)
    *)
    let find_last f m = match find_last_opt f m with Some b -> b | None -> raise Not_found
    (*$Q find_last
      Q.int (fun n -> catch (find_last (fun x -> x <= n)) empty = catch (M.find_last (fun x -> x <= n)) empty)
      Q.(pair genmap int) (fun (m,n) -> catch (find_last (fun x -> x <= n)) m = catch (M.find_last (fun x -> x <= n)) m)
    *)

    (** {1 Official OCaml Map Functions} *)

    (** @closed *)
    include M

    (** {1 Additional Map Functions} *)

    (** {2 Development Utils} *)

    (** [(string_of_binding)] is a string representation of the binding of [k] to [v], separated by [sep] (default: [" -> "]). *)
    let string_of_binding ?(sep=" -> ") k v = sprintf "%s%s%s" k sep v
    (* TODO TESTS *)

    (** [(to_string ?(sep=", ") f t)] returns a string representation
        of the map [t], where the string representation of each key
        is given by [~key] and the string representation of each value
        is given by [~value]. *)
    let to_string ?(sep="\n") ~key ~value t =
      let each k v acc = string_of_binding (key k) (value v) :: acc in
      fold each t [] |> rev |> join ~sep
    (* TODO TESTS *)

    (** [(print ?sep ~key ~value t)] is [(to_string ?sep ~key ~value t |> print_endline)]. *)
    let print ?sep ~key ~value = to_string ?sep ~key ~value >> print_endline
    (* TODO TESTS *)

    (** {2 Creation} *)

    (** [(random ?size ~key ~value ())] is a random map of size [(size ())] (default: [< 100]),
        and each binding is given by [(key (), value ())]. *)
    let random ?size ~key ~value () =
      let n = match size with None -> Random.int 100 | Some f -> f () in
      foldil (fun acc _ -> add (key ()) (value ()) acc) empty (1,n)
    (* TODO TESTS *)

    (** {2 Comparison} *)
    (** [cmp] is {!Ord.compare} and is the comparison function that implements this map. *)
    let cmp = Ord.compare

    (** {2 Map Size} *)

    (** [size] is {!cardinal}. *)
    let size = cardinal
    (*$Q size
      Q.unit (fun () -> size empty = cardinal empty)
      genmap (fun m -> size m = cardinal m)
    *)

    (** {2 Access} *)

    (** [(lookup key)] is like [find] but returns the complete binding [(key,value)].

        This is useful when {!Ord.compare} maps many keys to one,
        e.g. [(let compare = String.(on compare lowercase_ascii))]. *)
    let lookup key t = match partition (fun k v -> Ord.compare key k = 0) t |> fst |> bindings with
    | [pair] -> pair
    | []     -> raise Not_found
    | _      -> assert false
    (*$Q lookup
      Q.int (fun n -> catch (lookup n) empty = None)
      Q.(pair genmap int) (fun (m,n) -> match lookup n m with exception Not_found -> true | b -> b = (n,n))
      genmap (fun m -> match choose m with exception Not_found -> true | k,v -> lookup k m = (k,v))
    *)

    (** {2 Adding Values to the Map} *)

    (** [(replace k v t)] replaces the binding for [k] in [t] with a new binding [(k,v)]. *)
    let replace k v t = update k (fun _ -> Some v) t
    (* TODO TESTS *)

    (** [(of_list t alist)] adds all the bindings in [alist] to [t]
        (which could be {!empty}). *)
    let of_list t = foldl (fun acc (k,v) -> add k v acc) t
    (*$Q of_list
      Q.(list int) (fun xs -> let m = map2 Pair.up xs xs |> of_list empty in foldl (fun b x -> b && exists (fun k v -> k=x&&v=x) m) true xs)
    *)

    (** [(adjoin vadd empty)] returns a function [(f k v m)] that
        adjoins [v] to a collection of values associated with key [k].
        [vadd] and [empty] might be {!Lists.cons} and [[]] for list
        accumulators, or [S.add] and [S.empty] for some set [S].

        See {!Multiset} for common situations.

        Examples:

        - accumulate the case-insensitive spellings of words:
        {[module M = Map.Make (String)
        let each k = M.adjoin cons [] (String.lowercase_ascii k) k in
        List.foldr each M.empty (split "Foo Bar bAR FOO") |> M.to_list
        = [[("bar", ["Bar"; "bAR"]); ("foo", ["Foo"; "FOO"])]]
        ]}

        - separate out the even versus the odd numbers in [xs] (indexed under [0] for even, [1] for odd):
        {[ foldr (fun x m -> adjoin cons [] (abs (x mod 2)) x m) empty xs ]}
    *)
    let adjoin vadd empty k v m = default empty (find k) m |> vadd v |> flip (add k) m
    (*$Q adjoin
      Q.int (fun n -> adjoin cons [] n n empty |> find n = [n])
      Q.int (fun n -> adjoin cons [] n n empty |> adjoin cons [] n n |> find n = [n;n])
      Q.(list int) (fun xs -> let m = foldr (fun x m -> adjoin cons [] (abs (x mod 2)) x m) empty xs in keys m |> all (fun k -> (k=0||k=1) && find k m |> all (if k=0 then even else odd)))
    *)

    (** [(classify f vadd empty)] is a classifying adjoiner.

        {[ module M = Map.Make (struct type t = bool let compare = compare end)
M.(foldr (classify Int.even cons []) empty (1--10) |> to_list)
= [(false, [1; 3; 5; 7; 9]); (true, [2; 4; 6; 8; 10])]

        ]}
    *)
    let classify f vadd empty = fun x m -> adjoin vadd empty (f x) x m
    (*$Q classify
      Q.(list int) (fun xs -> foldr (fun x m -> classify (fun x -> abs (x mod 2)) cons [] x m) empty xs |> for_all (fun k vs -> if k=0 then all even vs else all odd vs))
    *)

    (** [(incr ?z ?i k m)] is [(adjoin (+) z k i m)], i.e. [incr]
        generates a function for counting instances of items ([k]) in
        an [(int t)] ([m]).

        [~z] is the "zero" for the counters (default: [0]); [~i] is the
        increment (default: [1]).

        Count the chars in a string:
        {[ String.foldr M.incr "abbcccd" M.empty |> M.to_list =
        [('a', 1); ('b', 2); ('c', 3); ('d', 1)] ]}

    *)
    let incr ?(z=0) ?(i=1) k m = adjoin (+) z k i m

    (** {2 Accessing Bindings in Key Order}

        The most efficient way to access the bindings of a map in key
        order is to use {!fold}, but sometimes access is required in a
        non-[fold] context.  OCaml's map doesn't provide this access
        directly, so these functions do it by calling {!partition}, so
        each call to one of these functions is proportional to the
        size of the map. *)

    (** [(neighbors k m)] is the pair [(prev,next)] where [prev] and
        [next] are, respectively, options carrying the binding prior
        to and succeeding the key [k] in the map [m], in [Ord.compare]
        order.

        If [k] is the first key in [m] (i.e. would be returned by
        [min_binding]), then [prev] is [None].  Likewise, if [k] is
        the last key in the map (i.e. would be returned by
        [max_binding]), then [next] is [None].

        Otherwise, [prev] and [next] are of the form [(Some (k',v))],
        where [(k',v)] is the appropriate binding.
    *)
    let neighbors k t =
      let a,b = partition (fun k' _ -> cmp k' k = 1) t in
      catch (partition (fun k' _ -> cmp k' k = 0) $ snd $ max_binding) b, catch min_binding a
    (* TODO TESTS *)

    (** [(prev k m)] is [(neighbors k m |> fst)], the binding prior to [k] in map [m]. *)
    let prev k t = neighbors k t |> fst
    (* TODO TESTS *)

    (** [(prev_key k m)] is just the key part of the binding prior to [k] in map [m]. *)
    let prev_key k t = Option.(prev k t >>| fst)
    (* TODO TESTS *)

    (** [(next k m)] is [(neighbors k m |> snd)], the binding succeding [k] in map [m]. *)
    let next k t = neighbors k t |> snd
    (* TODO TESTS *)

    (** [(next_key k m)] is just the key part of the binding succeeding [k] in map [m]. *)
    let next_key k t = Option.(next k t >>| snd)
    (* TODO TESTS *)

    (** {2 Extracting Data from the Map} *)

    (** [to_list] is {!bindings}. *)
    let to_list = bindings

    (** [(keys m)] is the keys in the map [m] as a list, in [Ord.compare] key-order. *)
    let keys m = fold (fun k _ acc -> k::acc) m [] |> rev
    (* TODO TESTS *)

    (** [(values m)] is the values in the map [m] as a list, in
        [Ord.compare] key-order (not value-order). *)
    let values m = fold (fun _ v acc -> v::acc) m [] |> rev
    (* TODO TESTS *)
  end

  (** Functor building an implementation of a "specialized",
      non-polymorphic map from [K.t] to [V.t].

      The resulting module contains modified [to_string] and [print]
      for easy top-level development and debugging. *)
  module Specialize (K : TOSTRING) (V : TOSTRING) = struct (* TODO PROMOTE to any other multiparm functors *)
    (** @closed *)
    include (Make (K))
    type nonrec t = V.t t
    let to_string ?sep = to_string ?sep ~key:K.to_string ~value:V.to_string
    let print ?sep = to_string ?sep >> print_endline
  end
end (*$>*)

(** {1:sets Sets} *)

(** [Set.Make (M)] is the module returned by [Stdlib.Set.Make (M)] with some additional functions. *)
module Set = struct (*$< Set *)
  module type S = Set.S
  module Make (Ord : Set.OrderedType) = struct
    (*$inject module S = Set.Make (Int) open S *)
    (* generate a random set *)
    (*$inject
      let gens =
        let open QCheck.Gen in
        let gen = list_size small_nat small_int >>= fun ns -> return S.(of_list ns) in
        let print = to_list $ List.to_string string_of_int in
        QCheck.make ~print gen
    *)

    module S = Set.Make(Ord)

    (** [(map f t)] is the set [t] with each element [x] replaced by [(f x)].

        @before 4.04 [map] went missing from 4.03; this function
        provides compatibility.  If compiled with 4.04, the more
        sophisticated 4.04 [map] will be used instead.  *)
    let map f t = S.(fold (fun e t -> add (f e) t) t empty)

    (** @closed *)
    include S

    (** [cmp] is [Ord.compare] and is the comparison function that implements this set. *)
    let cmp = Ord.compare
    (*$Q cmp
      gens (fun s -> is_empty s || let a,b = min_elt s, max_elt s in cmp a b = Int.compare a b)
    *)

    (** [(size t)] is {!cardinal}. *)
    let size = cardinal
    (*$Q size
      gens (fun s -> size s = cardinal s)
    *)

    (** [(to_list)] is {!elements}. *)
    let to_list = elements
    (*$Q to_list
      gens (fun s -> to_list s = elements s)
    *)

    (** [(replace elt t)] is [(remove elt t |> add elt)].

        {[
module S = Set.Make (struct type t = int * string let compare = on compare fst end)
S.(empty |> add (1,"one") |> add     (1,"ONE") |> to_list) = [(1, "one")]
S.(empty |> add (1,"one") |> replace (1,"ONE") |> to_list) = [(1, "ONE")]
        ]}
    *)
    let replace elt t = remove elt t |> add elt
    (*$Q replace
      Q.int (fun n -> replace n empty |> equal (singleton n))
      gens (fun s -> is_empty s || let elt = choose s in replace elt s = add elt (remove elt s))
      Q.(pair gens int) (fun (s,elt) -> replace elt s = add elt (remove elt s))
      Q.(pair int int) (fun (a,b) -> let s = List.repeat 100 a |> of_list in replace b s = add b s)
      Q.(pair int int) (fun (a,b) -> let s = List.repeat 100 a |> of_list in replace a s = singleton a)
    *)

    (** [(addwith f)] is [(fun f x t -> add (f x) t)]. *)
    let addwith f = lefthook add f
    (*$Q addwith
      Q.int (fun n -> addwith id n empty = singleton n)
      Q.int (fun n -> addwith (k 1) n empty = singleton 1)
      Q.(pair int gens) (fun (n,s) -> addwith id n s = add n s)
      Q.(pair int gens) (fun (n,s) -> addwith (k 1) n s = add 1 s)
    *)

    (** [(addwhen p x xs)] is [(add x xs)] when [p x = true] and otherwise is [xs]. *)
    let addwhen p x t = if p x then add x t else t
    (*$Q addwhen
      Q.int (fun n -> addwhen even n empty = if even n then singleton n else empty)
      Q.(pair int gens) (fun (n,s) -> addwhen even n s = if even n then add n s else s)
    *)

    (** {2 Development Utils} *)

    (** [(random ?size r ())] is a random set of size [(size ())]
        (default: [< 100]), and each element is given by [(r ())]. *)
    let random ?size r () =
      let n = match size with None -> Random.int 100 | Some f -> f () in
      foldil (fun acc _ -> add (r ()) acc) empty (1,n)
    (* TODO TESTS *)

    (** [(to_string ?(sep=", ") f t)] returns a string representation
        of the set [t], where the string representation of each [elt]
        is given by [f]. *)
    let to_string ?(sep=", ") f t =
      fold (conswith f) t [] |> rev |> join ~sep
    (* TODO TESTS *)

    (** [(print ?sep f t)] is [(to_string ?sep f t |> print_endline)]. *)
    let print ?sep f t = to_string ?sep f t |> print_endline (* TODO also add optional Format.formatter *)
  end
end (*$>*)

(** {1:multisets Multisets (aka {i bags})} *)

(** [Multiset.Make (Ord)] is a set of [Ord.t]'s with repetition.

    The order the repetitions of any element is undefined.

    A multiset is especially useful for gathering together values that
    are members of some equivalence class but are not completely
    identical.  Examples would be strings considered
    case-insensitively, or records compared only on one field.
    You can add these values to a multiset, and then ask for
    everything that is equivalent to some value.

    Example: gather ints by parity and then find odd ints in the multiset:
    {[module PS = Multiset.Make (struct type t = int let compare = on compare even end)
    let ps = 1--10 |> PS.of_list in
    PS.find 1 ps = [9; 7; 5; 3; 1]
    && PS.find 5 ps = [9; 7; 5; 3; 1]
    ]}

    You can also use a multiset simply to count repetitions of values:
    {[module PS = Multiset.Make (Int)
    (1--3 @ [4;4;4] @ 5--10) |> PS.of_list |> PS.multiplicity 4 = 3
    ]}

    But since a multiset actually {i stores} all the repetitions, it would
    be more efficient to use {!Map.Make.incr}:

    {[module M = Map.Make (Int)
    (1--3 @ [4;4;4] @ 5--10) |> foldr M.incr M.empty |> M.find 4 = 3
    ]}

 *)
module Multiset = struct (*$< Multiset *)
  module type OrderedType = OrderedType
  module type S = sig

    (** {1 Types and Values} *)

    type elt
    (** The type of the multiset elements. *)

    type t
    (** The type of multisets. *)

    val empty : t
    (** The empty set. *)

    (** {1 Comparison} *)

    val cmp : elt -> elt -> int
    (** The comparison function for [elt]'s. *)

    val compare : t -> t -> int
    (** [compare] is the version of [compare] for multisets. *)

    (** {1 Support} *)

    val support : t -> elt list
    (** [(support t)] is the list of all the {i distinct} (under
        [Ord.compare]) elements of the multiset [t].

        Note that each of the elements in the result is an arbitrary
        exemplar of its equivalent repetitions.

        [support] is equivalent to [(to_list >> sort_uniq Ord.compare)] but is faster.
     *)

    val support_size : t -> int
    (** [(support_size)] is [(len << support)] but is faster. *)

    (** {1 Sizes}

        See also {!support_size}.
     *)

    val cardinal : t -> int
    (** [(cardinal t)] is the sum of the multiplicities of all the elements of [t]. *)

    val size : t -> int
    (** [size] is [cardinal]. *)

    val multiplicity : elt -> t -> int
    (** [(multiplicity elt t)] is the number of repetitions of [elt] in [t]. *)

    val count : elt -> t -> int
    (** [count] is [multiplicity]. *)

    (** {1 Predicates } *)

    val is_empty : t -> bool
    (** [(is_empty t)] is [true] iff [t] is empty. *)

    val mem : elt -> t -> bool
    (** [(mem x t)] is [true] iff [x] is a member of [t] *)

    val disjoint : t -> t -> bool
    (** [(disjoint a b)] is [true] iff [a] and [b] have no elements in common. *)

    val equal : t -> t -> bool
    (** [(equal a b)] is [true] iff both multisets contain the same
        elements, and the same repetitions of elements. *)

    val subset : t -> t -> bool
    (** [(subset a b)] is [true] iff all the distinct elements of [a]
        are contained in [b], and the {!multiplicity} of each element
        of [a] is [<=] the multiplicity of the corresponding
        element of [b]. *)

    val for_all : (elt -> bool) -> t -> bool
    (** [(for_all p t)] is [true] iff all elements of [t]
        satisfy the predicate [p]. *)

    val exists  : (elt -> bool) -> t -> bool
    (** [(exists p t)] is [true] iff at least one element of [t]
        satisfies the predicate [p]. *)

    (** {1 Creating Multisets}

        See also {!of_seq} and {!add_seq}.
     *)

    val singleton : elt -> t
    (** [(singleton elt)] is the multiset that contains only [elt]. *)

    val of_list : elt list -> t
    (** [(of_list xs)] is the multiset containing containing all the elements of [xs]. *)

    val split : elt -> t -> t * bool * t
    (** [(split x t)] is a triple [(l, present, r)], where [l] is the
        multiset of elements of [t] that are strictly less than [x];
        [r] is the multiset of elements of [t] that are strictly
        greater than [x]; [present] is [false] if [t] contains no
        element equal to [x], or [true] if [t] contains an element
        equal to [x].

        If you also need the repetitions of [x], you can call [(find x t)].
     *)

    (** {1 Adding Elements} *)

    val add : elt -> t -> t
    (** [(add elt t)] is the multiset [t] with [elt] added.

        Invariant: [(multiplicity elt (add elt t)) = (1 + multiplicity elt t)]
     *)

    val addwith : ('a -> elt) -> 'a -> t -> t
    (** [(addwith f)] is [(fun f x t -> add (f x) t)]. *)

    val addwhen : (elt -> bool) -> elt -> t -> t
    (** [(addwhen p x t)] is [(add x t)] when [p x = true] and otherwise is [t]. *)

    (** {1 Removing Elements} *)

    val remove : elt -> t -> t
    (** [(remove elt t)] is the multiset that contains all the
        elements of [t] excepting [elt].

        All repetitions of [elt] are removed.

        Invariants:
        - [(mem x (remove x t)) = false]
        - [(multiplicity x (remove x t)) = 0]
     *)

    val replace : elt -> (elt list) -> t -> t
    (** [(replace elt reps t)] is [t] with all the repetitions of [elt] replaced by [reps]. *)

    (** {1 Functionals} *)

    val iter : (elt list -> unit) -> t -> unit
    (** [(iter f t)] iterates [f] across each distinct element of [t];
        the repetitions of each element are given to [f] as a list. *)

    val flatiter : (elt -> unit) -> t -> unit
    (** [(flatiter f t)] iterates [f] over all the repetitions of all
        the elements of [t].

        [flatiter f] is equivalent to [(iter (List.iter f))]. *)

    val map : (elt -> elt) -> t -> t
    (** [(map f t)] is a multiset where all repetitions of all elements
        have been replaced by the result of applying [f] to those
        elements. *)

    val flatfold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
    (** [(flatfold f t init)] computes (f x{^ n} ... (f x{^ 2} (f
        x{^ 1} init))...), where x{^ 1} ... x{^ n} are the
        repetitions of all elements of [t], in increasing order. *)

    val fold : (elt list -> 'b -> 'b) -> t -> 'b -> 'b
    (** [(fold f t init)] is like {!flatfold} except that [f] is
        called once for each distinct distinct element of [t], and [f]
        receives the repetitions of an element as a list. *)

    val filter  : (elt -> bool) -> t -> t
    (** [(filter p t)] is the multiset of all elements that satisfy the predicate [p].

        [p] is applied to each repetition of each distinct element. *)

    val partition : (elt -> bool) -> t -> t * t
    (** [(partition p t)] returns a pair of multisets [(t1, t2)],
        where [t1] is the multiset of all the elements of [t] that
        satisfy the predicate [p], and [t2] is the multiset of all the
        elements of [t] that do not satisfy [p]. *)

    (** {1 Set Theoretic Functions} *)

    val union : t -> t -> t
    (** [(union s t)] is the maximum or lowest common multiple of multisets [s] and [t].

        See {!sum} for what seems to me a more practical function. *)

    val sum : t -> t -> t
    (** [(sum s t)] is the disjoint union of [s] and [t].

       The sum contains the union of the distinct elements of [s] and
       [t], and the repetitions of each element is the union of its
       repetitions in the two original sets..

       Invariants:
       - [(support (sum s t)) = List.union (support s) (support t)]
       - [(sort Ord.compare (elements (sum s t))) = sort Ord.compare (elements s @ elements t)] *)

    val inter : t -> t -> t
    (** [(inter s t)] is the infimum or greatest common divisor of multisets [s] and [t]. *)

    val diff : t -> t -> t
    (** [(diff s t)] is the multiset that contains the distinct
        elements of [s] that are not in [t]. *)

    (** {1 Extracting Elements} *)

    val find : elt -> t -> elt list
    (** [(find elt t)] is the list of the repetitions of [elt] in [t].

        Invariant: [(len (default [] (find elt) t)) = (multiplicity elt t)]

        @raise Not_found if [is_empty t]
     *)

    val find_opt : elt -> t -> elt list option
    (** [(find_opt elt t)] is [(catch ~this:Not_found (find elt) t).] *)

    val elements : t -> elt list
    (** [(elements t)] are all the repetitions of all the distinct
        elements of [t], in ascending order of [Ord.compare].

        Invariant:
        - [len (support t) <= len (elements t)]
     *)

    val to_list : t -> elt list
    (** [(to_list t)] is {!elements}. *)

    val repetitions : t -> elt list list
    (** [(repetitions t)] is the list of all repetitions of each
        distinct element, in ascending order of [Ord.compare], in the
        form of a list of lists.

        Invariants:
        - [(len << repetitions) = (len << support)]
        - [(repetitions >> List.map len >> List.sum) = cardinal]
        - [flatten (repetitions t) = elements t]
     *)

    val choose : t -> elt list
    (** [(choose t)] is the set of repetitions of one distinct element
        of the given multiset.  Which element is chosen is
        unspecified, but equal elements will be chosen for equal
        multisets.

        @raise Not_found if [is_empty t]
     *)

    val choose_opt : t -> elt list option
    (** [(choose t)] is the set of repetitions of one distinct element
        of the given multiset; if the multiset is empty, the result is
        [None].  Which element is chosen is unspecified, but equal
        elements will be chosen for equal multisets. *)

    val find_first : (elt -> bool) -> t -> elt list
    (** [(find_first f t)] where [f] is a monotonically increasing
        function, returns the repetitions of the lowest element [e] of
        [t] such that [(f e)].

        @raise Not_found if no such element exists.
     *)

    val find_first_opt : (elt -> bool) -> t -> elt list option
    (** [(find_first_opt f)] is [(catch (find_first_opt f))]. *)

    val find_last : (elt -> bool) -> t -> elt list
    (** [(find_last f t)] where [f] is a monotonically decreasing
        function, returns the repetitions of the highest element [e] of
        [t] such that [(f e)].

        @raise Not_found if no such element exists.
     *)

    val find_last_opt : (elt -> bool) -> t -> elt list option
    (** [(find_last_opt f)] is [(catch (find_last_opt f))]. *)

    val min_elt : t -> elt list
    (** [(min_elt_opt t)] is the smallest element of [t], per [Ord.compare].

        @raise Not_found if no such element exists.
     *)

    val min_elt_opt : t -> elt list option
    (** [(min_elt f)] is [(catch (min_elt f))]. *)

    val max_elt : t -> elt list
    (** [(max_elt_opt t)] is the largest element of [t], per [Ord.compare].

        @raise Not_found if no such element exists.
     *)

    val max_elt_opt : t -> elt list option
    (** [(max_elt_opt f)] is [(catch (max_elt f))]. *)

    (** {1 Sequences} *)

    val to_seq : t -> elt list Seq.t
    (** [(to_seq t)] is the [Seq.t] equivalent to [(repetitions t)]. *)

    val to_seq_from : elt -> t -> elt list Seq.t
    (** [(to_seq_from elt t)] is [(to_seq t)] but containing only
        those elements from [elt] or above. *)

    val of_seq : elt Seq.t -> t
    (** [(of_seq seq)] is the multiset containing all the elements of [seq]. *)

    val add_seq : elt Seq.t -> t -> t
    (** [(add_seq seq t)] is [t] with the addition of all the elements in [seq]. *)

    (** {1 Development Utils} *)

    val to_string : ?sep:string -> (elt -> string) -> t -> string
    (** [(to_string ?(sep=", ") f t)] returns a string representation
        of the multiset [t], where the string representation of each [elt]
        is given by [f]. *)

    val print : ?sep:string -> (elt -> string) -> t -> unit
    (** [(print ?sep f t)] is [(to_string ?sep f t |> print_endline)]. *)

    val random : ?size:(unit -> int) -> (unit -> elt) -> unit -> t
    (** [(random ?size r ())] is a random multiset of size [(size ())]
        (default: [< 100]), and each element is given by [(r ())]. *)

  end

  (**/**)

  (* the implementation of the functor without a sig applied, so that
     unit tests can use knowledge of the implementation.
   *)

  module Make' (Ord : OrderedType) = struct
    (*$inject module B = Make' (Int) open B *)
    (* generate a random multiset *)
    (*$inject
      let genms =
        let open QCheck.Gen in
        let gen = list_size small_nat small_int >>= fun ns -> return B.(of_list ns) in
        let print = B.to_list $ List.to_string string_of_int in
        QCheck.make ~print gen
     *)
    (*$inject let sortedly = sort Int.compare *)
    (**/**)
    module M = Map.Make (Ord)
    (**/**)
    type elt = Ord.t
    type t = (int * elt list) M.t

    (** [cmp] is {!Ord.compare} and is the comparison function that implements this set. *)
    let cmp = Ord.compare
    (*$Q cmp
      genms (fun s -> is_empty s || let a,b = min_elt s, max_elt s in comparison cmp a b = comparison Int.compare a b)
    *)
    let empty = M.empty
    let is_empty = M.is_empty
    (*$Q is_empty
      Q.unit (fun () -> is_empty empty)
      genms (fun ms -> add 1 ms |> is_empty |> not)
      genms (fun ms -> is_empty ms || M.for_all (fun elt (n,xs) -> n > 0 && xs <> [] && List.mem elt xs) ms)
     *)
    (*$Q is_empty;singleton
      Q.unit (fun () -> not (is_empty (singleton 1)))
     *)
    let mem = M.mem
    (*$Q mem
      Q.int (fun i -> mem i empty |> not)
      genms (fun ms -> equal empty ms || for_all (flip mem ms) ms)
    *)
    (*$Q mem;add
      Q.(pair small_int genms) (fun (i,ms) -> add i ms |> mem i)
    *)
    let add elt t = match M.find elt t with
      | exception Not_found -> M.add elt (1, [elt]) t
      | (n,xs)              -> M.add elt (succ n, cons elt xs) t
    (* test: see also mem above *)
    (*$Q add
      Q.int (fun i -> add i empty <> empty)
      Q.int (fun i -> add i empty = singleton i)
      Q.int (fun i -> add i empty |> M.choose = (i,(1,[i])))
      Q.int (fun i -> add i empty |> M.find i = (1,[i]))
      Q.(pair int genms) (fun (n,ms) -> add n ms |> find n = (default [] (find n) ms |> cons n))
      Q.(list int) (fun xs -> foldl (flip add) empty xs |> M.for_all (fun k (n,rs) -> n = len rs && all (fun r -> Int.compare r k = 0) rs))
     *)
    let singleton elt = M.add elt (1,[elt]) M.empty
    (*$Q singleton
      Q.int (fun i -> singleton i |> count i = 1)
      Q.int (fun i -> singleton i |> add i |> count i = 2)
      Q.int (fun i -> singleton i |> is_empty |> not)
    *)
    let remove elt t = M.remove elt t       (* remove all reps *)
    (*$Q remove
      Q.(pair small_int genms) (fun (i,ms) -> add i ms |>          remove i |> mem i |> not)
      Q.(pair small_int genms) (fun (i,ms) -> add i ms |> add i |> remove i |> mem i |> not)
    *)
    let replace elt reps t = if mem elt t then foldl (flip add) (remove elt t) reps else t
    (*$Q replace
      Q.(pair small_int int) (fun (n,x) -> replace x (repeat n x) empty = empty)
      Q.(pair small_int int) (fun (n,x) -> replace x (repeat n x) (singleton x) = (repeat n x |> sort Int.compare |> of_list))
      Q.(triple small_int int genms) (fun (n,x,t) -> replace x (repeat n x) t |> default [] (find x) = if mem x t then repeat n x |> sort Int.compare else [])
     *)
    let repetitions t = M.fold (fun _ (_,xs) acc -> xs :: acc) t [] |> rev
    (*$Q repetitions
      Q.unit (fun () -> repetitions empty = [])
      Q.unit (fun () -> singleton 1 |> repetitions = [[1]])
      Q.unit (fun () -> singleton 1 |> add 2 |> add 1 |> repetitions |> sort Stdlib.compare = [[1;1]; [2]])
      genms (fun ms -> repetitions ms |> flatten |> sort_uniq Int.compare = (support ms |> sortedly))
      genms (fun ms -> repetitions ms |> flatten |> sortedly = (elements ms |> sortedly))
      genms (fun ms -> len (repetitions ms) = len (support ms))
      genms (fun ms -> List.sum (List.map len (repetitions ms)) = cardinal ms)
     *)
    let union t1 t2 = M.union (fun _ (n1,v1) (n2,v2) -> Some (if n1 > n2 then n1, v1 else n2, v2)) t1 t2
    (*$Q union
      genms (fun ms -> equal (union ms empty) ms)
      genms (fun ms -> (union ms ms |> support) = (support ms))
      Q.(pair genms genms) (fun (r,s) -> (union r s |> support) = (List.union (support r) (support s)))
      Q.(pair genms genms) (fun (r,s) -> let t = union r s in flatfold (fun x acc -> count x t = max (count x r) (count x s)) t true)
    *)
    let sum t1 t2 = M.union (fun _ (n1,v1) (n2,v2) -> Some (n1+n2, trappend v1 v2)) t1 t2
    (*$Q sum
      Q.unit (fun () -> equal (sum empty empty) empty)
      genms (fun ms -> equal (sum ms empty) ms)
      genms (fun ms -> (sum ms ms |> support) = (support ms))
      Q.(pair genms genms) (fun (r,s) -> (sum r s |> support) = (List.union (support r) (support s)))
      Q.(pair genms genms) (fun (r,s) -> let t = sum r s in flatfold (fun x acc -> count x t = (count x r) + (count x s)) t true)
      Q.(pair genms genms) (fun (r,s) -> let t = sum r s in fold (fun xs acc -> let x = hd xs in trappend (default [] (find x) r) (default [] (find x) s) |> sort cmp = find x t) t true)
    *)
    let inter a b =
      let each _ a' b' = match a', b' with
        | Some a', Some b' -> Some (min a' b')
        | _,       _       -> None
      in
      M.merge each a b
    (* TODO TESTS $Q inter
      Q.unit (fun () -> equal (inter empty empty) empty)
      genms (fun ms -> equal (inter ms ms) ms)
      genms (fun ms -> equal (inter ms empty) empty)
      Q.(pair genms genms) (fun (r,s) -> S.equal (inter r s |> support) (S.inter (support r) (support s)))
      Q.(pair genms genms) (fun (r,s) -> let t = inter r s in fold (fun x acc -> count x t = min (count x r) (count x s)) t true)
    *)
    let disjoint a b = inter a b = empty
    (*$Q disjoint
      Q.unit (fun () -> disjoint empty empty)
      Q.unit (fun () -> disjoint (singleton 1) empty)
      Q.int (fun x   -> not @@ disjoint (singleton x) (singleton x))
      Q.int (fun x   -> not @@ disjoint (singleton x) (of_list [x; succ x]))
      genms (fun ms  -> disjoint ms empty)
      genms (fun ms  -> disjoint empty ms)
      genms (fun ms  -> is_empty ms || disjoint ms ms |> not)
      Q.(pair genms genms) (fun (a,b) -> is_empty a || not @@ disjoint a (choose a |> foldr add b))
    *)
    let diff a b = M.partition (fun elt _ -> M.mem elt b) a |> snd
    (*$Q diff
      Q.unit (fun () -> diff empty empty |> equal empty)
      Q.int (fun n -> singleton n |> diff empty |> equal empty)
      Q.int (fun n -> singleton n |> flip diff empty |> equal (singleton n))
      genms (fun s -> diff s empty |> equal s)
      genms (fun s -> diff empty s |> equal empty)
      Q.(triple genms genms genms) (fun (a,b,c) -> diff c (inter a b) |> support = (union (diff c a) (diff c b) |> support))
      Q.(triple genms genms genms) (fun (a,b,c) -> diff c (union a b) |> equal (inter (diff c a) (diff c b)))
      Q.(triple genms genms genms) (fun (a,b,c) -> diff b a |> inter c |> equal (diff (inter b c) a))
      Q.(triple genms genms genms) (fun (a,b,c) -> diff c (diff b a) |> support = (union (inter c a) (diff c b) |> support))
      Q.(triple genms genms genms) (fun (a,b,c) -> diff b a |> inter c |> support = (inter b (diff c a) |> support))
    *)
    (* let compare x y = let c = Ord.compare in M.compare (fun (na,a) (nb,b) -> na = nb && comparison c (sort c a) (sort c b)) x y *)
    let compare x y = M.compare (fun (_,a) (_,b) -> comparison Ord.compare a b) x y
    (*$Q compare
      Q.unit (fun () -> compare empty empty = 0)
      Q.unit (fun () -> singleton 1 |> compare empty = -1)
      Q.unit (fun () -> singleton 1 |> flip compare empty = 1)
      genms (fun ms -> M.map (function (n,[]) -> n,[] | (n,v::[]) -> n,[v] | (n,x::y::zs) -> n, y::x::zs) ms |> compare ms = 0)
      genms (fun ms -> compare ms ms = 0)
      genms (fun ms -> match choose_opt ms with None -> true | Some xs -> foldr add ms xs |> compare ms = -1)
    *)
    (* assure that compare works even if 2 corresponding lists have the same elements but in a different order *)
    (*$Q compare
      genms (fun ms -> M.map (function (n,[]) -> n,[] | (n,v::[]) -> n,[v] | (n,x::y::zs) -> n, y::x::zs) ms |> compare ms = 0)
     *)
    let equal t1 t2 = compare t1 t2 = 0
    (*$Q equal
      Q.unit (fun () -> equal empty empty)
      Q.unit (fun () -> singleton 1 |> equal empty |> not)
      Q.unit (fun () -> singleton 1 |> flip equal empty |> not)
      genms (fun ms -> equal ms ms)
      genms (fun ms -> match choose_opt ms with None -> true | Some xs -> foldr add ms xs |> equal ms |> not)
    *)
    let multiplicity : elt -> t -> int = fun elt t -> match M.find elt t with exception Not_found -> 0 | n,_ -> n
    (*$Q multiplicity
      Q.(pair small_int genms) (fun (i,ms) -> add i ms |> multiplicity i > 0)
      Q.(pair small_int genms) (fun (i,ms) -> add i ms |> add i |> multiplicity i > 1)
      Q.(pair small_int genms) (fun (i,ms) -> add i ms |> add i |> add i |> multiplicity i > 2)
      Q.(pair small_int genms) (fun (i,ms) -> add i ms |> multiplicity i = multiplicity i ms + 1)
      genms (fun ms -> ms = empty || let x = choose ms |> hd in len (find x ms) = multiplicity x ms)
    *)
    let count = multiplicity
    let support t = M.keys t
    (*$Q support
      Q.unit (fun () -> support empty = [])
      Q.int (fun i -> singleton i |> support = [i])
      Q.(list int) (fun xs -> of_list xs |> support |> sortedly = sort_uniq Int.compare xs)
    *)
    let support_size t = M.cardinal t
    (*$Q support_size
      Q.unit (fun () -> support_size empty = 0)
      Q.int (fun i -> singleton i |> support_size = 1)
      genms (fun ms -> support_size ms = (support ms |> len))
    *)
    let subset a b = M.fold (fun elt (n,_) r -> r && n <= multiplicity elt b) a true
    (*$Q subset
      Q.unit (fun () -> subset empty empty)
      genms (fun ms -> subset empty ms)
      genms (fun ms -> subset ms ms)
      genms (fun ms -> is_empty ms || subset ms empty |> not)
      Q.(pair genms genms) (fun (a,b) -> if subset a b && subset b a then equal a b else true)
      Q.(pair genms genms) (fun (a,b) -> not @@ subset a b || List.subset (support a) (support b) && for_all (fun x -> count x a <= count x b) a)
      genms (fun a -> let b = a in not @@ subset a b || subset a b && List.subset (support a) (support b) && for_all (fun x -> count x a <= count x b) a)
      genms (fun b -> is_empty b || let a = remove (choose b |> hd) b in subset a b && List.subset (support a) (support b) && for_all (fun x -> count x a <= count x b) a)
    *)
    let iter f = M.iter (fun _ (_,s) -> f s)
    (*$Q iter
      Q.unit (fun () -> let r = ref true in iter (fun _ -> r := false) empty; !r)
      genms (fun ms -> let r = ref 0 in iter (fun xs -> r +:= len xs) ms; !r = size ms)
      genms (fun ms -> let r = ref [] in iter (fun xs -> List.iter (fun x -> r @:= x) xs) ms; sortedly !r = (to_list ms |> sortedly))
    *)
    let flatiter f = M.iter (fun _ (_,s) -> List.iter f s)
    (*$Q flatiter
      Q.unit (fun () -> let r = ref true in flatiter (fun _ -> r := false) empty; !r)
      genms (fun ms -> let r = ref 0 in flatiter (fun _ -> incr r) ms; !r = size ms)
      genms (fun ms -> let r = ref [] in flatiter (fun elt -> r @:= elt) ms; sortedly !r = (to_list ms |> sortedly))
    *)
    let map f t = M.fold (fun elt (_,xs) acc -> foldl (fun acc x -> add (f x) acc) acc xs) t empty
    (*$Q map
      Q.int (fun n -> map (k n) empty = empty)
      Q.int (fun n -> singleton n |> map (k n) = singleton n)
      Q.int (fun n -> let ms = singleton n |> add n in map (k n) ms = ms)
      genms (fun ms -> map (k 0) ms |> size = size ms)
      Q.(pair int int) (fun (n,m) -> singleton n |> map (k m) = singleton m)
      Q.(pair int int) (fun (n,m) -> let ms = singleton n |> add n in map (k m) ms = of_list [m;m])
      Q.(pair int int) (fun (n,m) -> let ms = singleton n |> add m in map (k m) ms = of_list [m;m])
    *)
    let fold f t init = M.fold (fun _ (_,xs) acc -> f xs acc) t init
    (*$Q fold
      Q.unit (fun () -> fold cons empty [] = [])
      genms (fun ms -> fold cons ms [] |> flatten |> sortedly = (to_list ms |> sortedly))
    *)
    let flatfold  f t init = M.fold (fun _ (_,xs) acc -> foldl (flip f) acc xs) t init
    (*$Q flatfold
      Q.unit (fun () -> flatfold cons empty [] = [])
      genms (fun ms -> flatfold cons ms [] |> sortedly = (to_list ms |> sortedly))
    *)
    let for_all f = M.for_all (fun _ (_,s) -> all f s)
    (*$Q for_all
      Q.unit (fun () -> for_all (k true) empty)
      Q.unit (fun () -> for_all (k false) empty)
      Q.int (fun n -> singleton n |> for_all (fun m -> m=n))
      Q.int (fun n -> singleton n |> for_all (fun m -> m=n+1) |> not)
      Q.int (fun n -> singleton n |> add n |> add n |> for_all (fun m -> m=n))
      Q.(pair int genms) (fun (n,ms) -> map (k n) ms |> for_all (fun m -> m=n))
      Q.(pair int genms) (fun (n,ms) -> map (k n) ms |> add ~++n |> for_all (fun m -> m=n) |> not)
    *)
    let exists f = M.exists (fun _ (_,s) -> exists f s)
    (*$Q exists
      Q.unit (fun () -> exists (k true) empty |> not)
      Q.unit (fun () -> exists (k false) empty |> not)
      Q.int (fun n -> singleton n |> exists (fun m -> m=n))
      Q.int (fun n -> singleton n |> exists (fun m -> m=n+1) |> not)
      Q.int (fun n -> singleton n |> add n |> add n |> exists (fun m -> m=n))
      Q.(pair int genms) (fun (n,ms) -> is_empty ms || map (k n) ms |> exists (fun m -> m=n))
      Q.(pair int genms) (fun (n,ms) -> is_empty ms || map (k n) ms |> add ~++n |> exists (fun m -> m=n))
    *)
    let filter f = M.filter (fun _ (_,s) -> List.exists f s)
    (*$Q filter
      Q.unit (fun () -> filter (k true) empty |> equal empty)
      Q.unit (fun () -> filter (k false) empty |> equal empty)
      Q.int (fun n -> singleton n |> filter ((=) n) |> equal (singleton n))
      Q.int (fun n -> singleton n |> filter ((=) ~++n) |> equal empty)
      genms (fun ms -> union (filter odd ms) (filter even ms) |> equal ms)
    *)
    (* TODO DESIGN this partition is based on keys; could be based on values; which is right? *)
    let partition f t = M.partition (fun elt _ -> f elt) t
    (*$Q partition
      Q.unit (fun () -> partition (k true) empty |> Pair.map (equal empty) |> fun (a,b) -> a && b)
      Q.unit (fun () -> partition (k false) empty |> Pair.map (equal empty) |> fun (a,b) -> a && b)
      Q.int (fun n -> singleton n |> partition ((=) n) |> fun (a,b) -> equal a (singleton n) && equal b empty)
      genms (fun ms -> partition (k true) ms |> uncurry union |> equal ms)
      genms (fun ms -> partition (k false) ms |> uncurry union |> equal ms)
      genms (fun ms -> partition even ms |> uncurry union |> equal ms)
    *)
    let cardinal t = M.fold (fun _ (n,_) m -> n+m) t 0
    (*$Q cardinal
      Q.unit (fun () -> cardinal empty = 0)
      Q.int (fun n -> singleton n |> cardinal = 1)
      Q.int (fun n -> singleton n |> add n |> cardinal = 2)
      Q.int (fun n -> singleton n |> add ~++n |> cardinal = 2)
      genms (fun ms -> (foldl (fun n x -> n + multiplicity x ms) 0 (support ms)) = cardinal ms )
      genms (fun ms -> let r = ref 0 in flatiter (fun _ -> incr r) ms; !r = cardinal ms)
    *)
    let size = cardinal
    let elements t = M.fold (fun _ (_,s) xs -> trappend s xs) t [] |> rev
    (*$Q elements
      Q.unit (fun () -> elements empty = [])
      Q.int (fun n -> singleton n |> elements = [n])
      Q.int (fun n -> singleton n |> add n |> add n |> elements = [n;n;n])
          genms (fun ms -> len (support ms) <= len (elements ms))
      genms (fun ms -> elements ms |> nub |> sortedly = (support ms |> sortedly))
    *)
    let to_list = elements
    (*$Q to_list
      genms (fun ms -> Stdlib.compare (to_list ms |> sortedly) (elements ms |> sortedly) = 0)
    *)
    (*$Q to_list;of_list
      genms (fun ms -> equal (to_list ms |> of_list) ms)
      Q.(list int) (fun xs -> of_list xs |> to_list |> sortedly = sortedly xs)
     *)
    let of_list = foldl (fun t x -> M.adjoin (fun x (n,xs) -> succ n, x::xs) (0,[]) x x t) empty
    (*$Q of_list
      Q.unit (fun () -> of_list [] |> equal (foldr add empty []))
      Q.(list int) (fun xs -> of_list xs |> support |> sortedly = sortedly xs)
     *)
    let choose t = M.choose t |> snd |> snd
    (*$Q choose
      Q.unit (fun () -> not @@ succeeds choose empty)
      Q.int (fun n -> singleton n |> choose = [n])
      genms (fun ms -> match choose ms with xs -> all (flip mem ms) xs | exception Not_found -> true)
     *)
    let choose_opt t = catch choose t
    (*$Q choose_opt
      Q.unit (fun () -> choose_opt empty = None)
      Q.int (fun n -> singleton n |> choose_opt = Some [n])
      genms (fun ms -> match choose_opt ms with Some xs -> all (flip mem ms) xs | None -> true)
     *)
    let min_elt t = M.min_binding t |> snd |> snd
    (*$Q min_elt
      Q.unit (fun () -> succeeds min_elt empty |> not)
      Q.int (fun n -> singleton n |> min_elt = [n])
      Q.int (fun n -> singleton n |> min_elt = (singleton n |> max_elt))
      Q.(pair int int) (fun (n,m) -> singleton n |> add m |> min_elt = [min n m])
      genms (fun ms -> equal ms empty || succeeds min_elt ms)
      genms (fun ms -> equal ms empty || min_elt ms <= max_elt ms)
      genms (fun ms -> equal ms empty || let xs = min_elt ms in for_all (fun n -> all (fun x -> x <= n) xs) ms)
    *)
    let min_elt_opt t = catch min_elt t
    (*$Q min_elt_opt
      Q.unit (fun () -> min_elt_opt empty = None)
      Q.int (fun n -> singleton n |> min_elt_opt = Some [n])
      Q.int (fun n -> singleton n |> min_elt_opt = (singleton n |> max_elt_opt))
      Q.(pair int int) (fun (n,m) -> singleton n |> add m |> min_elt_opt = Some [min n m])
      genms (fun ms -> equal ms empty || match min_elt_opt ms with Some xs -> hd xs <= hd (max_elt ms) | None -> assert false)
      genms (fun ms -> equal ms empty || let m = min_elt_opt ms |> Option.get in for_all (fun n -> hd m <= n) ms)
    *)
    let max_elt t = M.max_binding t |> snd |> snd
    (*$Q max_elt
      Q.unit (fun () -> succeeds max_elt empty |> not)
      Q.int (fun n -> singleton n |> max_elt = [n])
      Q.int (fun n -> singleton n |> max_elt = (singleton n |> min_elt))
      Q.(pair int int) (fun (n,m) -> singleton n |> add m |> max_elt = [max n m])
      genms (fun ms -> equal ms empty || succeeds max_elt ms)
      genms (fun ms -> equal ms empty || max_elt ms >= min_elt ms)
      genms (fun ms -> equal ms empty || let m = max_elt ms in for_all (fun n -> hd m >= n) ms)
    *)
    let max_elt_opt t = catch max_elt t
    (*$Q max_elt_opt
      Q.unit (fun () -> max_elt_opt empty = None)
      Q.int (fun n -> singleton n |> max_elt_opt = Some [n])
      Q.int (fun n -> singleton n |> max_elt_opt = (singleton n |> min_elt_opt))
      Q.(pair int int) (fun (n,m) -> singleton n |> add m |> max_elt_opt = Some [max n m])
      genms (fun ms -> equal ms empty || match max_elt_opt ms with Some xs -> hd xs >= hd (max_elt ms) | None -> assert false)
      genms (fun ms -> equal ms empty || let m = max_elt_opt ms |> Option.get in for_all (fun n -> hd m >= n) ms)
    *)
    let split : elt -> t -> t * bool * t = fun elt t -> let lt,opt,gt = M.split elt t in lt, something opt, gt
    (*$Q split
      Q.int (fun n -> match split n empty with lt,false,gt -> equal empty lt && equal empty gt | _ -> false)
      Q.int (fun n -> match split n (singleton n) with lt,true,gt -> equal empty lt && equal empty gt | _ -> false)
      Q.int (fun n -> match split n (singleton n |> add n) with lt,true,gt -> equal empty lt && equal empty gt | _ -> false)
      Q.int (fun n -> match split n (singleton n |> add ~++n) with lt,true,gt -> equal empty lt && size gt = 1 | _ -> false)
      Q.int (fun n -> match split n (singleton n |> add ~--n) with lt,true,gt -> size lt = 1 && equal empty gt | _ -> false)
      genms (fun ms -> is_empty ms || let elts = choose ms in match split (hd elts) ms with lt,true,gt -> for_all (fun n -> n < hd elts) lt && for_all (fun n -> n > hd elts) gt | _ -> false)
     *)
     let find elt t = M.find elt t |> snd
    (*$Q find
      Q.int (fun n -> not @@ succeeds (find n) empty)
      Q.int (fun n -> singleton n |> find n = [n])
      Q.int (fun n -> singleton n |> add n |> add n |> find n = [n;n;n])
      Q.(pair int genms) (fun (elt,t) -> (len (default [] (find elt) t)) = (multiplicity elt t))
      genms (fun t -> let t = add 0 t in let elt = choose t |> hd in ((len (find elt t)) = (multiplicity elt t)))
      genms (fun ms -> equal empty ms || let elts = choose ms in all (fun elt -> match find elt ms with [] -> false | xs -> all (fun x -> Int.compare elt x = 0) xs) elts)
      genms (fun ms -> equal empty ms || let elts = choose ms in all (fun elt -> exists (fun n -> match find n ms with [] -> false | xs -> all (fun x -> Int.compare elt x = 0) xs) ms) elts)
     *)
     let find_opt elt t = catch (find elt) t
     let find_first f t = M.find_first f t |> snd |> snd
    (*$Q find_first
      Q.unit (fun () -> succeeds (find_first (k true))  empty |> not)
      Q.unit (fun () -> succeeds (find_first (k false)) empty |> not)
      Q.int (fun n -> singleton n |> find_first ((=) n) = [n])
      genms (fun ms -> equal ms empty || let elts = choose ms in all (fun elt -> find_first (fun n -> n >= elt) ms |> all (fun x -> x=elt)) elts)
     *)
     let find_first_opt f t = catch (find_first f) t
     let find_last f t = M.find_last f t |> snd |> snd
    (*$Q find_last
      Q.unit (fun () -> succeeds (find_last (k true))  empty |> not)
      Q.unit (fun () -> succeeds (find_last (k false)) empty |> not)
      Q.int (fun n -> singleton n |> find_last ((=) n) = [n])
      genms (fun ms -> equal ms empty || let elts = choose ms in all (fun elt -> find_last (fun n -> n <= elt) ms |> all (fun x -> x=elt)) elts)
     *)
     let find_last_opt f t = catch (find_last f) t
     let to_seq_from elt t = M.to_seq_from elt t |> Seq.map (fun (_,(_,xs)) -> xs)
     (*$Q to_seq_from
       Q.int (fun n -> to_seq_from n empty () = Seq.Nil)
       Q.(pair int genms) (fun (n,ms) -> to_seq_from n ms |> Seq.to_list |> all (len >> (<) n))
       Q.(pair int genms) (fun (n,ms) -> to_seq_from n ms |> Seq.to_list |> sort Stdlib.compare = (repetitions ms |> List.filter (hd >> (<=) n) |> sort Stdlib.compare))
      *)
     let to_seq t = M.to_seq t |> Seq.map (fun (_,(_,xs)) -> xs)
     (*$Q to_seq
       Q.unit (fun () -> to_seq empty () = Seq.Nil)
       genms (fun ms -> to_seq ms |> Seq.to_list |> sort Stdlib.compare = (repetitions ms |> sort Stdlib.compare))
      *)
     let add_seq seq t = Seq.fold_left (flip add) t seq
     (*$Q add_seq
       Q.unit (fun () -> add_seq Seq.empty empty = empty)
       Q.(list int) (fun xs -> List.to_seq xs |> flip add_seq empty = of_list xs)
      *)
     let of_seq seq = add_seq seq empty
     (*$Q of_seq
       Q.unit (fun () -> of_seq Seq.empty = empty)
       Q.(list int) (fun xs -> List.to_seq xs |> of_seq = of_list xs)
      *)
     let addwith f elt t = add (f elt) t
     (*$Q addwith
       Q.int (fun n -> addwith id n empty = singleton n)
       Q.int (fun n -> addwith (k ~++n) n empty = singleton ~++n)
      *)
     let addwhen f elt t = if f elt then add elt t else t
     (*$Q addwhen
       Q.(pair int int) (fun (n,max) -> addwhen (fun x -> x < max) n empty = if n < max then singleton n else empty)
      *)
     (** [(random ?size r ())] is a random multiset of size [(size ())]
         (default: [< 100]), and each element is given by [(r ())]. *)
     let random ?size r () =
       let n = match size with None -> Random.int 100 | Some f -> f () in
       foldil (fun acc _ -> add (r ()) acc) empty (1,n)
     (* TODO TESTS *)
     let to_string ?(sep=", ") f t =
       flatfold (conswith f) t [] |> rev |> join ~sep
     (* TODO TESTS *)
     let print ?sep f t = to_string ?sep f t |> print_endline (* TODO also add optional Format.formatter *)

  end
  (**/**)

  module Make (Ord : OrderedType) : S with type elt = Ord.t = Make' (Ord)
end (*$>*)

(** {1:stacks Stacks} *)

(** Purely functional stacks.

    Basically, a set of functions that treat lists as stacks, and a
    private type to enhance type safety.

    The [hd] of the list is the top of the stack.

    Conversion between lists and stacks is essentially free, and you
    can coerce stacks to lists for pattern matching:

    {[ match ((empty |> push 1) :> int list) with [] -> ... ]}
*)
module type STACK = sig

  (** The type of stacks. *)
  type 'a t = private 'a list

  (** [compare] is the version of [compare] for {!t}'s. *)
  val compare : 'a t -> 'a t -> int

  (** The empty stack. *)
  val empty : 'a t

  (** [(size s)] is the number of items on the stack [s]. *)
  val size : 'a t -> int

  (** [(push x s)] returns a new stack with [x] on top of it.*)
  val push : 'a -> 'a t -> 'a t

  (** [dup] is Forth's [DUP] i.e. it pushes a copy of the top item of [s] onto [s]. *)
  val dup  : 'a t -> 'a t

  (** [(top s)] is the top item on stack [s]. *)
  val top : 'a t -> 'a option

  (** [drop] is [(pop $ snd)]: it throws away the top item on [s].

      Invariant: [(push x s |> drop) = s]
  *)
  val drop : 'a t -> 'a t

  (** [(pop s)] is [(top s, drop s)]

      Invariants:
      - [(pop empty) = (None, empty)]
      - [(push x s |> pop) = (Some x,s)]
  *)
  val pop  : 'a t -> 'a option * 'a t

  (** [swap] is Forth's [SWAP].

      Applying [swap] to the empty list or to a singleton list has no effect.

      Invariants:
      - [(push x empty           |> swap |> pop) = (None,   empty)]
      - [(push x s     |> push y |> swap |> pop) = (Some x, push y s)]
  *)
  val swap : 'a t -> 'a t

  (** [(map f t)] maps [f] across each element in the stack [t]. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** [(fold f init t)] folds [f] across the stack [t] with [init] as the initial accumulator. *)
  val fold  : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  (** [(of_list (x::[])) = (push x empty)] *)
  val of_list : 'a list -> 'a t

  (** [(push 1 empty |> push 2 |> to_list) = [2;1]] *)
  val to_list : 'a t -> 'a list

  (** [(random ?size r ())] is a random stack of size [(size ())]
      (default: [< 100]), and each element is given by [(r ())]. *)
  val random : ?size:(unit -> int) -> (unit -> 'a) -> unit -> 'a t
end

(** See {!STACK}. *)
(* TODO TESTS *)
module Stack : STACK = struct
  type 'a t = 'a list
  let compare = compare
  let empty = []
  let size = len
  let push x t = x::t
  let top  = function [] -> None | x::_ -> Some x
  let dup  = function [] ->      [] |    x::xs ->   x::x::xs
  let drop = function [] -> [] | _::xs -> xs
  let pop t = top t, drop t
  let swap = function [] -> [] | x::y::xs -> y::x::xs | xs -> xs
  let map = map
  let fold = foldl
  let of_list l = l
  let to_list t = t
  let random = List.random
  (* TODO TESTS *)
end

(**/**)
module Queue' = Queue
(**/**)

(** {1:queues Queues} *)

(** Purely functional FIFO queues.

    This is the data structure due to F. Warren Burton, "An Efficient
    Functional Implementation of FIFO Queues", {i Information
    Processing Letters}, 1982.
*)
(* TODO TESTS *)
module Queue :
sig
  type 'a t
  val compare : 'a t -> 'a t -> int
  val empty : 'a t
  val null : 'a t -> bool
  val size : 'a t -> int
  val enq : 'a -> 'a t -> 'a t
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a option
  val deq : 'a t -> 'a option * 'a t
  val pop : 'a t -> 'a option * 'a t
  val drop : 'a t -> 'a t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val random : ?size:(unit -> int) -> (unit -> 'a) -> unit -> 'a t
end = struct
  (** The type of queues. *)
  type 'a t = 'a list * 'a list

  (** [compare] is the version of [compare] for {!t}'s. *)
  let compare = compare
  (** The empty queue. *)
  let empty = [],[]
  (** [(size q)] is the the number of elements in the queue [q]. *)
  let size (f,b) = List.length f + List.length b
  (** [(null q)] is [true] iff [(size q = 0)]. *)
  let null = function
  | [], [] -> true
  | _,  _  -> false
  (** [(enq x q)] enqueues [x] onto queue [q].*)
  let enq x (front, back) = front, (x::back)
  (** [push] is {!enq}. *)
  let push = enq
  (** [peek] is [(deq $ fst)]. *)
  let peek = function
  | x::_,  _    -> Some x
  |    [], back -> rev back |> head
  (** [(deq q)] is [(peek q, drop q)]. *)
  let deq = function
  | x::xs, back -> Some x, (xs,back)
  |    [], back ->
      match List.rev back with
      |    [] -> None,   ([], [])
      | x::xs -> Some x, (xs, [])
  (** [pop] is {!deq}. *)
  let pop : 'a t -> 'a option * 'a t = deq
  (** [drop] is [(deq $ snd)]: it throws away the first (oldest) item
      in the queue.

      {[ (push x q |> drop) = q ]}
  *)
  let drop t = pop t |> snd
  (** [(of_list xs) = (foldr enq empty xs)] but is tail-recursive.

      If [(xs <> [])] then [(hd xs)] becomes the first (oldest) element
      of the queue.

      {[ of_list [1;2] |> deq |> fun (Some 1, q) -> deq q = (Some 2, empty) ]}
  *)
  let of_list l = [], rev l
  (** [(push 1 empty |> push 2 |> to_list) = [2;1]] *)
  let to_list = function (front, back) -> front @ rev back
  (** [(random ?size r ())] is a random queue of size [(size ())]
      (default: [< 100]), and each element is given by [(r ())]. *)
  let random ?size r () = List.random ?size r () |> of_list
  (* TODO TESTS *)
end

(** The Standard Library's imperative queues. *)
module IQueue = Queue'

(** {1:rosetrees Rose Trees} *)

(** N-ary (Rose) trees. *)
(* TODO TESTS *)
module Rose = struct
  (** The type of Rose trees. *)
  type 'a t = Node of ('a * 'a t list)

  (** [(tree v ts)] is the tree carrying value [v] with children [ts]. *)
  let tree v ts = Node (v,ts)

  (** [(unit v)] is the unit tree carrying value [v] and no children. *)
  let unit v = tree v []

  (** [(unfold f seed)] builds a {!t} by recusively expanding [(f seed)].

      The recursion is terminated when [(snd << f) = []].

      This example builds a tree isomorphic to the directory structure
      rooted at [dir]:
      {[ let f d =
  Filename.(basename d, default [||] Sys.readdir d |> Array.to_list |> map (concat d)) in
unfold f dir ]}

  *)
  let rec unfold f init =
    let (v, ts) = f init in
    Node (v, (map (unfold f) ts))

  (* TODO ENHANCE add option for tree size to replace 10 *)
  (** [(random r ())] is a random rose tree; each element is given by [(r ())]. *)
  let random r () =
    let each n = match Random.int n with
      | 0  -> r (), []
      | n' -> r (), 1--n'
    in
    Random.int 10 + 1 |> unfold each
  (* TODO TESTS *)

  (** [(fold f init)] folds [f] across a {!t} with [init] as the
      initial accumulator.

      Example:
      - {!size} [= (fold (fun n _ -> succ n) 0)]
  *)
  let rec fold f z (Node (v, ts)) = foldl (fold f) (f z v) ts

  (** [size] returns the number of nodes in a {!t}. *)
  let size t = fold (fun n _ -> succ n) 0 t

  (** [(map f)] maps [f] across all the values in a {!t}. *)
  let rec map f (Node (v, ts)) =
    Node (f v, List.map (map f) ts)

  (** [flatten] does a pre-order traversal of a {!t} and returns the
      values in a list. *)
  let flatten t =
    let rec inner (Node (v, ts)) acc =
      v :: foldr inner acc ts
    in inner t []

  (** [(draw f t)] "returns a neat two dimentional drawing of [t];
      shamelessly ripped of from Haskell 'containers' package."

      Each element of the list is a line of output; the list is
      suitable to pass to [(iter print_endline)].

      @author Sergei Lebedev
      @see <https://gist.github.com/superbobry/1838676> https://gist.github.com/superbobry/1838676
  *)
  let rec draw f (Node (v, ts)) =
    let shift start other =
      let rec inner acc = function
      | [] -> List.rev acc
      | l :: lines ->
          let markup = if acc = [] then start else other in
          inner (String.concat "" [markup; l] :: acc) lines
      in inner []
    in
    let rec inner = function
    | [] -> []
    | t :: ts ->
        let (start, other) = match ts with
        | [] -> ("`- ", "   ")
        | _  -> ("+- ", "|  ")
        in
        let lines = draw f t in
        "|" :: shift start other lines @ inner ts
    in f v :: inner ts
end

(** {1:leftist Leftist} *)

(** A leftist heap; can be a min-heap or a max-heap.
    @see <http://typeocaml.com/2015/03/12/heap-leftist-tree/> http://typeocaml.com/2015/03/12/heap-leftist-tree/
*)
(* TODO TESTS *)
module Leftist = struct

  (** The type of leftist heaps. *)
  module type S = sig
    type elt
    type t
    val empty : t
    val singleton : elt -> t
    val merge : t -> t -> t
    val insert : elt -> t -> t
    val push : elt -> t -> t
    val get : t -> elt
    val delete : t -> t
    val pop : t -> elt * t
    val fold : (elt -> 'a -> 'a) -> 'a -> t -> 'a
    val iter : (elt -> unit) -> t -> unit
    val to_list : t -> elt list
    val of_list : elt list -> t
    val random : ?size:(unit -> int) -> (unit -> elt) -> unit -> t
  end

  (** The type of heaps. *)
  type m = Min | Max

  (**/**)
  let cmp m i = match m with Min -> i > 0 | Max -> i < 0
  (**/**)

  (** [Min] is module that constructs a min-heap. *)
  module Min = struct let m = Min end

  (** [Max] is module that constructs a max-heap. *)
  module Max = struct let m = Max end

  module type MinMax = sig val m : m end

  (** [(Make (M) (OT))] is a leftist min- or max-heap on the ordered type [OT]; see {!S};
      [(Make (Min) (OT))] is a min-heap on [OT], while [(Make (Max) (OT))] is a max-heap on [OT].
  *)
  module Make (M : MinMax) (Ord : OrderedType) : S with type elt = Ord.t = struct

    (** The type of heap elements. *)
    type elt = Ord.t

    (** The type of heaps. *)
    type t =
      | Empty
      | Node of t * elt * t * int

    (** [empty] is the empty heap. *)
   let empty = Empty
    (** [(singleton k)] is the heap containing only [k]. *)
    let singleton k = Node (Empty, k, Empty, 1)
    let rank = function Empty -> 0 | Node (_,_,_,r) -> r

    (** [(merge h1 h2)] merges two heaps. *)
    let rec merge t1 t2 =
      match t1,t2 with
      | Empty, t | t, Empty -> t
      | Node (l, k1, r, _), Node (_, k2, _, _) ->
        if cmp M.m (Ord.compare k1 k2) then merge t2 t1 (* switch merge if necessary *)
        else
          let merged = merge r t2 in (* always merge with right *)
          let rank_left = rank l and rank_right = rank merged in
          if rank_left <= rank_right then Node (l, k1, merged, rank_right+1)
          else Node (merged, k1, l, rank_left+1) (* left becomes right due to being shorter *)

    (** [(insert x h)] inserts [x] into heap [h]. *)
    let insert x t = merge (singleton x) t
    (** [push] is [insert]. *)
    let push = insert
    (** [(get h)] returns the maximum or minimum element (for a maxheap or minheap, respectively) in the heap [h].

        @raise Failure if heap is empty
    *)
    let get = function
      | Empty              -> failwith "empty"
      | Node (_, k, _, _) -> k

    (** [(delete h)] returns the heap which is [h] minus its maximum
        or minimum element (for a maxheap or minheap, respectively).

        @raise Failure if heap is empty
    *)
    let delete = function
      | Empty              -> failwith "empty"
      | Node (l, _, r, _) -> merge l r
    (** [(pop h)] is [(get h, delete h)].

        @raise Failure if heap is empty
    *)
    let pop = function
      | Empty              -> failwith "empty"
      | Node (l, k, r, _) -> k, merge l r
    (** [(fold f init t)] folds [f] over the elements of [t] in order. *)
    let rec fold f acc h = match pop h with
    | exception Failure _ -> acc
    | k, h'               -> fold f (f k acc) h'
    (** [(iter f t)] iterates [f] over the elements of [t] in order. *)
    let iter f = fold (fun x () -> f x) ()
    (** [(to_list t)] is the list containing the elements of [t] in
        decreasing or increasing order (for a maxheap or minheap,
        respectively).

        Example: if [H] is a minheap, then [((H.of_list >> H.to_list)
        = (List.sort compare))]; if [H] is a maxheap, then [((H.of_list >> H.to_list)
        = (List.sort (flip compare)))].

     *)
    let to_list = fold cons [] $ rev
    (** [(of_list l)] returns the heap containing all the elements of list [l]. *)
    let of_list = function
      | []    -> failwith "of_list"
      | x::xs -> foldl (flip insert) (singleton x) xs
    (** [(random ?size r ())] is a random heap of size [(size ())]
        (default: [< 100]), and each element is given by [(r ())]. *)
    let random ?size r () = List.random ?size r () |> of_list
    (* TODO TESTS *)
  end
end

(** {1:system System} *)

(** [argv0] is [(Sys.argv.[0])] or else [Sys.executable_name]. *)
let argv0 = match Array.to_list Sys.argv with [] -> Sys.executable_name | a::_ -> a

(** [argv] is [(Array.to_list Sys.argv |> List.tl)], but is [[]] if [(Sys.argv) = [||]]. *)
let argv = match Array.to_list Sys.argv with [] -> [] | _::a -> a
(**/**)
let forward_message = ref (fun ?ff ?prefix str -> assert false)
(**/**)

(** [(syserrors ?exit ?message f x)] is [(Sys.catch_break true; f x)];
    any [Sys_error] or [Unix.Unix_error] exceptions are printed
    "more attractively" (via {!Exn.to_string}) to [stderr].

    We call [(Sys.catch_break true)] because otherwise [SIGINT] (the
    signal sent by a control-C) terminates the program without
    allowing finalizers to finalize!

    If [(~exit = true)], the program terminates via [Stdlib.exit];
    otherwise, the program does {i not} exit at all -- [false] is good
    for testing in the top-level (default value for [~exit]: [true]).

    Error messages are printed via [~message] (default:
    {!Message.message}).  [~message] will be called with various exit
    statuses for the following exceptions as follows:
    - [Sys.Break]: [~exit:1]
    - {!Failed_prerequisite}: [~exit:3]
    - any other exception: [~exit:125]

    "More attractively" means, for example, that an attempt to open a
    non-existent file would look like: [/etc/passwdx: No such file or directory]

    rather than the usual OCaml:
    {[ Fatal error: exception Sys_error("/etc/passwdx: No such file or directory")
Raised by primitive operation at unknown location
Called from unknown location ]}

    Typical usage:

    {[let main () = iter (readfile $ write stdout) argv
    let () = syserrors main ()]}
 *)
let syserrors ?(exit=true) ?message f x =
  let message = match message with None -> !forward_message | Some f -> f in
  let exitish n = if exit then Stdlib.exit n in
  let ff = Format.stderr in
  Sys.catch_break true;
  match f x with
  | exception Sys.Break -> message ~prefix:"FATAL" "interrupt"; exitish 1
  | exception Failed_prerequisite cmds ->
     iter (fun s -> sprintf "missing prerequisite: %s" s |> message ?prefix:None ~ff) cmds; exitish 3
  | exception exn -> Exn.to_string exn |> message ~prefix:"FATAL"; exitish 125
  | () -> ()
(*$Q syserrors
  Q.unit (fun () -> ignore syserrors; succeeds (!forward_message ~ff:Format.null) "")
  Q.unit (fun () -> () = syserrors ~exit:false id ())
*)

(** [version_of_string] converts common version number strings to values that can be easily compared.

    N.B. due to the variation in version number formats, one can't
    typically expect to meaningfully compare the versions of two
    different programs, but fortunately one {i can} typically compare newer
    and older versions of the same program.

    Examples:
    - [(version_of_string "1.1.11+50+gb685338-1" = [Some 1; Some 1; Some 11; Some 50; None; Some 1])]
    - [(on (<) version_of_string "1.1.11+50+gb685338-1" "1.2.11+50+gb68531238-1")]
*)
let version_of_string str = split ~sep:".-+" str |> map (catch int_of_string)
(*$Q version_of_string
  Q.unit (fun () -> [] = version_of_string "")
  Q.(list_of_size Gen.(int_range 1 10) (string_gen_of_size Gen.(int_range 1 6) Gen.numeral)) (fun xs -> xs |> join ~sep:(choose 1 String.(explode ".-+" |> List.map (make 1)) |> hd) |> version_of_string |> len = len xs )
  Q.(list_of_size Gen.(int_range 1 10) (string_gen_of_size Gen.(int_range 1 6) Gen.numeral)) (fun xs -> xs |> join ~sep:(choose 1 String.(explode ".-+" |> List.map (make 1)) |> hd) |> version_of_string |> all something)
  Q.(make Gen.(list_size (int_range 1 10) ((string_size ~gen:(String.('a'--'z') |> oneofl) (int_range 1 10))))) (fun xs -> xs |> join ~sep:(choose 1 String.(explode ".-+" |> List.map (make 1)) |> hd) |> version_of_string |> all nothing)
 *)

(** {1:files Files} *)

(** Additional functions on files and filenames. *)
(* TODO TESTS *)
module File = struct (*$< File *)

  (** {1 File Names} *)

  (** [sep] is [Filename.dir_sep]. *)
  let sep = Filename.dir_sep

  (** [(join dir name)] is like [(Filename.concat dir name)] except
      that if [name] is absolute, it is returned as is. *)
  let join dir name =
    if Filename.is_relative name
    then Filename.concat dir name
    else name

  (** [(components ?sep path)] is the list of the leading directory
      components (and possible final file component) of [path], as
      separated by [sep] (default: [Filename.dir_sep]).

      Invariant: [(not (Filename.is_relative p)) && (hd (components p) = sep)]

      Example: [(components "/etc/passwd" = ["/"; "etc"; "passwd"])]
   *)
  let components ?(sep=Filename.dir_sep) path =
    if String.length sep <> 1
    then invalid_arg "components"
    else if path = ""
    then []
    else (if path.[0] = sep.[0] then (* abs *) [sep] else (* rel *) []) @ split ~sep path
  (*$T components
    components "" = []
   *)
  (*$Q components
    Q.(string_gen Gen.(char_range 'a' 'z')) (fun p -> p = "" || hd (components p) = p)
   *)
  (*$Q components;coalesce
    Q.(small_list (string_gen Gen.(char_range 'a' 'z'))) (fun cs -> let cs = filter ((<>) "") cs in cs = [] || let p = coalesce cs in Filename.is_relative p || (hd (components p) = sep))
   *)

  (** [(coalesce ?sep list)] is the filename corresponding to the
      directory components (and possible final file component) of
      [path] separated by [sep] (default: [Filename.dir_sep]).

      Any list returned by {!components} is guaranteed to be valid
      input to [coalesce], but other lists may raise [Invalid_arg]:

      - none of the components may be [""]
      - none of the component strings in the {!tl} may contain [sep], except:
      - [sep], exactly, may be the {!hd}

      Example: [(coalesce ["/"; "etc"; "passwd"] = "/etc/passwd")]
   *)
  let coalesce ?(sep=Filename.dir_sep) list =
    let slashy = succeeds (find (String.substr sep >> Option.to_bool)) in
    match list with
    | []                        -> invalid_arg "coalesce"
    | _::cs when mem sep cs     -> invalid_arg "coalesce"
    | _::cs when slashy cs      -> invalid_arg "coalesce"
    | c::cs when mem "" (c::cs) -> invalid_arg "coalesce"
    | c::cs when c = sep        -> c ^ String.join ~sep cs
    | c::_  when slashy [c]     -> invalid_arg "coalesce"
    | cs      (* else *)        ->     String.join ~sep cs
  (* tests: see components *)
  (*$T coalesce
    not (succeeds (coalesce ~sep) [])
    coalesce ~sep [sep] = sep
   *)
  (*$Q coalesce
    Q.(pair (small_list (string_gen Gen.(char_range 'a' 'z'))) (small_list (string_gen Gen.(char_range 'a' 'z')))) \
      (fun (a,z) -> a = [] || not (succeeds (coalesce ~sep) (a @ [sep] @ z)))
    Q.(pair (small_list (string_gen Gen.(char_range 'a' 'z'))) (small_list (string_gen Gen.(char_range 'a' 'z')))) \
      (fun (a,z) -> not (succeeds (coalesce ~sep) (a @ [""] @ z)))
   *)

  (* TODO DOC *)
  let absolutify dir =
    if not @@ Filename.is_relative dir
    then dir
    else if dir = Filename.current_dir_name
    then Sys.getcwd ()
    else join (Sys.getcwd ()) dir
  (* TODO TESTS *)

  (** [(squiggle ?home fn)] expands the traditional Unix leading
      "~/" in filename [fn] to the home directory of the user running the program.

      The [home] function is called to get the user's home directory.
      The default is {!Unix.home} (which uses [Unix.getpwuid]); another candidate might be:

      {[ default (Unix.home ()) Sys.getenv "HOME" ]}

      to use the value of the environment variable, if defined, but
      fall back to the passwd database if not.

   *)
  let squiggle ?home fn =
    let sep = Filename.dir_sep in
    let split = split ~elide:false ~sep in
    let join = String.join ~sep in
    let user name = default ("~" ^ name) Unix.(fun n -> (getpwnam n).pw_dir) name in
    match split fn with
    |     []        -> assert false (* via split ~elide:false *)
    | "~" :: rest   -> (Option.call Unix.(((getpwuid (geteuid ())).pw_dir |> split) |> join) home () |> split) @ rest |> join
    | u   :: rest   -> match u.[0] with
                       | '~'         -> (String.(sub u 1 (length u-1)) |> user) :: rest |> join
                       | _           -> u :: rest |> join
                       | exception _ -> u :: rest |> join
  (*$=
    "" (squiggle "")
    "/" (squiggle "/")
    "/etc" (squiggle "/etc")
    "/etc/passwd" (squiggle "/etc/passwd")
    "" (squiggle ~home:(k "") "~")
    "foo" (squiggle ~home:(k "foo") "~")
   *)
  (*$T squiggle
    not (squiggle "~" = "~")
    (squiggle "~" |> String.len > 1)
   *)

  (**/**)
  let prng = lazy (Random.State.make_self_init ())
  (**/**)

  (** [temp_file_name ?temp_dir prefix suffix] returns a filename
      suitable for an {i INSECURE} temporary file.

      This function may be useful for creating temp files that are not
      the ordinary files that [Filename.temp_file] and
      [Filename.open_temp_file] make, such as fifos, unix domain
      sockets, or even just directories.

      Remember that securely creating (and possibly opening) such a
      temp file is more complex then merely doing:
      - [mkfifo (temp_file_name "foo" "") 0o400]

      This code was extracted from the source code of OCaml's [Filename] module.
   *)
  let temp_file_name ?(temp_dir = Filename.get_temp_dir_name ()) prefix suffix =
    let rnd = (Random.State.bits (Lazy.force prng)) land 0xFFFFFF in
    Filename.concat temp_dir (sprintf "%s%06x%s" prefix rnd suffix)
  (*$Q temp_file_name
    Q.(pair (make ~print:Print.string Gen.(string_size ~gen:(String.('a'--'z') |> oneofl) (int_range 1 10))) (make ~print:Print.string Gen.(string_size ~gen:(String.('a'--'z') |> oneofl) (int_range 1 10)))) String.(fun (p,s) -> on (+) len p s + 6 = len (temp_file_name p s |> Filename.basename))
    Q.(pair (make ~print:Print.string Gen.(string_size ~gen:(String.('a'--'z') |> oneofl) (int_range 1 10))) (make ~print:Print.string Gen.(string_size ~gen:(String.('a'--'z') |> oneofl) (int_range 1 10)))) String.(fun (p,s) -> let t = temp_file_name p s |> Filename.basename in prefix p t && suffix s t)
   *)

  (** {1 Predicates} *)

  (** [(exists fn)] is [true] iff the file [fn] exists. *)
  let exists fn = Unix.(succeeds (access fn) [F_OK])

  (** [(readable fn)] is [true] iff the file [fn] is readable. *)
  let readable fn = Unix.(succeeds (access fn) [R_OK])

  (** [(writeable fn)] is [true] iff the file [fn] is writeable. *)
  let writeable fn = Unix.(succeeds (access fn) [W_OK])

  (** [(executable fn)] is [true] iff the file [fn] is executable. *)
  let executable fn = Unix.(succeeds (access fn) [X_OK])

  (**/**)
  module Stat2 = struct
    open Unix
    (* TODO TESTS *)
    let dev    stat fn = (stat fn).st_dev
    let ino    stat fn = (stat fn).st_ino
    let kind   stat fn = (stat fn).st_kind
    let perm   stat fn = (stat fn).st_perm
    let nlink  stat fn = (stat fn).st_nlink
    let uid    stat fn = (stat fn).st_uid
    let gid    stat fn = (stat fn).st_gid
    let rdev   stat fn = (stat fn).st_rdev
    let size   stat fn = (stat fn).st_size
    let atime  stat fn = (stat fn).st_atime
    let mtime  stat fn = (stat fn).st_mtime
    let ctime  stat fn = (stat fn).st_ctime
  end
  (**/**)

  (** {2 Types of Files} *)

  (** Predicates testing the type of a file. *)
  module Is = struct (*$<Is *)
    (** These functions all follow symlinks, with the exception of
        {!Is.link}.  The rationale is that, for example. a symlink to a
        directory is usually just as good when you need a directory, so
        [dir fn] should return [true].

        In the case where this isn't correct, you will have to call, e.g.:
        - [(fork (&&) (not<<link) dir)]
     *)

    open Unix
    (** [(reg fn)] is [true] iff [fn] is a regular file. *)
    let reg fn = (stat fn).st_kind = S_REG
    (*$T reg
      reg "/etc/passwd"
     *)

    (** [(dir fn)] is [true] iff [fn] is a directory. *)
    let dir fn = (stat fn).st_kind = S_DIR
    (*$T dir
      dir "/etc"
     *)

    (** [(chr fn)] is [true] iff [fn] is a character special file. *)
    let chr fn = (stat fn).st_kind = S_CHR
    (*$T chr
      chr "/dev/null"
     *)

    (** [(blk fn)] is [true] iff [fn] is a block special file. *)
    let blk fn = (stat fn).st_kind = S_BLK
    (* TODO TESTS *)

    (** [(link fn)] is [true] iff [fn] is a symbolic link. *)
    let link fn = (lstat fn).st_kind = S_LNK
    (*$T link
      not @@ Unix.has_symlink () || finalize (fun fn -> Unix.symlink "/etc/passwd" fn; link fn) Sys.remove (temp_file_name "prelude" "")
     *)

    (** [(fifo fn)] is [true] iff [fn] is a fifo (named pipe). *)
    let fifo fn = (stat fn).st_kind = S_FIFO
    (*$T fifo
      not Sys.unix || finalize (fun fn -> Unix.mkfifo fn 0o600; fifo fn) Sys.remove (temp_file_name "prelude" "")
     *)

    (** [(sock fn)] is [true] iff [fn] is a socket. *)
    let sock fn = (stat fn).st_kind = S_SOCK
    (*$T sock
      finalize (fun fn -> Unix.(socket PF_UNIX SOCK_STREAM 0 |> flip bind (ADDR_UNIX fn)); sock fn) Sys.remove (temp_file_name "prelude" "")
     *)
  end (*$>*)

  (** {1 Stat and Lstat} *)

  (** Convenient functions to access the fields of the Unix stat
      structure.

      These functions retrieve information about the file ultimately
      pointed to by [fn] -- in other words, if [fn] is a symlink then
      [Stat.ino fn = Stat.ino (Unix.readlink fn)]. *)
  module Stat = struct          (*$<Stat *)
    (* TODO TESTS *)
    (** [(dev fn)] is the device major number of [fn]. *)
    let dev   = Stat2.dev    Unix.stat

    (** [(rdev fn)] is the device minor number of [fn]. *)
    let rdev  = Stat2.rdev   Unix.stat

    (** [(ino fn)] is the inode number of [fn]. *)
    let ino   = Stat2.ino    Unix.stat

    (** [(kind fn)] is the kind of [fn]. *)
    let kind  = Stat2.kind   Unix.stat

    (** [(perm fn)] is the permissions of [fn]. *)
    let perm  = Stat2.perm   Unix.stat

    (** [(nlink fn)] is the number of links of [fn]. *)
    let nlink = Stat2.nlink  Unix.stat

    (** [(uid fn)] is the user id of the owner of [fn]. *)
    let uid   = Stat2.uid    Unix.stat

    (** [(gid fn)] is the group id of the owner of [fn]. *)
    let gid   = Stat2.gid    Unix.stat

    (** [(size fn)] is the size of [fn] in bytes. *)
    let size  = Stat2.size   Unix.stat

    (** [(atime fn)] is the last access time of [fn]. *)
    let atime = Stat2.atime  Unix.stat

    (** [(mtime fn)] is the last modifcation time of [fn]. *)
    let mtime = Stat2.mtime  Unix.stat

    (** [(ctime fn)] is the last status change time of [fn]. *)
    let ctime = Stat2.ctime  Unix.stat
  end (*$>*)

  (** Convenient functions to access the fields of the Unix
      stat structure of symbolic links.

      These functions are analogous to those in [Stat], except that if
      [fn] is a symlink they return the information for the link
      itself.  For example:

      If [fn] {i is} a symlink then:
      - [Lstat.ino fn <> Stat.ino (Unix.readlink fn)]

      If [fn] is {i not} a symlink then:
      - [Lstat.ino fn = Stat.ino fn] *)
  module Lstat = struct         (*$<Lstat *)
    (* TODO TESTS *)
    (** [(dev fn)] is the device major number of [fn]. *)
    let dev   = Stat2.dev    Unix.lstat

    (** [(rdev fn)] is the device minor number of [fn]. *)
    let rdev  = Stat2.rdev   Unix.lstat

    (** [(ino fn)] is the inode number of [fn]. *)
    let ino   = Stat2.ino    Unix.lstat

    (** [(kind fn)] is the kind of [fn]. *)
    let kind  = Stat2.kind   Unix.lstat

    (** [(perm fn)] is the permissions of [fn]. *)
    let perm  = Stat2.perm   Unix.lstat

    (** [(nlink fn)] is the number of links of [fn]. *)
    let nlink = Stat2.nlink  Unix.lstat

    (** [(uid fn)] is the user id of the owner of [fn]. *)
    let uid   = Stat2.uid    Unix.lstat

    (** [(gid fn)] is the group id of the owner of [fn]. *)
    let gid   = Stat2.gid    Unix.lstat

    (** [(size fn)] is the size of [fn] in bytes. *)
    let size  = Stat2.size   Unix.lstat

    (** [(atime fn)] is the last access time of [fn]. *)
    let atime = Stat2.atime  Unix.lstat

    (** [(mtime fn)] is the last modifcation time of [fn]. *)
    let mtime = Stat2.mtime  Unix.lstat

    (** [(ctime fn)] is the last status change time of [fn]. *)
    let ctime = Stat2.ctime  Unix.lstat
  end (*$>*)

  (** {1 Symbolic Links} *)

  (**/**)
  let withcd f newdir =
    let olddir = Sys.getcwd () in
    Sys.chdir newdir;
    finalize (fun _ -> f newdir) Sys.chdir olddir
  (**/**)
  (* see below for tests and doc *)

  (** [(follow ?(max=1000) path)] returns an absolute pathname equivalent to [path] unless:

      1. [path] doesn't exist, in which case [Unix.Unix_error (Prelude.Unix.ENOENT, "lstat", _)] is raised; or

      2. [path] is a symbolic link, in which case the link is chased,
      recursively, until a non-symlink is found, in which case an
      absolute pathname of the final destination is returned; or

      3. [path] is a symbolic link with more than [max] (default:
      [1000]) levels of redirections, in which case [Failure _] is raised.
   *)
  let rec follow ?(max=1000) path =
    if Is.link path
    then if max > 0
         then Filename.dirname path |> withcd (fun _ -> path |> Unix.readlink |> follow ~max:~--max)
         else failwith "too many levels of symbolic links"
    else if Filename.is_relative path
    then join (Sys.getcwd ()) path
    else path
  (* TODO TESTS *)

  (** {1 Commands i.e. Executables} *)

  (** Functions to find executable programs, possibly in [$PATH]. *)
  module Command = struct (*$<Command*)

    (** [(hits ?path name)] returns a list of the programs named "name" in [path].

        Only an implicit [name] (see [Filename.is_implicit]) is looked up
        in [path]; a non-implicit name is returned iff it is {!executable} and a plain file.

        Commands are returned in the order in which they are found in [path].

        The default value for [path] is [(Sys.getenv "PATH")].

        Example:
        - [hits "true" = ["/bin/true";"/usr/bin/true"]]

        Invariant: [(Filename.is_implicit name && executable name)] implies [(len (hits name) = 1)]
    *)
    let hits ?path name =
      let commandish fn = executable fn && Is.reg fn in
      if not @@ Filename.is_implicit name
      then (if commandish name then [name] else [])
      else let path = Option.default (Sys.getenv "PATH") path |> split ~elide:false ~sep:":" |> map (whenever "" ".") in
      let eachdir dir fns = let fn = join dir name in if commandish fn then fn::fns else fns in
      foldr eachdir [] path
    (*$T hits
      len (hits "ls") > 0
      len (hits "dgfsdhjgfdjhgdjhgdjh") = 0
      len (hits ~path:"_build" "test.native") = 1
      len (hits ~path:(Sys.getenv "PATH" ^ ":_build") "test.native") > 0
      (hits ~path:(diff (Sys.getenv "PATH" |> split ~elide:false ~sep:":") (hits "ls" |> map Filename.dirname) |> String.concat ":") "ls") = []
    *)

    (** [(exists ?path name)] is [(hits ?path name |> len > 0)]. *)
    let exists ?path name = hits ?path name |> len > 0
    (*$T exists
      exists "ls"
      not @@ exists "fdjhgdjhgdjhgdjhgdjhgdjhg"
    *)

    (** [(first ?path name)] returns [(Some fn)] where [fn] is [(hd (hits ?path name))], or else is [None]. *)
    let first ?path name = match hits ?path name with hit::_ -> Some hit | [] -> None
    (*$= first
      (first "ls") (Some (hd (hits "ls")))
    *)
  end (*$>*)

  (** {1 Directories} *)

  (** [(withcd f dir)] evaluates [(f dir)] with the working
      directory set to [dir], guaranteeing to restore the current
      working directory even in the event of an exception.

      Contrived example:
      {[ let f () = Sys.file_exists "tmp" in
      let a = f () in
      let b = File.withcd (fun _ -> f ()) "/" in
      [a; b; f ()] = [false; true; false] ]}
  *)
  let withcd = withcd
  (* this test needs to be done for two different dirs, just in case we happen to be cd'ed to one of them! *)
  (*$T withcd
    (let dir = "/"    in withcd (fun d -> d = dir && Sys.getcwd () = dir) dir)
    (let dir = "/etc" in withcd (fun d -> d = dir && Sys.getcwd () = dir) dir)
   *)

  (** [(with_tempdir ?temp_dir prefix suffix f)] evaluates [(f dir)] with the working
      directory set to [dir], guaranteeing to restore the current
      working directory even in the event of an exception.

      [dir] is a newly created temporary directory, with permissions
      given by [perm], as named by {!temp_file_name} [?temp_file_name
      prefix suffix].  After evaluation of [f dir], this directory {i
      AND ALL THE FILES IN IT} will be removed by [rm -rf] even in the
      event of an exception.

   *)
  let with_tempdir ?(perm=0o700) ?temp_dir prefix suffix f =
    let dirname = temp_file_name ?temp_dir prefix suffix in
    let with_dirname dir =
      Unix.mkdir dir perm;
      withcd f dir
    in
    let cleanup dir =
      Filename.quote_command "rm" ["-rf";dir] |> Sys.command |> ignore
    in
    finalize with_dirname cleanup dirname
  (* TODO TESTS *)

  (** [(glob pred dir)] is the list of files in [dir] for which [pred] returns [true].

      The predicate is invoked with a filename joined onto [dir]; e.g.
      if [(glob pred ".")] then [pred] will called like [(pred "./foo")].

      The filenames returned will also be joined on [dir].

      See {!Extended.Re.fileglob} for one suitable predicate.
  *)
  let glob pred dir = Sys.readdir dir |> Array.to_list |> map (Filename.concat dir) |> filter pred
  (*$inject
    let testing = "TESTING/fold"
    let (!) = join testing
  *)
  (*$T glob
    (glob (k true) testing |> sort compare) = [!"d1"; !"d2"; !"f1"]
    (glob (fun p -> (Filename.basename p).[0] = 'd') testing |> sort compare) = [!"d1"; !"d2"]
    (glob (k false) testing |> sort compare) = []
    (catch (glob (k true)) "/jhbfrr8oyfrjhbfoiughch") = None
    (catch (glob (k true)) "/dev/null") = None
  *)

  (** [(folddir f init dir)] folds the function [f] across all of the
      filenames the directory [dir], with [init] the initial accumulator.

      The filenames passed to [f] are all relative to [dir].

      {i N.B.}: the list of files never contains ["."] nor [".."].

      [Error msg] is returned if there is an error opening [dir] as a
      directory,
   *)
  let folddir f init dir =
    let each dh acc =
      let fn = Unix.readdir dh in
      if fn = "." || fn = ".."
      then acc
      else f fn acc
    in
    let open Result.Ops in
    Result.trap Exn.to_string Unix.opendir dir
    >>| finalize (foldex ~exn:End_of_file each init) Unix.closedir
  (*$T folddir
    folddir (k succ) 0 "TESTING/fold" = Ok 3
    folddir (k succ) 0 "prelude.ml" |> Result.bad
    Result.Ops.(folddir cons [] "TESTING/fold" >>| sort String.compare ) = Ok ["d1"; "d2"; "f1"]
   *)

  (** The type of items returned by {!to_seq}.

      The components of the tuple are:
      - [None] unless an exception occurred
      - the depth: 0 for the starting directory [dir] itself (only), incremented as we descend
      - the complete pathname relative to the starting [dir]; if [dir] is absolute, so will be all the pathnames.
   *)
  type seqitem = exn option * int * string

  (** [(to_seq ?follow ?xdev dir)] is a sequence consisting of one
      {!seqitem} for each file in [dir] and all its subdirectories, recursively.

      If [follow] (default: [false]) then symlinks to directories will be followed.

      If [xdev] (default: [false]) then we don't descend into
      directories on other filesystems.

      {i N.B.} neither ["."] nor [".."] are ever returned.

      [to_seq] is about 10% slower than {!fold}, but [to_seq] is
      1. easier to use, and 2. much faster if you are going to quit
      early.
   *)
  let to_seq ?follow:following ?(xdev=false) dir =
    let following = match following with Some b -> b | None -> false in
    let dev = Lstat.dev dir in
    let xdevok path = not xdev || Lstat.dev (follow path) = dev in
    let isdir fn = Unix.((lstat fn).st_kind = S_DIR) in
    let islink fn = Unix.((lstat fn).st_kind = S_LNK) in
    let rec loop depth dir =
      let rec each i depth dir arr =
        if i >= Array.len arr
        then fun () -> Seq.Nil
        else let path = Filename.concat dir arr.(i) in
             match xdevok path && (following && isdir path || not (islink path) && isdir path) with
             | true          -> if Is.dir path
                                then Seq.append (loop ~++depth path) (each ~++i depth dir arr)
                                else fun () -> Seq.Cons ((None, depth, path), each ~++i depth dir arr)
             | false         -> fun () -> Seq.Cons ((None, depth, path), each ~++i depth dir arr)
             | exception exn -> fun () -> Seq.Cons ((Some exn, depth, path), each ~++i depth dir arr) (* typically broken symlink *)
      in
      match Sys.readdir dir with
      | [||]          -> fun () -> Seq.Cons ((None, depth, dir), fun () -> Seq.Nil)
      | arr           -> fun () -> Seq.Cons ((None, depth, dir), each 0 ~++depth dir arr)
      | exception exn -> fun () -> Seq.Cons ((Some exn, depth, dir), fun () -> Seq.Nil)
    in
    loop 0 dir
  (* TODO TESTS *)

  type 'a folder = exn option -> int -> string -> 'a -> 'a (** the type of function parameters for {!fold}. *)

  (** [(fold ?(follow=false) ?(xdev=false) f init dir)] folds [f] over
      all files in [dir], recursively, with [init] as initial
      accumulator.

      {i N.B.} neither ["."] nor [".."] are ever passed to [f].

      [f] is of the form [(f exnopt depth path acc)].

      [exnopt] is [(Some exn)] iff an exception occurred during the
      directory traversal.  The main possibilities are permission
      denied in reading the directory, and broken symlinks if [(follow = true)].

      [depth] is 0 for the starting directory [dir] itself (only), and
      is incremented as we descend.  So all the files and directories
      directly contained in [dir] have depth 1, and so on.

      [path] is the complete pathname relative to the starting [dir];
      if [dir] is absolute, so will be all the pathnames.

      [acc] is the accumulator.

      [fold] is about 10% faster than {!to_seq} but always processes
      all the files in [dir] (you can't quit early without resorting to
      {!Exn.val-label} and {!Exn.return}).

      See {!Fold} for helpful combinators.  The following examples don't use
      any of them.

      Example: collect all filenames into a list (ignoring errors):
      - [(fold (fun _ _ p a -> cons p a) [] ".")]
      = [(fold (fun _ _ -> cons) [] ".")]

      Example: print exceptions while traversing:
      - [(fold (fun eo _ -> Option.maybe (Exn.to_string >> prerr_endline) eo; cons) [] ".")]

      Example: terminate traversal on any exception:
      - [(fold (fun eo _ -> Option.maybe raise eo; cons) [] ".")]

      Example: count the files under ["."], ignoring exceptions:
      - [(fold (fun _ _ _ -> succ) 0 ".")]
   *)
  (* TODO OPTIMIZE support short-circuiting the traversal via an exception
     e.g. allowing a maxdepth that doesn't actually traverse below it's max depth
     e.g. allowing a function to return a result immediately
   *)
  let fold ?follow:following ?(xdev=false) (f : 'a folder) init dir =
    let following = match following with Some b -> b | None -> false in
    let dev = Lstat.dev dir in
    let xdevok path = not xdev || Lstat.dev (follow path) = dev in
    let rec loop depth acc dir =
      let each dir acc fn =
        if fn = "." || fn = ".." (* TODO SIMPLIFICATION confirm that Sys.readdir NEVER returns "." or ".." *)
        then acc
        else let path = Filename.concat dir fn in
             match xdevok path && Is.(following && dir path || not (link path) && dir path) with
             | true          -> loop ~++depth acc path
             | false         -> f None depth path acc
             | exception exn -> f (Some exn) depth path acc (* typically broken symlink *)
      in
      match Sys.readdir dir with
      | arr           -> Array.foldl (each dir) (f None ~--depth dir acc) arr
      | exception exn -> f (Some exn) depth dir acc
    in
    loop 1 init dir
  (*$inject let contents = [testing; !"d1"; !"d2"; !"d2/d21"; !"d2/d21/d21f1"; !"d2/d2f1"; !"f1"] *)
  (*$inject let nonexistent = "NONEXISTENT" *)
  (*$T fold
    fold (fun _ _ _ _ -> ()) () testing |> ignore; not (Sys.file_exists nonexistent)
   *)
  (*$T fold
    not (succeeds (fold (fun _ _ _ a -> a) []) nonexistent)
    succeeds (fold (fun _ _ _ a -> a) ()) "."
   *)
  (*$= fold
    (fold (fun _ _ -> cons) [] ".") (fold (fun _ d p a -> cons p a) [] ".")
    (fold (k @@ k cons) [] ".")     (fold (fun _ d p a -> cons p a) [] ".")
   *)

  (** Utility Functions and Combinators for {!fold}.

      These functions  make it easier to provide the function
      parameter to {!fold} for certain use cases.
   *)
  module Fold = struct (*< Fold*)

    (** [fold] is {!fold}. *)
    let fold = fold

    (** {1 Utllity Functions}

        These functions are all {!folder}'s.
     *)

    (** [noop] simply returns its accumulator.

        Invariant: ∀dir s.t. [File.Is.dir dir] . [(fold noop x dir = x)]
     *)
    let noop : 'a folder = fun _exn _depth x xs -> xs

    (** [cons] is [(fun _exn _depth x xs -> List.cons x xs)].

        Example: [(fold cons [] ".")] returns a list of all the files
        in the current directory, recursively, ignoring any directory
        traversal errors.
     *)
    let cons : 'a list folder = fun _exn _depth x xs -> cons x xs

    (** {1 Combinators}

        These functions all return a {!folder}.
     *)

    (** {2 Ignoring Both Exception Option and Depth Parameters} *)

    (** [elided] takes a function shaped like {!List.cons} and returns
        one that ignores the [exn] and [depth] parameters.

        I.e. [(elided f = (fun _exn _depth x xs -> f x xs))]

        Example: [(fold (elided List.cons) [] ".")]
     *)
    let elided f : 'a folder =  fun _exn _depth x xs -> f x xs

    (** {2 Exception Handlers} *)

    (** [(prerr f)] calls {!folder} [f] preceded by printing any
        exception via [(Exn.to_string >> prerr_endline)].

        Example:
        - [File.Fold.(fold (prerr cons) [] ".")]
        = [File.Fold.(fold (prerr (elided List.cons)) [] ".")]
     *)
    let prerr (f : 'a folder) : 'a folder = fun exn depth x xs ->
      match exn with
      | None -> f exn depth x xs
      | Some e -> Exn.to_string e |> prerr_endline; f exn depth x xs

    (** [(err f)] is {!folder} [f] except any exception is raised.

        Therefore, [(err f)] terminates the fold at the first exception.
     *)
    let err (f : 'a folder) : 'a folder = fun exn depth x xs ->
      match exn with
      | None -> f exn depth x xs | Some exn -> raise exn

    (** {2 Controlling the Depth of Recursion} *)

    (** [(maxdepth n f)] only applies {!folder} [f] to the current path if the
        depth of recursion is [<= n].

        {i N.B.} every directory under [dir] is still traversed (i.e. there
        is no short-circuiting), so using [maxdepth] will not
        significantly decrease the amount of time spent in traversal.
     *)
    let maxdepth n (f : 'a folder) : 'a folder = fun exn depth x xs -> if depth > n then xs else f exn depth x xs
  end (*>*)

  (** @deprecated  Previous version of {!fold}. *)
  module Deprecated = struct    (*$< Deprecated *)

    (** [(fold ?err f init dir)] folds [f] over all files in [dir],
      recursively, with [init] as initial accumulator.

      {i N.B.} neither ["."] nor [".."] are ever passed to [f].

      [f] is called with the depth, the complete pathname relative to
      the starting [dir], and the accumulator; if [dir] is absolute,
      so will be all the pathnames.

      The depth is 0 (only) for the starting directory [dir] itself,
      and is incremented as we descend.  So all the files and
      directories directly contained in [dir] have depth 1, and so on.

      An exception can be raised either by [fold] as it descends into
      directories (permission denied or bad symlink), or by [f].  In
      either case, [~err] will be called, instead of [f], with the
      exception, the depth, the pathname, and the accumulator.

      In general it's preferable for [f] to do its own error handling.

      You almost always want to handle exceptions, as there are
      several surprising possibilities: mode = 0, broken symlink, etc.

      If [~err] isn't provided, the exception will be raised and the
      fold terminated.

      See {!Find} for helpful combinators.

      Example: collect all filenames into a list (assuming no errors):
      - [(fold (fun d p a -> cons p a) [] ".")]
      = [(fold (fun _ -> cons) [] ".")]
      = [(fold (k cons) [] ".")]
      = [(fold Find.(igndepth cons) [] ".")]
      = [(fold Find.cons [] ".")]
     *)
    (* DESIGN NOTES
       1. why ?err instead of letting f choose whether or not to handle errs?
          ans: this was a mistake!
       2. why pass depth instead of letting the user use an accumulator with a depth?
          ans: because f can't maintain a depth without more info: it can't just incr,
          but also must decr: it can't know when to do which without an extra parm,
          so we might as well use the depth as an extra parm
     *)
    let fold[@deprecated] = fun ?err f acc dir ->
      let errfn = match err with Some e -> e | None -> fun exn _depth _acc _path -> raise exn in
      let rec loop depth acc dir =
        let witherr depth acc path = match f depth path acc with
          | acc           -> acc
          | exception exn -> errfn exn depth path acc
        in
        let each dir acc fn =
          if fn = "." || fn = ".."
          then acc
          else let path = Filename.concat dir fn in
               match Sys.is_directory path with
               | true          -> loop (succ depth) acc path
               | false         -> witherr depth acc path
               | exception exn -> witherr depth acc path (* can fail on broken symlink *)
        in
        match Sys.readdir dir with
        | arr           -> Array.foldl (each dir) (witherr ~--depth acc dir) arr
        | exception exn -> errfn exn depth dir acc
      in
      loop 1 acc dir

    (** @deprecated Useful combinators and error handlers for {!fold}. *)
    module Find = struct (*$< Find *)

      (** {1 Predefined Folders}

        A {i folder} is a function suitable as the [f] parameter of {!fold}. *)

      (** [noop] is a folder that simply returns its accumulator. *)
      let noop[@deprecated] = fun _ _ acc -> acc
      (** [cons] is [(fun _ -> cons)]. *)
      let cons[@deprecated] = fun _ p a -> cons p a

      (** {1 Folder Combinators}

        These combinators convert functions of various shapes into folders. *)

      (** [(igndepth f)] returns a folder that ignores the depth parameter.

        [f] should be a function like [List.cons].

        Example: collect all files into a list (assuming no errors):
        - [(fold Find.(igndepth List.cons) [] ".")]
       *)
      let igndepth[@deprecated] = fun f -> fun _ path acc -> f path acc
      (** [(whenfile p f)] returns a folder that invokes [(f depth path acc)] iff [(p path = true)].

        Example: [(fold Find.(whenfile Sys.is_directory cons) [] ".")] *)
      let whenfile[@deprecated] = fun p f -> fun depth path acc -> if p path then f depth path acc else acc
      (** [(maxdepth d f)] returns a folder that invokes [f] iff the depth [<= d]. *)
      let maxdepth[@deprecated] = fun d f -> fun depth path acc -> if depth <= d then f depth path acc else acc
      (** [(mindepth d f)] returns a folder that invokes [f] iff the depth [>= d]. *)
      let mindepth[@deprecated] = fun d f -> fun depth path acc -> if depth >= d then f depth path acc else acc

      (** @deprecated Error Combinators

        These combinators convert functions of various shapes into
        functions suitable for the [~err] parameter of {!fold}.

        You can pass your folder to most of these combinators, or use
        a completely different one.

        Note the difference between:

        {[ let f _ _ n = succ n in fold ~err:Find.(ignore noop) f 0 "/etc" ;;
        - : int = 2175
        # let f _ _ n = succ n in fold ~err:Find.(ignore f) f 0 "/etc" ;;
        - : int = 2184 ]}

        The first doesn't count some broken symlinks, and doesn't
        count the subdirectories that can't be descended into; the
        second includes these.  *)

      (** [(ignore f)] ignores errors, but still passes the pathname and accumulator to [f]. *)
      let ignore[@deprecated] = fun f _exn depth path acc -> f depth path acc
      (** [(log hdl f)] logs errors by passing a string representation
        of the exception to [hdl], but still passes the pathname and
        accumulator to [f].

        Note that most errors will be [Sys_error]'s and the string
        representation will contain the pathname. *)
      let log[@deprecated] = fun hdl f -> fun exn depth path acc -> match exn with
                                                | Sys_error err -> hdl err;                       f depth path acc
                                                | _             -> Printexc.to_string exn |> hdl; f depth path acc
      (** [(prerr f)] is [(log prerr_endline f)]. *)
      let prerr[@deprecated] = fun f -> (log [@warning "-3"]) prerr_endline f

      (** @deprecated Predicates *)

      (** [(isdirectory fn)] is [true] if [fn] is a directory.

        N.B. this differs from [(Sys.is_directory fn)] in that the
        latter will raise an exception if [fn] is a broken symlink,
        whereas [isdirectory] will simply return [false].
       *)
      let isdirectory[@deprecated] = default false Sys.is_directory
    end (*$>*)

  end (*$>*)

end (*$>*)

(** [(~~)] is {!File.squiggle}. *)
let (~~) fn = File.squiggle fn
(*$Q (~~)
  Q.small_string (fun s -> ~~s = File.squiggle s)
*)

(** [withcd] is {!File.withcd}. *)
let withcd = File.withcd

(* TODO need tmpfile stuff -- or, rely on Bos.File? *)
(** {1:io Input / Output} *)

(** {2 Types} *)

(** The type of line-endings. *)
type eol = CR                   (** ['\r'] is the EOL character (old Macintosh-style) *)
           | LF                 (** ['\n'] is the EOL character (Unix-style) *)
           | CRLF               (** ["\r\n"] are the EOL characters (Windows-style) *)

(** [(eol e)] returns the string corresponding to the line-ending. *)
let eol = function CR -> "\r"| LF -> "\n" | CRLF -> "\r\n"

(** {2 Finalizers}
    Avoiding file descriptor leaks.

    N.B. [within] and [without] do not perform [~]-expansion on
    filenames; in typical use where filenames come from the
    command-line, this is unnecessary because the shell does the
    [~]-expansion before your program sees the filename.

    If you want to do [~]-expansion yourself, just transform e.g.:

    [(within read)] -> [(within read << File.squiggle)]
*)

(** [iflags] are the default flags passed to [open_in_gen] by
    {!within} (default: [[Open_rdonly]]). *)
let iflags = [Open_rdonly]

(** [oflags] are the default flags passed to [open_out_gen] by {!without} (default:
    [[Open_creat;Open_wronly;Open_trunc]]). *)
let oflags = [Open_creat;Open_wronly;Open_trunc]

(** [dash] is the default filename that is interpreted as [stdin] or
    [stdout] by [Prelude] I/O functions.

    The default is ["-"].

    See: {!within}, {!without}, {!readfile}, and {!writefile}.
*)
let dash = "-"

(** [(within ?dash ?iflags f file)] opens [file] for input and
    returns [(f c)] where [c] is the [in_channel]; [c] is guaranteed
    to be closed when [within] returns, even if [f] raises an
    exception (which is re-raised).

    [iflags] are passed to [open_in_gen] (default: {!iflags}).

    [dash] specifies a filename that is interpreted as [stdin]
    (default: [Some] {!dash}); use [~dash:None] to avoid any such interpretation. *)
let within ?(dash=Some dash) ?(iflags=iflags) f fn = (* TODO use close_in_noerr? *)
  match dash with
  | Some d when d=fn -> f stdin
  | _ (* otherwise*) -> finalize f close_in (open_in_gen iflags 0o666 fn)
(*$T within
  within read "/dev/null" = ""
  within input_char "/dev/zero" = '\000'
  not @@ succeeds (within read) "NONEXISTENT"
  let dc = Unix.descr_of_in_channel in let d = ref (dc stdin) in within Unix.(fun c -> d := dc c) "/dev/zero"; not @@ succeeds (Unix.in_channel_of_descr $ read) !d
  let dc = Unix.descr_of_in_channel in let d = ref (dc stdin) in try within Unix.(fun c -> d := dc c; failwith "") "/dev/zero" |> ignore; false with _ -> not @@ succeeds (Unix.in_channel_of_descr $ read) !d
*)

(** [(without ?dash ?oflags ?perm f file)] opens [file] for output and
    returns [(f c)] where [c] is the [out_channel]; [c] is guaranteed
    to be closed when [without] returns, even if [f] raises an
    exception (which is re-raised).

    [dash] specifies a filename that is interpreted as [stdout]
    (default: [Some] {!dash}); use [~dash:None] to avoid any such
    interpretation.

    [oflags] are passed to [open_out_gen] (default: {!oflags}).

    [perm] specifies the permissions used when a file is created
    (default: [0o666]); these are subject to your [umask] in the usual
    manner.
*)
let without ?(dash=Some dash) ?(oflags=oflags) ?(perm=0o666) f fn = (* TODO use close_out_noerr? *)
  match dash with
  | Some d when d=fn -> f stdout
  | _ (* otherwise*) -> finalize f close_out (open_out_gen oflags perm fn)
(*$T without
  succeeds (without (flip write "x")) "/dev/null"
*)
(* TODO can't figure out how to test the closing of the fd! *)

(** [(withinout ?dash ?iflags ?oflags ?perm f ~infile ~outfile)] opens
    [infile] for input and [outfile] for output and returns [(f inc outc)]
    where [inc] is the [in_channel] and [outc] is the
    [out_channel]; both channels are guaranteed to be closed when
    [withinout] returns, even if [f] raises an exception (which is
    re-raised).

    [dash], [perm], and [oflags] are as for {!without}, and [iflags]
    is as for {!within}.
*)
let withinout ?(dash=Some dash) ?(iflags=iflags)  ?(oflags=oflags) ?(perm=0o666) f ~infile ~outfile =
  without (fun outc -> within (fun inc -> f inc outc) infile) outfile
(*$T withinout
  withinout (flip copyto) ~infile:"/dev/null" ~outfile:"/dev/null" = ()
*)


(** [(withtempfile ?tmpdir ?oflags ?perm ?prefix ?suffix f)] calls [f]
    with a secure, writable temporary file; when [f] returns, the temp
    file is guaranteed to be removed.

    [f] is called with two parameters, the name of the temp file, and
    an output channel open on the temp file.  [f] can close and reopen
    the file (say, for reading) if it likes.
*)
let withtempfile ?tmpdir:temp_dir ?oflags:mode ?perm:perms ?(prefix="") ?(suffix="") f =
  let fn,c = Filename.open_temp_file ?temp_dir ?perms ?mode prefix suffix in
  finalize (f fn) (fun c -> close_out_noerr c; Exn.ignore Sys.remove fn) c
(*$T withtempfile
  (withtempfile Filename.(fun fn _ -> dirname fn = get_temp_dir_name ()))
  (withtempfile ~tmpdir:"/tmp" Filename.(fun fn _ -> dirname fn = "/tmp"))
  (withtempfile ~prefix:"FOO" Filename.(fun fn _ -> basename fn |> String.prefix "FOO"))
  (withtempfile ~suffix:"FOO" Filename.(fun fn _ -> basename fn |> String.suffix "FOO"))
  (let x = ref "" in withtempfile (fun fn _ -> x:=fn); not @@ File.exists !x)
 *)

(** {2 Input} *)

(** [blocksize] is the default block and buffer size for {!read} and {!readblock}.  *)
let blocksize = 8192

(** [(read ?bufsize c)] reads the entire contents of in_channel [c] and
    returns the result as a string.

    [bufsize] is the size of the input buffer used (default: {!blocksize}).
*)
let read ?(bufsize=blocksize) c =
  if bufsize = 0
  then ""
  else withbuf bufsize (fun b -> foldex ~exn:End_of_file (fun c () -> Buffer.add_channel b c bufsize) () c)
(*$inject
  let readfn = "TESTING/lines.txt"
  let readsize = Unix.((stat readfn).st_size)
  let readfnlines = within input_line "TESTING/lines.count" |> String.(trim whitespace) |> int_of_string
*)
(*$= read
  (within read readfn |> String.len) readsize
*)
(*$Q read
  Q.(int_bound readsize) (fun bufsize -> bufsize = 0 || (within (read ~bufsize) readfn |> String.len) = readsize)
*)

(** [(readblock ?buf ?size c)] returns [(Some b)] where [b] is the
    next block of [size] (or fewer) characters from channel [c], or
    else [None] upon end of file.

    If you are calling [readblock] repeatedly, as is typical, it is
    more efficient to pass [~buf], a [Bytes.t] of size [>= size] (this
    minimizes allocations).

    [size] is the size of the input buffer used (default: [Bytes.length buf] or else {!blocksize}).
*)
let readblock ?buf ?size c =
  let b = Option.default (Bytes.create @@ Option.default blocksize size) buf in
  let n = input c b 0 (Bytes.length b) in
  if n = 0 then None else Bytes.sub_string b 0 n |> some
(*$T readblock
  within  readblock                                readfn |> something
  within (readblock ~buf:(Bytes.make 1024 '\000')) readfn |> something
  within (readblock ~buf:(Bytes.make 1024 '\000')) readfn |> Option.get |> String.len = 1024
*)
(*$= readblock
  (within readblock "/dev/null") None
  (within (readblock ~buf:(Bytes.make blocksize 'x')) "/dev/null") None
  (within (readblock ~size:blocksize ~buf:(Bytes.make blocksize 'x')) "/dev/null") None
  (within (readblock ~size:(blocksize / 2) ~buf:(Bytes.make blocksize 'x')) "/dev/null") None
  Option.(within readblock readfn >>| String.len) (Some (min readsize blocksize))
*)
(*$Q readblock
  Q.(int_bound readsize) (fun size -> size = 0 || Option.(within (readblock ~size) readfn >>| String.len) = (Some (min readsize size)))
*)

(** [(readfile ?dash ?bufsize fn)] is [(within ?dash (read ?bufsize) fn)]. *)
let readfile ?dash ?bufsize = within ?dash (read ?bufsize)
(*$= readfile
  (readfile readfn |> String.len) readsize
*)
(*$Q readfile
  Q.(int_bound readsize) (fun bufsize -> bufsize = 0 || (readfile ~bufsize readfn |> String.len) = readsize)
*)

(** [readline] is [input_line]. *)
let readline = input_line
(*$T readline
  within readline readfn = within input_line readfn
*)

(** [(readline' ?elide ?buf ?eol chan)] is a {!readline} that retains the EOL character(s).

    Rationale:
    {ol {- Precise handling of Mac and Windows files on Unix systems.}
    {- Neither [(foldlines ~readline:readline (fun () -> print_endline) ())] nor
    [(foldlines ~readline:readline (fun () -> print_string) ())]
    is guaranteed to make an exact copy of their input, whereas
    [(foldlines ~readline:readline' (fun () -> print_string) ())] is.}}

    So: [(writefile ~fn:"/tmp/foo" "foo\n"; within readline' "/tmp/foo" = "foo\n")]

    whereas: [(writefile ~fn:"/tmp/foo" "foo\n"; within readline  "/tmp/foo" = "foo")]

    [~eol] determines the EOL character(s).  The default for [~eol] is
    [LF].  In [CRLF] mode, isolated ['\r'] and ['\n'] do not terminate
    the line, and are returned "in the middle" of the line.

    [~elide:true] causes the EOL character(s) to be trimmed from the
    end of the line, like {!readline} always does for ['\n'].  The
    default value for [~elide] is [false].

    Mnemonic: the ['] in [readline'] stands for the presence of the EOL.
*)
let readline' ?(elide=false) ?(eol=LF) chan = (* TODO OPTIMIZE rewrite in terms of input *)
  let open Buffer in
  let buf = create 1024 in
  let get buf = let str = contents buf in clear buf; str in
  let rec loop cr = match eol, input_char chan with
    | LF,   c when c = '\n' -> if not elide then add_char buf c; get buf
    | CR,   c when c = '\r' -> if not elide then add_char buf c; get buf
    | CRLF, c when c = '\r' ->                   add_char buf c; loop true
    | CRLF, c when c = '\n' ->
       if cr
       then if elide
         then sub buf 0 (length buf - 1)
         else (add_char buf c; get buf)
       else (add_char buf c; loop false)
    | _,     c               -> add_char buf c; loop false
    | exception End_of_file  -> if length buf > 0 then get buf else raise End_of_file
  in
  if elide && eol = LF then input_line chan else loop false
(*$inject
  let withtempfile ?(suffix="") g x =
    let fn,c = Filename.open_temp_file "prelude" suffix in
    let eachtemp c = write c x; close_out c; within g fn in
    finalize eachtemp (fun c -> close_out c; Sys.remove fn) c
*)
(*$= readline'
  (withtempfile readline' "foo") "foo"

  (withtempfile (readline' ~elide:false ~eol:LF)   "foo") "foo"
  (withtempfile (readline' ~elide:true  ~eol:LF)   "foo") "foo"
  (withtempfile (readline' ~elide:false ~eol:CR)   "foo") "foo"
  (withtempfile (readline' ~elide:true  ~eol:CR)   "foo") "foo"
  (withtempfile (readline' ~elide:false ~eol:CRLF) "foo") "foo"
  (withtempfile (readline' ~elide:true  ~eol:CRLF) "foo") "foo"

  (withtempfile (readline' ~elide:false ~eol:LF)   "foo\n") "foo\n"
  (withtempfile (readline' ~elide:true  ~eol:LF)   "foo\n") "foo"
  (withtempfile (readline' ~elide:false ~eol:CR)   "foo\n") "foo\n"
  (withtempfile (readline' ~elide:true  ~eol:CR)   "foo\n") "foo\n"
  (withtempfile (readline' ~elide:false ~eol:CRLF) "foo\n") "foo\n"
  (withtempfile (readline' ~elide:true  ~eol:CRLF) "foo\n") "foo\n"

  (withtempfile (readline' ~elide:false ~eol:LF)   "foo\r") "foo\r"
  (withtempfile (readline' ~elide:true  ~eol:LF)   "foo\r") "foo\r"
  (withtempfile (readline' ~elide:false ~eol:CR)   "foo\r") "foo\r"
  (withtempfile (readline' ~elide:true  ~eol:CR)   "foo\r") "foo"
  (withtempfile (readline' ~elide:false ~eol:CRLF) "foo\r") "foo\r"
  (withtempfile (readline' ~elide:true  ~eol:CRLF) "foo\r") "foo\r"

  (withtempfile (readline' ~elide:false ~eol:LF)   "foo\r\n") "foo\r\n"
  (withtempfile (readline' ~elide:true  ~eol:LF)   "foo\r\n") "foo\r"
  (withtempfile (readline' ~elide:false ~eol:CR)   "foo\r\n") "foo\r"
  (withtempfile (readline' ~elide:true  ~eol:CR)   "foo\r\n") "foo"
  (withtempfile (readline' ~elide:false ~eol:CRLF) "foo\r\n") "foo\r\n"
  (withtempfile (readline' ~elide:true  ~eol:CRLF) "foo\r\n") "foo"

  (withtempfile (readline' ~elide:false ~eol:CRLF) "foo\rbar") "foo\rbar"
  (withtempfile (readline' ~elide:true  ~eol:CRLF) "foo\rbar") "foo\rbar"

  (withtempfile (readline' ~elide:false ~eol:CRLF) "foo\nbar") "foo\nbar"
  (withtempfile (readline' ~elide:true  ~eol:CRLF) "foo\nbar") "foo\nbar"

  (withtempfile (readline' ~elide:false ~eol:LF)   "foo\nzap") "foo\n"
  (withtempfile (readline' ~elide:true  ~eol:LF)   "foo\nzap") "foo"
  (withtempfile (readline' ~elide:false ~eol:LF)   "foo\rzap") "foo\rzap"
  (withtempfile (readline' ~elide:true  ~eol:LF)   "foo\rzap") "foo\rzap"
  (withtempfile (readline' ~elide:false ~eol:LF)   "foo\r\nzap") "foo\r\n"
  (withtempfile (readline' ~elide:true  ~eol:LF)   "foo\r\nzap") "foo\r"

  (withtempfile (readline' ~elide:false ~eol:CR)   "foo\nzap") "foo\nzap"
  (withtempfile (readline' ~elide:true  ~eol:CR)   "foo\nzap") "foo\nzap"
  (withtempfile (readline' ~elide:false ~eol:CR)   "foo\rzap") "foo\r"
  (withtempfile (readline' ~elide:true  ~eol:CR)   "foo\rzap") "foo"
  (withtempfile (readline' ~elide:false ~eol:CR)   "foo\r\nzap") "foo\r"
  (withtempfile (readline' ~elide:true  ~eol:CR)   "foo\r\nzap") "foo"

  (withtempfile (readline' ~elide:false ~eol:CRLF) "foo\nzap") "foo\nzap"
  (withtempfile (readline' ~elide:true  ~eol:CRLF) "foo\nzap") "foo\nzap"
  (withtempfile (readline' ~elide:false ~eol:CRLF) "foo\rzap") "foo\rzap"
  (withtempfile (readline' ~elide:true  ~eol:CRLF) "foo\rzap") "foo\rzap"
  (withtempfile (readline' ~elide:false ~eol:CRLF) "foo\r\nzap") "foo\r\n"
  (withtempfile (readline' ~elide:true  ~eol:CRLF) "foo\r\nzap") "foo"
*)

(** [(readlines ?readline c)] is a list of all the lines on the
    in_channel [c]; end-of-line characters will be present or absent
    depending the value you choose for [~readline]; the default trims EOLs.

    [~readline] is the function that reads each line; suitable
    candidates include [input_line] (the default), {!readline}, and
    {!readline'}.
*)
let readlines ?(readline=input_line) = foldex ~exn:End_of_file (conswith readline) [] $ rev
(*$= readlines
  (within readlines "/dev/null") []
  (Unix.open_process_in "echo foo" |> finalize readlines close_in) ["foo"]
  (within readlines readfn |> len) readfnlines
  (within (readlines ~readline:readline') readfn |> len) readfnlines
*)

(** [(foldchars f acc c)] folds [f] over the chars on in_channel [c].

    [(foldchars snoc [] >> rev >> String.implode = readfile)], but would be
    considerably less efficient than [readfile]. *)
let rec foldchars f acc chan = match input_char chan with
| c -> foldchars f (f acc c) chan | exception End_of_file -> acc
(*$= foldchars
  (within (foldchars (fun n _ -> succ n) 0) readfn) readsize
*)

(**/**)
(** [(fold_substrings n f init chan)] left-fold [f] across sequential
    non-overlapping length [n] substrings of the data on input channel
    [chan]; tail-recursive.

    For example, [(fold_substrings 2)] is a pairwise fold.  If the
    input consists of the string "abcdefgh", then [(fold_substrings 2 snoc)]
    returns [["ab";"cd";"ef";"gh"]]

    [f] is given an accumulator and a string of length [n] or less (if
    the length of the input is not an even multiple of [n]).

    Invariant:
    - [(within (fold_substrings 2 snoc [] $ rev $ String.concat "") fn) = (read fn)]
 *)
let fold_substrings size f acc chan =
  let each (i,g,acc) x =
    if i = size-1
    then 0, [], f acc (x :: g |> rev |> String.implode)
    else i+1, (x::g), acc
  in
  match foldchars each (0,[],acc) chan with
  | _,[],acc -> acc
  | _,g, acc -> rev g |> String.implode |> f acc
(* TODO TESTS *)
(**/**)

(** [(foldlines ?readline f acc c)] folds [f] over the lines on in_channel [c].

    [?readline] is as for {!readlines}.

    {[ (foldlines (snocwith f) [] >> rev) = maplines f ]}
 *)
let rec foldlines ?(readline=input_line) f acc chan = match readline chan with
| line -> foldlines ~readline f (f acc line) chan | exception End_of_file -> acc
(*$= foldlines
  (within (foldlines snoc []) "/dev/null") []
  (Unix.open_process_in "echo foo" |> finalize (foldlines snoc []) close_in) ["foo"]
  (within (foldlines snoc []) readfn |> len) readfnlines
  (within (foldlines ~readline:readline' snoc []) readfn |> len) readfnlines
*)

(** [(maplines ?readline f c)] maps [f] across all the lines on in_channel [c],
    returning the list of results.

    [?readline] is as for {!readlines}.
 *)
let maplines ?(readline=input_line) f = foldlines ~readline (snocwith f) [] $ rev
(*$= maplines
  (within (maplines id) "/dev/null") []
  (Unix.open_process_in "echo foo" |> finalize (maplines id) close_in) ["foo"]
  (Unix.open_process_in "echo foo" |> finalize (maplines (k "X")) close_in) ["X"]
  (within (maplines id) readfn |> len) readfnlines
  (within (maplines ~readline:readline' id) readfn |> len) readfnlines
*)

(** [(iterlines1 ?readline f c)] applies [f] to each of the lines on in_channel [c],
    for side-effect.

    [?readline] is as for {!readlines}.
 *)
let iterlines ?(readline=input_line) f = foldlines ~readline (k f) ()
(*$= iterlines
  (let n = ref 0 in within (iterlines (fun _ -> incr n)) "/dev/null"; !n) 0
  (let n = ref 0 in Unix.open_process_in "echo foo" |> finalize (iterlines (fun _ -> incr n)) close_in; !n) 1
  (let n = ref 0 in within (iterlines (fun _ -> incr n)) readfn; !n) readfnlines
  (let n = ref 0 in within (iterlines ~readline:readline' (fun _ -> incr n)) readfn; !n) readfnlines
*)

(** [(chars_of_chan chan)] is the sequence of characters from [chan]. *)
let rec chars_of_chan chan () = match input_char chan with
  | exception End_of_file -> Seq.Nil
  | line                  -> Seq.Cons (line, chars_of_chan chan)
(*$T chars_of_chan
  within (chars_of_chan >> Seq.to_list >> eq []) "/dev/null"
  let fn = "/etc/passwd" in within (chars_of_chan >> Seq.to_list) fn = within (read >> String.explode) fn
 *)

(** [(lines_of_chan chan)] is the sequence of lines from [chan]. *)
let rec lines_of_chan chan = fun () ->
  match input_line chan with
  | exception End_of_file -> Seq.Nil
  | line                  -> Seq.Cons (line, lines_of_chan chan)
(*$T lines_of_chan
  within (lines_of_chan >> Seq.to_list >> eq []) "/dev/null"
  let fn = "/etc/passwd" in within (lines_of_chan >> Seq.to_list) fn = within readlines fn
 *)

(** {2 Output} *)

(**/**)
let outin f x = withtempfile ~prefix:"prelude" (fun fn c -> f c x; close_out c; readfile fn)
(**/**)

(** [print] is [print_endline]. *)
let print = print_endline

(** [write] is [output_string]. *)
let write = output_string
(*$Q write
  Q.string (fun s -> outin write s = s)
 *)

(** [(writeline ?eol chan s)] is [(output_string chan s)] followed by the line-ending [~eol] (default: {!LF}). *)
let writeline ?eol:e c s =
  output_string c s; Option.default LF e |> eol |> output_string c
(*$Q writeline
  Q.string (fun s -> outin  writeline            s = s ^ "\n")
  Q.string (fun s -> outin (writeline ~eol:LF)   s = s ^ eol LF)
  Q.string (fun s -> outin (writeline ~eol:CR)   s = s ^ eol CR)
  Q.string (fun s -> outin (writeline ~eol:CRLF) s = s ^ eol CRLF)
 *)

(** [(writelines ?eol chan)] is [(iter (writeline ?eol chan))]*)
let writelines ?eol:e c = iter (writeline ?eol:e c)
(*$Q writelines
  Q.(list string) (fun xs -> let xs = filter ((<>) "") xs in xs = [] || outin writelines             xs = String.concat "\n"       xs ^ "\n")
  Q.(list string) (fun xs -> let xs = filter ((<>) "") xs in xs = [] || outin (writelines ~eol:LF)   xs = String.concat (eol LF)   xs ^ eol LF)
  Q.(list string) (fun xs -> let xs = filter ((<>) "") xs in xs = [] || outin (writelines ~eol:CR)   xs = String.concat (eol CR)   xs ^ eol CR)
  Q.(list string) (fun xs -> let xs = filter ((<>) "") xs in xs = [] || outin (writelines ~eol:CRLF) xs = String.concat (eol CRLF) xs ^ eol CRLF)
 *)

(** [(writefile ?dash ?oflags ?perm ~fn s)] writes s to file [fn],
    passing the optional parameters to {!without}. *)
let writefile ?dash ?oflags ?perm ~fn str = without ?dash ?oflags ?perm (flip write str) fn
(*$Q writefile
  Q.string (fun s -> finalize (fun fn -> writefile ~fn s; readfile fn) Sys.remove (Filename.temp_file "prelude" "") = s)
 *)

(** [(copyto ?size oc ic)] copies the contents of the input channel
    [ic] to the output channel [oc] one [size]-block at a time.

    [?size] defaults to {!blocksize}.

    Examples:
    - print a file to standard output: [(within (copyto stdout) "/etc/passwd")]
    - copy ["infile"] to ["outfile"]: [(without (fun oc -> within (copyto oc) "infile") "outfile")]
 *)
let copyto ?(size=blocksize) oc ic =
  let buf = Bytes.create size in
  Gen.(iter (write oc) @@ optional (readblock ~buf ~size) ic)
(*$= copyto
  (readfile "/etc/passwd") (finalize (fun fn -> without (fun oc -> within (copyto oc) "/etc/passwd") fn; readfile fn) Sys.remove (Filename.temp_file "prelude" ""))
 *)

(** [(copy ?size ?oflags ?iflags ?perm ~infile ~outfile ())] copies
    the contents of the file [infile] to the file [outfile].

    [?size] is passed to {!copyto}; [?oflags] is passed to {!within};
    [?iflags] and [?perm] are passed to {!without}.
*)
let copy ?size ?oflags ?iflags ?perm ~infile ~outfile () =
  without ?oflags ?perm (fun oc -> within ?iflags (copyto ?size oc) infile) outfile

(** {1:interactive Interactive} *)

(** Simple interaction with a human at a terminal. *)
(* TODO TESTS *)
module Interact = struct

  (** The type of terminal (TTY) input / output channels. *)
  module type TTY = sig
    val ttyin  : in_channel
    val ttyout : out_channel
  end

  (** Functor that instantiates a {!TTY} module which uses
        [/dev/tty] for both input and output.

        Using a functor lets us delay the opening of the TTY until needed.

        Typical use for simple interactive programs is to just
        instantiate the module at the global level of your program:

        {[ module Tty = Tty (Null) ]}
   *)
  module Tty (U : NULL) = struct
    open Unix
    let t = openfile "/dev/tty" [O_RDWR] 0
    let ttyin = in_channel_of_descr t
    let ttyout = out_channel_of_descr t
  end

  (** Module that instantiates a {!TTY} module which uses [stdin] and
     [stdout] for input and output.

     This might be useful for testing in a top-level running in
     Emacs, where [/dev/tty] isn't available.
   *)
  module Stdio : TTY = struct
    let ttyin = stdin
    let ttyout = stdout
  end

  (** Functor to construct an [Interactive] module from a given [TTY] module.

      Typical usage is:
      {[ module I = Interact.Make (Interact.Tty (Null)) ]}
   *)
  module Make (Tty : TTY) = struct

    module Tty = Tty

    (** [(display msg)] prints [msg] to [Tty.ttyout] and flushes. *)
    let display msg =
      write Tty.ttyout msg;
      flush Tty.ttyout

    (** [(norm str)] is [str] trimmed of whitespace on both ends and converted to lowercase.

        This function works for ASCII only. *)
    let norm = String.(trim whitespace $ lowercase_ascii)

    (** [(read ?norm prompt)] prints the prompt on [Tty.ttyout] and
        then reads a one-line normalized string from [Tty.ttyin].

        The default value for [~norm] is {!id} (no normalization). *)
    let read ?(norm=id) prompt =
      Tty.(output_string ttyout prompt; flush ttyout; input_line ttyin |> norm)

    (** [(query ?norm ?what valid prompt)] calls [(read ?norm prompt)]
        repeatedly until [((valid (read ?norm prompt)) = true)].

        [~what] is used as the prompt after receiving a bogus (not
        [valid]) answer; the default is ["What? "].

        The normalized response is returned.

        The default value for [~norm] is as for {!read}. *)
    let query ?norm ?(what="What? ") valid prompt =
      read ?norm prompt |> whilst (not $. valid) (fun _ -> read ?norm what)

    (** [(oneof list)] is a validator for {!query} that accepts any of
        the strings in [list]. *)
    let oneof list (ans : string) = mem ans list

    (** [(whatof list)] returns a string suitable for [query]'s [~what]
        parameter:

        - [(query ~what:(whatof list) (oneof list))].
    *)
    let whatof list = map (sprintf "\"%s\"") list |> join ~sep:" or " |> sprintf "Please answer %s: "

    (** [(yorn ?norm ?what prompt)] calls [(read ?norm prompt)]
        repeatedly until a ["y"] or ["n"] answer is received.

        Any string beginning (after normalization) with either ["y"] or
        ["n"] is acceptable as an answer.

        [~what] is as for {!query}.

        The normalized answer is returned.
    *)
    let yorn ?norm ?what = query ?norm ?what (disjunction String.[prefix "y"; prefix "n"])

    (** [yesorno] is a stricter {!yorn} that insists on a normalized
        answer of exactly either ["yes"] or ["no"]. *)
    let yesorno ?norm ?(what=whatof ["yes"; "no"]) = query ?norm ~what (oneof ["yes"; "no"])

    (** [(okay ?what prompt)] returns [true] iff [(yorn ?what prompt)]
        returns a ["y"] answer. *)
    let okay ?what prompt = yorn ?what prompt |> String.prefix "y"
  end
end

(** {1:units Units} *)

(** Modules for converting and displaying units. *)
module Units = struct
(* TODO TESTS *)

  (** The type of modules that implement numbers. *)
  module type NUM = sig
    type t
    val compare : t -> t -> int
    val zero : t
    val of_int : int -> t
    val to_int : t -> int
    val of_string : string -> t
    val to_string : t -> string
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val (mod) : t -> t -> t
  end

  (** Integers; satisfies [NUM]. *)
  module Int = struct
    (** @closed *)
    include Int
    let zero = 0
    let (mod) = (mod)
    let of_string = int_of_string
    let to_string = string_of_int
    let to_int, of_int = id,id
  end

  (** 64-bit integers; satisfies [NUM]. *)
  module Int64 = struct
    (** @closed *)
    include Int64
    let ( + ), ( - ), ( * ), ( / ), (mod) = add, sub, mul, div, rem
  end

  (** {2:unitstime Time Units} *)

  (** Functions for converting and displaying units of time.

      This functor needs to be applied to a module of type [NUM]
      representing your preferred base time unit resolution.

      Example: [module Time_int = Time (Int)]

      This module is intended to provide informal human-readable
      representations of {i approximate} time durations, converting a
      value of type [t] to friendly strings like ["3 weeks, 1 day"] or
      ["3 hours and 10 minutes"].

      Accuracy is not the {i raison d'etre} of this module; for example,
      months are defined as 31 days long, and years are defined as 12
      months long!  Leap years and leap seconds are completely ignored.

      Examples:
      - [(minutes !12 + seconds !15) * !6 |> long = "1 hour, 13 minutes and 30 seconds"]
      - [of_string "7:03" |> long = "7 minutes and 3 seconds"]

      {i N.B.} For computing with accurate representations of time
      durations, see e.g. Daniel Bünzli's excellent modules:

      @see <http://erratique.ch/software/mtime> Bünzli's monotonic wall-clock time
      @see <http://erratique.ch/software/ptime> Bünzli's POSIX timestamps with picosecond precision
  *)
  module Time (N : NUM) = struct (* TODO should add a language module: Time (L:English) (N:Num) *)
                                 (* TODO turn into a functor that can handle other units e.g. distance *)
    (** {1 Types and type conversion} *)

    type t = N.t
    (** The type of base time units. *)

    let ( ! ) = N.of_int
    (** Prefix operator to convert an [int] to a [t]. *)

    (** {1 Arithmetic} *)

    let ( + ) = N.( + )
    (** Addition over values of type [t]. *)

    let ( - ) = N.( - )
    (** Subtraction over values of type [t]. *)

    let ( * ) = N.( * )
    (** Multiplication over values of type [t]. *)

    let ( / ) = N.( / )
    (** Division over values of type [t]. *)

    (** {1 Units} *)

    (* NB if changing factors, check accuracy of docstrings for factors and units *)
    let factors = [
      (* multiplier singular  plural *)
      !12,    "year",   "years";
      !31,    "month",  "months";
      !7,     "week",   "weeks";
      !24,    "day",    "days";
      !60,    "hour",   "hours";
      !60,    "minute", "minutes";
      !1_000, "second", "seconds";
      !1_000, "millisecond", "milliseconds";
      !1,     "microsecond", "microseconds";
    ]
    (** Multiplicative factors for each unit, in descending order, with
        the unit's name and the plural form of the name.

        For example, [(!24, "day", "days")] says that one day is 24 of
        the unit that follows it in the list, which is ["hour"].

        This list may contain units that cannot be represented in a
        given base number type [N]; see {!section-units}.

        [factors] contains the following units:
        - year
        - month
        - week
        - day
        - hour
        - minute
        - second
        - millisecond
        - microsecond
    *)
    (* TODO TEST assert non-empty *)

    (**/**)
    module M = Map.Make (String)
    let pmap = foldr (fun (_,n,p) m -> M.add n p m) M.empty factors
    (**/**)

    let units =
      let rec loop (m, units) = function
      | (p,n,ns)::fs when m*p < p -> units               (* overflow, stop *)
      | (p,n,ns)::fs (* else *)   -> loop (m*p, (m*p,n,ns)::units) fs
      |           []              -> units
      in
      rev factors |> loop (!1,[])
    (** All the units, with their multipliers, in descending order,
        that are resolvable with base number type [N].

        For example, with {!Units.Int} base units, [units] contains
        [(86400000000, "day", "days")], because one day is [86_400_000_000]
        microseconds, which is the minimum factor or "base unit".

        If you use a small base unit, then [units] may not contain all
        the units in [factors].  For example:

        {[
module T = Units.Time (struct
  include Int32
  let ( + ), ( - ), ( * ), ( / ), (mod) = add, sub, mul, div, rem
end)
T.(minimum,maximum) = ((1l, "microsecond", "microseconds"), (60000000l, "minute", "minutes"))
        ]}

*)
    (* TODO TEST assert descending *)

    (** [maximum] is the maximum resolvable unit with base type [t]. *)
    let maximum = units |> hd

    (** [minimum] is the minimum resolvable unit with base type [t]. *)
    let minimum = units |> last

    (**/**)
    let multiplier name =
      let rec loop = function
      | (m,n,_)::us when n = name -> m
      | (_,_,_)::us (* else *)    -> loop us
      |          []               -> invalid_arg name
      in
      loop units

    let plural name =
      let rec loop = function
      | (m,n,p)::us when n = name -> p
      | (_,_,_)::us (* else *)    -> loop us
      |          []               -> invalid_arg name
      in
      loop units
    (**/**)

    let microseconds n = N.(multiplier "microsecond" * n)
    (** [(microseconds n)] is [n] microseconds in base units. *)

    let milliseconds n = N.(multiplier "millisecond" * n)
    (** [(milliseconds n)] is [n] milliseconds in base units. *)

    let seconds n = N.(multiplier "second" * n)
    (** [(seconds n)] is [n] seconds in base units. *)

    let minutes n = N.(multiplier "minute" * n)
    (** [(minutes n)] is [n] minutes in base units. *)

    let hours   n = N.(multiplier "hour"   * n)
    (** [(hours n)] is [n] hours in base units. *)

    let days    n = N.(multiplier "day"    * n)
    (** [(days n)] is [n] days in base units. *)

    let weeks   n = N.(multiplier "week"   * n)
    (** [(weeks n)] is [n] weeks in base units. *)

    let months  n = N.(multiplier "month"  * n)
    (** [(months n)] is [n] months in base units. *)

    let years   n = N.(multiplier "year"   * n)
    (** [(years n)] is [n] years in base units. *)

    let to_units t =            (* TODO DOC or else hide! *)
      let open N in
      let rec loop (t,r) = function
        |          [] -> t,r
        | (u,n,_)::us -> loop (t mod u, ((n, t / u) :: r)) us
      in
      loop (t,[]) units |> snd |> rev |> dropwhile (fun (_,v) -> v = !0)

    let long ?(zero=N.(to_string zero)) ?(sep=", ") ?(last=" and ") t =
      let plurals (n,v) = String.plural ~reg:(k (M.find n pmap)) (N.to_int v) n, v in
      let jam (n,v) = sprintf "%s %s" (N.to_string v) n in
      match to_units t |> filter (snd $ (<>) N.zero) |> map (plurals $ jam) |> rev with
      | []      -> zero
      | nv::[]  -> nv
      | nv::nvs -> (rev nvs |> join ~sep) ^ last ^ nv
    (** [(long ?zero ?sep ?last t)] is a long-format string representation of time [t].

        [~zero] is returned when [(t = N.zero)] (default: N.(to_string zero))

        [~sep] is the string that separates all the units of the time
        description (default: [", "]), except that [~last] is used as
        the separator between the last unit and the one preceding it.

        Examples:
        - [(days !22 + minutes !10 |> long) = ("3 weeks, 1 day and 10 minutes")]
        - [(days !22 |> long) = ("3 weeks and 1 day")]
        - [(days !1 |> long) = ("1 day")]
        - [(days !0 |> long) = ("0")]
        - [(days !0 |> long ~zero:"no days, baby") = ("no days, baby")]
    *)

    (* let format t = to_units t |> map (N.to_int $ sprintf "%02d") |> join ~sep:":" *)

    (** [(of_string ?min ?sep str)] converts a string in a certain format to a [t].

        The format of the string that of positive integers (with
        optional leading zeroes) separated by any of the characters in
        [~sep] (default: [":"]).  [~min] is (singular form of) the
        name of minimal unit i.e. the unit of the rightmost integer
        (default: ["second"]).

        For example, [(of_string 10)] is 10 seconds in base units (due
        to the default [~sep]), while [(of_string "1:12:04")] is 1 hour, 12 minutes
        and 4 seconds.
    *)
    let of_string ?(min="second") ?(sep=":") str =
      let rec minimize acc = function
      | []                              -> acc
      | ((_,n,_) as f)::fs when n = min -> f::acc
      | ((_,n,_) as f)::fs (* else *)   -> minimize (f::acc) fs
      in
      let rec loop xs us t = match xs,us with
        |      _,            []  -> invalid_arg "ran out of units"
        | (x::xs), ((u,_,_)::us) -> loop xs us N.(t + x * u)
        |     [],  _             -> t
      in
      let list = split ~sep str |> map N.of_string in
      if any (fun n -> N.compare n !0 = ~-1) list
      then invalid_arg ("negative components not allowed: %s" % str)
      else if filter (fun (_,n,ns) -> n = min) units = []
      then invalid_arg ("no such unit: %s" % min)
      else loop (rev list) (minimize [] units) (N.of_int 0)

    (** Functions to convert between month names and month numbers. *)
    module Month = struct

      (** The type of months. *)
      type t = | JANUARY | FEBRUARY | MARCH | APRIL | MAY | JUNE
               | JULY | AUGUST | SEPTEMBER | OCTOBER | NOVEMBER | DECEMBER

      (** The months of the year, in ascending order. *)
      let list = [JANUARY;  FEBRUARY;  MARCH;  APRIL;  MAY;  JUNE;
                  JULY;  AUGUST;  SEPTEMBER;  OCTOBER;  NOVEMBER;  DECEMBER]

      (** [(of_int n)] returns the month corresponding the number [n]; 1-based.

          Example: [of_int 1 = JANUARY]
      *)
      let of_int = function
      | 1 -> JANUARY | 2 -> FEBRUARY | 3 -> MARCH | 4 -> APRIL
      | 5 -> MAY | 6 -> JUNE | 7 -> JULY | 8 -> AUGUST
      | 9 -> SEPTEMBER | 10 -> OCTOBER | 11 -> NOVEMBER | 12 -> DECEMBER
      | _ -> raise (Invalid_argument "of_int")

      (** {1 English Months} *)

      (** English language month names. *)
      module English = struct

        (** [(to_string m)] returns full name of month [m]. *)
        let to_string = function
        | JANUARY -> "January" | FEBRUARY -> "February" | MARCH -> "March"
        | APRIL -> "April" | MAY -> "May" | JUNE -> "June" | JULY -> "July"
        | AUGUST -> "August" | SEPTEMBER -> "September" | OCTOBER -> "October"
        | NOVEMBER -> "November" | DECEMBER -> "December"

        (** The full names of the months of the year, in ascending order. *)
        let longs = map to_string list

        (** The abbreviated (3-letter) names of the months of the year, in ascending order. *)
        let shorts = map (to_string $ String.take 3) list
      end

      (** {1 Default Month Names}

          Default month names are in English.*)

      (** [to_string] is {!English.to_string}. *)
      let to_string = English.to_string
      (** [longs] is {!English.longs}. *)
      let longs = English.longs
      (** [shorts] is {!English.shorts}. *)
      let shorts = English.shorts
    end

  end
end

(** {1:time Format Date and Time} *)

(** {1 Parsing and Formatting Date and Time}

    Including a pure OCaml implementation of [strftime(3)].
*)

module Time = struct (*$<Time *)
  include Prelude_time

  (** {1 Parsing Dates}

      {!parse} doesn't attempt to parse a free-form date, but rather
      a date specified by a {!strftime} format; it's to some extent
      the inverse of {!strftime}.
   *)

  exception UNSUPPORTED of char
  (** [(UNSUPPORTED (c))] is the type of exception raised by {!tm_of_string} for an unsupported [strtime] escape character [c]. *)

  type locale = {
      full_months  : string list;
      abbr_months  : string list;
      full_days    : string list;
      abbr_days    : string list;
      ampm         : string list;
    }
  (** The type of locale-specific data for month names. *)

  (* "(The specific names used in the current locale can be ob‐
       tained by calling nl_langinfo(3) with ABMON_{1–12} as an argument.)" etc *)
  (** [locale] is the values for the [en_US] locale. *)
  let locale =
    let full_months = ["january"; "february"; "march"; "april"; "may"; "june"; "july";
                       "august"; "september"; "october"; "november"; "december"] in
    let full_days = [
        "sunday"; "monday"; "tuesday"; "wednesday"; "thursday"; "friday"; "saturday";] in
    {
      full_months;
      abbr_months = map (String.take 3) full_months;
      full_days;
      abbr_days = map (String.take 3) full_days;
      ampm = ["am";"pm"];
    }

  (**/**)
  let to_num str = reraise (Invalid_argument ("not a number: %S" % str)) int_of_string str

  let expand pat =
    let open Str in
    pat
    |> global_replace (regexp {|%D|}) {|%m/%d/%y|}
    |> global_replace (regexp {|%c|}) {|%a %b %e %H:%M:%S %Y|}
    |> global_replace (regexp {|%F|}) {|%Y-%m-%d|}
    |> global_replace (regexp {|%G|}) {|%Y|}
    |> global_replace (regexp {|%g|}) {|%y|}
    |> global_replace (regexp {|%R|}) {|%H:%M|}
    |> global_replace (regexp {|%r|}) {|%H:%M:%S %p|}
    |> global_replace (regexp {|%T|}) {|%H:%M:%S|}
    |> global_replace (regexp {|%v|}) {|%e-%b-%Y|}
    |> global_replace (regexp {|%X|}) {|%H:%M:%S|}
    |> global_replace (regexp {|%x|}) {|%m/%d/%y|}
  (**/**)

  (* NB no support for %E or %O modifiers *)
  (* TODO IMPROVE maybe we can get away with Str after all! *)
  (** [(parse format str)] parses [str] according to {!strftime}
      format [format] and returns values for each [%]-escape in
      [format].

      Example of parsing an American-style date format:
      {[(Time.parse "%m/%e/%Y" "7/1/2003") = [|"7"; "1"; "2003"|]]}

      @raise Invalid_arg if [format] contains an invalid escape.
      @raise Failure if [str] doesn't match [format]
   *)
  let parse pat str =
    let wordrex word =
      let eachc c =
        let open String in
        let c = String.make 1 c in
        let c = lowercase_ascii c in
        uppercase_ascii c |> sprintf "[%s%s]" c
      in
      {|\b%s\b|} % String.maps eachc word
    in
    let escape =
      let capture = sprintf {|\(%s\)|} in
      let dofm = {|0[1-9]\|1[0-9]\|2[0-9]\|3[01]|} in
      let monthnum =  {|[01][0-9]\|[0-9]|} in
      let d2 = {|[0-9][0-9]|} in
      function
      | 'A' -> (* locale's full weekday name. *)
         map wordrex locale.full_days |> join ~sep:{|\||} |> capture
      | 'a' -> (* locale's abbreviated weekday name. *)
         map wordrex locale.abbr_days |> join ~sep:{|\||} |> capture
      | 'B' -> (* locale's full month name. *)
         map wordrex locale.full_months |> join ~sep:{|\||} |> capture
      | 'b'    (* locale's abbreviated month name = %h *)
      | 'h' -> (* locale's abbreviated month name = %b *)
         map wordrex locale.abbr_months |> join ~sep:{|\||} |> capture
      | 'C' -> (* century (a year divided by 100 and truncated to an integer) as a decimal number [00,99]. *)
         capture {|[0-9][0-9]|}
      | 'c' -> (* locale's appropriate date and time representation. *)
         (* POSIX locale: %a %b %e %H:%M:%S %Y *)
         assert false           (* via expand below *)
      | 'D' -> (* date in the format ``%m/%d/%y''. *)
         assert false           (* via expand below *)
      | 'd' -> (* day of the month as a decimal number [01,31]. *)
         capture dofm
      | 'e' -> (* day of month as a decimal number [1,31]; single digits are preceded by a blank. *)
         capture ({|%s\| ?[1-9]|} % dofm)
      | 'F' -> (* date in the format ``%Y-%m-%d'' *)
         assert false           (* via expand below *)
      | 'G' -> (* ISO 8601 year with century as a decimal number. *)
         assert false           (* via expand below *)
      | 'g' -> (* ISO 8601 year without century as a decimal number *)
         assert false           (* via expand below *)
      | 'H' -> (* hour (24-hour clock) as a decimal number [00,23]. *)
         capture {|[012][0-9]|}
      | 'I' -> (* hour (12-hour clock) as a decimal number [01,12]. *)
         capture {|[01][0-9]|}
      | 'j' -> (* day of the year as a decimal number [001,366]. *)
         capture {|[0-9][0-9][0-9]|}
      | 'k' -> (* hour (24-hour clock) as a decimal number [0,23]; single digits are preceded by a blank. *)
         capture {|[12][0-9]\| [0-9]|}
      | 'l' -> (* hour (12-hour clock) as a decimal number [1,12]; single digits are preceded by a blank. *)
         capture {|1[0-2]\| [0-9]|}
      | 'M' -> (* minute as a decimal number [00,59]. *)
         capture {|[0-5][0-9]|}
      | 'm' -> (* month as a decimal number [01,12]. NB I allow single digit month numbers (strftime doesn't!)*)
         capture monthnum
      | 'n' -> (* is replaced by a newline. *)
         capture "\n"
      | 'p' -> (* locale's equivalent of either ``AM'' or ``PM''. *)
         map wordrex locale.ampm |> join ~sep:{|\||} |> capture
      | 'R' -> (* time in the format ``%H:%M''. *)
        assert false           (* via expand below *)
      | 'r' -> (* locale's representation of 12-hour clock time using AM/PM notation. *)
        assert false           (* via expand below *)
      | 'S' -> (* second as a decimal number [00,61]. *)
         capture {|[0-6][0-9]|}
      | 's' -> (* number of seconds since the Epoch, UTC (see mktime(3)). *)
         capture {|[0-9]+|}
      | 'T' -> (* time in the format ``%H:%M:%S''. *)
        assert false           (* via expand below *)
      | 't' -> (* is replaced by a tab. *)
         capture "\t"
      | 'U'    (* week number of the year (Sunday as the first day of the week) as a decimal number [00,53]. *)
      | 'V'    (* week number of the year (Monday as the first day of the week) as a decimal number [01,53]. *)
      | 'W' -> (* week number of the year (Monday as the first day of the week) as a decimal number [00,53]. *)
         capture {|[0-5][0-9]|}
      | 'u' -> (* weekday (Monday as the first day of the week) as a decimal number [1,7]. *)
         capture {|[1-7]|}
      | 'v' -> (* date in the format ``%e-%b-%Y''. *)
         assert false           (* via expand below *)
      | 'w' -> (* weekday (Sunday as the first day of the week) as a decimal number [0,6]. *)
         capture {|[0-6]|}
      | 'X' -> (* locale's appropriate time representation. *)
         assert false           (* via expand below *)
      | 'x' -> (* locale's appropriate date representation. *)
         assert false           (* via expand below *)
      | 'Y' -> (* year with century as a decimal number. *)
         capture {|[0-9][0-9][0-9][0-9]|}
      | 'y' -> (* year without century as a decimal number [00,99]. *)
         (* TODO AMIBIGUOUS 23 = 1923 or 2023? *)
         capture d2
      | 'Z' -> (* time zone name. *)
         capture {|[A-Z]+|}
      | 'z' -> (* offset from ITC in the ISO 8601 format ``[+-]hhmm''. *)
         capture {|Z\|[+-]\([0-2]:?[0-9]\|[0-2]\)|}
      | '%' -> (* is replaced by `%'. *)
         {|%|}
      |  c  -> raise (invalid_arg ("unknown %%-escape: %%%c" % c))
    in
    let pat' = expand pat in
    let ngroups =
      Str.(full_split (regexp {|%[^%]|}) pat')
      |> foldl (fun n -> function Str.Delim _ -> succ n | Str.Text _ -> n) 0
    in
    let buf = Buffer.create (String.len pat') in
    let rec parse' = function
      | pre, Some rest -> Str.quote pre |> Buffer.add_string buf;
                          let esc, rest = String.splitat 1 rest in
                          assert (String.length esc = 1);
                          escape esc.[0] |> Buffer.add_string buf;
                          String.cut ~sep:"%" rest |> parse'
      | pre, None      -> Str.quote pre |> Buffer.add_string buf; Buffer.contents buf
    in
    let rex = String.cut ~sep:"%" pat' |> parse' in
    let regexp = Str.regexp rex in
    let each i acc = match Str.matched_group i str with
      | exception Not_found -> assert false (* via Str.string_match success below *)
      | x -> x :: acc
    in
    if Str.string_match regexp str 0
    then foldir each [] (1,ngroups)
    else failwith "parse"

  (**/**)
  (* test helpers *)
  module TestHelp = struct
    let randomc () = sprintf "%s %s %2d %02d:%02d:%02d %02d%02d"
                       (choose 1 locale.abbr_days |> hd)
                       (choose 1 locale.abbr_months |> hd)
                       (Random.int 31 + 1)
                       (Random.int 23 +1)
                       (Random.int 60 +1)
                       (Random.int 60 +1)
                       (Random.int 2 + 19)
                       (Random.int 24)

    let checknum parse format digits numformat n = (* the parse parm makes qtest happy *)
      match sprintf (format_of_string numformat) n |> parse format with
      | exception exn -> false
      | str::[] -> begin match int_of_string str with
                   | exception _ -> false
                   | m -> String.len str = digits && m = n end
      | fields  -> false

    let sometznames = ["ACT"; "CET"; "EET"; "EST"; "GMT"; "HST"; "LHI"; "MET";
                       "MST"; "NSW"; "PRC"; "ROC"; "ROK"; "UCT"; "UTC"; "WET"]

  end
  (**/**)

  (*$T parse
    (flatmap (parse "%a") locale.abbr_days |> all (flip mem locale.abbr_days))
    (flatmap (parse "%A") locale.full_days |> all (flip mem locale.full_days))
    (not (succeeds (parse "%a") "monkey"))
    (parse "%a" "mon key") = ["mon"]
    (flatmap (parse "%b") locale.abbr_months |> all (flip mem locale.abbr_months))
    (flatmap (parse "%h") locale.abbr_months |> all (flip mem locale.abbr_months))
    (flatmap (parse "%B") locale.full_months |> all (flip mem locale.full_months))
    (not (succeeds (parse "%b") "janky"))
    (parse "%b" "jan key") = ["jan"]
    (parse "%c" "Tue Aug  8 15:52:17 2023" = ["Tue"; "Aug"; " 8"; "15"; "52"; "17"; "2023"])
    (succeeds (TestHelp.randomc >> parse "%c") ())
    (parse "%n" "\n") = ["\n"]
    (parse "foo%nbar" "foo\nbar") = ["\n"]
    (parse "%p" "am") = ["am"]
    (parse "%p" "AM") = ["AM"]
    (parse "%p" "pm") = ["pm"]
    (parse "%p" "PM") = ["PM"]
    (not (succeeds (parse "%p") "foo"))
    let secs = Unix.time () |> int_of_float in secs |> string_of_int |> parse "%s" |> map int_of_string = [secs]
    (parse "%t" "\t") = ["\t"]
    (parse "foo%tbar" "foo\tbar") = ["\t"]
   *)
  (*$Q parse
    Q.(int_range 0 99) (fun n -> sprintf "%02d" n |> parse "%C" |> hd |> int_of_string = n)
    Q.(triple (int_range 0 11) (int_range 1 31) (int_range 0 99)) (fun (m,d,y) -> sprintf "%02d/%02d/%02d" m d y |> parse "%D" |> map int_of_string = [m;d;y])
    Q.(int_range 1 31) (fun d -> sprintf "%02d" d |> parse "%d" |> map int_of_string = [d])
    Q.(int_range 1 31) (fun e -> (if e < 10 then "%2d" % e else sprintf "%02d" e) |> parse "%e" |> map (String.trimleft " " >> int_of_string) = [e])
    Q.(triple (int_range 1970 2999) (int_range 0 11) (int_range 1 31)) (fun (y,m,d) -> sprintf "%4d-%02d-%02d" y m d |> parse "%F" |> map int_of_string = [y;m;d])
    Q.(pair (int_range 19 20) (int_range 0 99)) (fun (c,d) -> TestHelp.checknum parse "%G" 4 "%d" (c*100+d))
    Q.(int_range 0 99) (fun d -> TestHelp.checknum parse "%g" 2 "%02d" d)
    Q.(int_range 0 23) (fun h -> TestHelp.checknum parse "%H" 2 "%02d" h)
    Q.(int_range 0 12) (fun i -> TestHelp.checknum parse "%I" 2 "%02d" i)
    Q.(int_range 0 59) (fun m -> TestHelp.checknum parse "%M" 2 "%02d" m)
    Q.(int_range 1 12) (fun m -> TestHelp.checknum parse "%m" 2 "%02d" m)
    Q.(int_range 1 9)  (fun m -> TestHelp.checknum parse "%m" 1 "%d" m)
    Q.(pair (int_range 0 23) (int_range 0 59)) (fun (h,m) -> sprintf "%02d:%02d" h m |> parse "%R" |> map int_of_string = [h;m])
    Q.(triple (int_range 0 23) (int_range 0 59) (int_range 0 59)) (fun (h,m,s) -> let ampm = choose 1 ["am";"pm"] |> hd in (sprintf "%02d:%02d:%02d %s" h m s ampm |> parse "%r" |> splitat 3 |> fun (init,last) -> map int_of_string init, hd last) = ([h;m;s],ampm))
    Q.(int_range 0 59) (fun s -> TestHelp.checknum parse "%S" 2 "%02d" s)
    Q.(triple (int_range 0 23) (int_range 0 59) (int_range 0 59)) (fun (h,m,s) -> sprintf "%02d:%02d:%02d" h m s |> parse "%T" |> map int_of_string = [h;m;s])
    Q.(triple (int_range 0 23) (int_range 0 59) (int_range 0 59)) (fun (h,m,s) -> sprintf "%02d:%02d:%02d" h m s |> parse "%X" |> map int_of_string = [h;m;s])
    Q.(pair (int_range 0 5) (int_range 0 9)) (fun (a,b) -> let w = sprintf "%d%d" a b in parse "%U" w = [w])
    Q.(pair (int_range 0 5) (int_range 0 9)) (fun (a,b) -> let w = sprintf "%d%d" a b in parse "%V" w = [w])
    Q.(pair (int_range 0 5) (int_range 0 9)) (fun (a,b) -> let w = sprintf "%d%d" a b in parse "%W" w = [w])
    Q.(triple (int_range 1 31) (oneofl locale.abbr_months) (int_range 1970 2900)) (fun (e,b,y) -> sprintf "%2d-%s-%d" e b y |> parse "%v" = ["%2d" % e;"%s" % b;"%d" % y])
    Q.(int_range 0 6) (fun w -> "%d" % w |> parse "%w" = ["%d" % w])
    Q.(triple (int_range 0 11) (int_range 1 31) (int_range 0 99)) (fun (m,d,y) -> sprintf "%02d/%02d/%02d" m d y |> parse "%x" |> map int_of_string = [m;d;y])
    Q.(pair (int_range 19 29) (int_range 0 99)) (fun (c,d) -> let n = c*100+d in "%d" % n |> parse "%Y" = ["%d" % n])
    Q.(int_range 0 99) (fun d -> "%02d" % d |> parse "%y" = ["%02d" % d])
    Q.(oneofl TestHelp.sometznames) (fun tz -> parse "%Z" tz = [tz])
   *)

(* these two don't work *)
(* Q.(small_printable_string) (fun s -> let fmt = String.maps (fun c -> if c='%' then "%%" else String.make 1 c) s in parse fmt s = []) *)
(* Q.(quad (oneofl ["+";"-"]) (int_range 0 23) bool (int_range 0 59)) (fun (pm,h,colon,m) -> let x = sprintf "%s%02d%s%02d" pm h (if colon then ":" else "") m in parse "%z" x = [x] ) *)

  (**/**)
  (*  I AM NOT A DOCSTRING but I used to be, so we're keeping this text.

      [(tm_of_format format matches)] takes a {!strftime} format and
      list of matching escape values and returns a [Unix.tm].

      You should use {!tm_of_string} instead.

      Each [%]-sescape in [format] must have a corresponding and
      appropriate value in [matches].  If there are {i N} escapes in
      [format], there must be {i N} values in [matches].

      In normal use, the [format] is the same one that's passed to {!parse}.

      Only a few {!strftime} escapes are supported at this time.

      @raise Invalid_arg if [format] contains an invalid escape.
      @raise UNSUPPORTED if [format] contains an unsupported {!strftime} escape.
      @raise NYI_TM_OF_FORMAT if [format] contains an as-yet-unimplemented escape.
  *)
  let tm_of_format pat list =
    let tm = {Unix.tm_sec = 0; tm_min = 0; tm_hour = 0; tm_mday = 1; tm_mon = 0;
              tm_year = 0; tm_wday = 0; tm_yday = 0; tm_isdst = false}
    in
    let rec escapes acc = function
      | _, Some rest -> let esc, rest = String.splitat 1 rest in
                        assert (String.length esc = 1);
                        String.cut ~sep:"%" rest |> escapes (esc.[0] :: acc)
      | _, None      -> rev acc
    in
    let each esc value tm = match esc with
      | 'A' -> (* locale's full weekday name. *)
         tm
      | 'a' -> (* locale's abbreviated weekday name. *)
         tm
      | 'B' -> (* locale's full month name. *)
         { tm with Unix.tm_mon = pos ~eq:(cmpeq AIString.compare) value locale.full_months } (* TODO CATCH *)
      | 'b'    (* locale's abbreviated month name. *)
      | 'h' -> (* locale's abbreviated month name. *)
         { tm with Unix.tm_mon = pos ~eq:(cmpeq AIString.compare) value locale.abbr_months } (* TODO CATCH *)
      | 'C' -> (* century (a year divided by 100 and truncated to an integer) as a decimal number [00,99]. *)
         tm
      | 'c' -> (* locale's appropriate date and time representation. *)
         assert false           (* via expand below *)
      | 'D' -> (* date in the format ``%m/%d/%y''. *)
         assert false           (* via expand below *)
      | 'd'    (* day of the month as a decimal number [01,31]. *)
      | 'e' -> (* day of month as a decimal number [1,31]; single digits are preceded by a blank. *)
         {tm with Unix.tm_mday = to_num (String.trimleft " " value)}
      | 'F' -> (* date in the format ``%Y-%m-%d'' *)
         assert false           (* via expand below *)
      | 'G' -> (* ISO 8601 year with century as a decimal number. *)
         assert false           (* via expand below *)
      | 'g' -> (* ISO 8601 year without century as a decimal number *)
         assert false           (* via expand below *)
      | 'j' -> (* day of the year as a decimal number [001,366]. *)
         raise (UNSUPPORTED 'j') (* TODO DIFFICULT leap years *)
      | 'H' -> (* hour (24-hour clock) as a decimal numbe [00,23]. *)
         { tm with Unix.tm_hour = to_num value }
      | 'I' -> (* hour (12-hour clock) as a decimal number [01,12]. *)
         invalid_arg "%I: ambiguous"
      | 'k' -> (* hour (24-hour clock) as a decimal number [0,23]; single digits are preceded by a blank. *)
         { tm with Unix.tm_hour = to_num (String.trimleft " " value) }
      | 'l' -> (* hour (12-hour clock) as a decimal number [1,12]; single digits are preceded by a blank. *)
         invalid_arg "%i: ambiguous"
      | 'M' -> (* minute as a decimal number [00,59]. *)
         { tm with Unix.tm_min = to_num value }
      | 'm' -> (* month as a decimal number [01,12]. *)
         {tm with Unix.tm_mon = to_num value - 1}
      | 'n' -> (* is replaced by a newline. *)
         tm
      | 'p' -> (* locale's equivalent of either ``AM'' or ``PM''. *)
         tm
      | 'R' -> (* time in the format ``%H:%M''. *)
         assert false           (* via expand below *)
      | 'r' -> (* locale's representation of 12-hour clock time using AM/PM notation. *)
         assert false           (* via expand below *)
      | 'S' -> (* second as a decimal number [00,61]. *)
         { tm with Unix.tm_sec = to_num value }
      | 's' -> (* number of seconds since the Epoch, UTC (see mktime(3)). *)
         reraise (Invalid_argument ("not a number: %S" % value)) float_of_string value
         |> Unix.localtime; (* TODO QUESTION should this be Unix.gmtime? parameterize? *)
      | 'T' -> (* time in the format ``%H:%M:%S''. *)
        assert false           (* via expand below *)
      | 't' -> (* is replaced by a tab. *)
         tm
      | 'U' -> (* week number of the year (Sunday as the first day of the week) as a decimal number [00,53]. *)
         raise (UNSUPPORTED 'U')
      | 'u' -> (* weekday (Monday as the first day of the week) as a decimal number [1,7]. *)
         tm
      | 'V' -> (* week number of the year (Monday as the first day of the week) as a decimal number [01,53]. *)
         raise (UNSUPPORTED 'V')
      | 'v' -> (* date in the format ``%e-%b-%Y''. *)
         assert false
      | 'W' -> (* week number of the year (Monday as the first day of the week) as a decimal number [00,53]. *)
         raise (UNSUPPORTED 'W')
      | 'w' -> (* weekday (Sunday as the first day of the week) as a decimal number [0,6]. *)
         tm
      | 'X' -> (* locale's appropriate time representation. *)
         assert false            (* via expand below *)
      | 'x' -> (* locale's appropriate date representation. *)
         assert false           (* via expand below *)
      | 'Y' -> (* year with century as a decimal number. *)
         { tm with Unix.tm_year = to_num value - 1900; }
      | 'y' -> (* year without century as a decimal number [00,99]. *)
         { tm with Unix.tm_year = to_num value; }
      | 'Z' -> (* time zone name. *)
         raise (UNSUPPORTED 'Z')
      | 'z' -> (* offset from ITC in the ISO 8601 format ``[+-]hhmm''. *)
         raise (UNSUPPORTED 'z')
      | '%' -> (* is replaced by `%'. *)
         tm
      |  c  -> raise (invalid_arg ("unknown %%-escape: %%%c" % c))
    in
    let pat' = expand pat in
    let escs = String.cut ~sep:"%" pat' |> escapes [] in
    if len escs = len list
    then fold_right2 each escs list tm
    else invalid_arg (to_string id list |> sprintf "num of escapes in %S (%d) does not match %S" pat (len escs))
  (**/**)

  (** [(tm_of_string format str)] parses [str] according to the
      {!strftime} [format], returning a [Unix.tm].

      The following {!strftime} escapes are {i not} supported: [%j],
      [%I], [%l], [%U], [%V], [%W], [%Z], and [%z].

      Example:
      {[(tm_of_string "%c" "Wed Aug  9 09:35:53 2023") = {Unix.tm_sec = 53; tm_min = 35; tm_hour = 9; tm_mday = 9; tm_mon = 7; tm_year = 123; tm_wday = 0; tm_yday = 0; tm_isdst = false}
      (tm_of_string "%c" "Wed Aug  9 09:35:53 2023" |> strftime "%c") = "Sun Aug  9 09:35:53 2023"
      ]}

      @raise UNSUPPORTED if [format] contains an unsupported {!strftime} escape.
      @raise Invalid_arg if [format] contains an invalid escape.
      @raise Failure if [format] does not match [str]
   *)
  let tm_of_string format str =
    parse format str |> tm_of_format format

end (*$>*)

(** {1:messages Messages} *)

(** Easy to use message functions for warnings, errors, and verbosity,
    with a lot of fiddly options. *)
(* TODO TESTS *)
module Message = struct
  (** {1 Tutorial}

      In simplest form, {!message} is just like [Printf.eprintf]
      except that it prefixes [argv0] to the output.

      {!warning} and {!fatal} are like {!message} except they add the
      word "warning" or "FATAL" (respectively) to the prefix, and
      {!fatal} also terminates execution (with exit status 1 (by
      default)).

      {!syntax} is for generating syntax error messages with a
      filename and line number in the usual Unix format understood by
      editors like Emacs and Vim.

      {!verbose}, {!wrap} and {!maybe} are for generating progress
      messages at varying levels of verbosity.
  *)
  (** {2 Examples}
      Here we assume that [(hd argv = "a.out")].
      - [(message "%d + %d = %d" 2 4 (2+4))] --stderr--> ["a.out: 2 + 4 = 6"]
      - [(warning "%d + %d <> %d" 2 4 100)] --stderr--> ["a.out: warning: 2 + 4 <> 100"]
      - [(fatal "out of stuff!")] --stderr--> ["a.out: FATAL: out of stuff!"] and [Sys.exit 1]
      - [(let fn,ln = "z.ml",134 in syntax fn ln "syntax error!")] --stderr--> ["z.ml:134:syntax error!"]
      - [(verbosity := 0; verbose 1 "something computed")] {i no output}
      - [(verbosity := 1; verbose 1 "something computed")] --stderr--> ["a.out: something computed"]
      - [(verbosity := 2; verbose 1 "something computed")] --stderr--> ["a.out: something computed"]
      - [(verbosity := 1; verbose 2 "something computed")] {i no output}
      - [(verbosity := 1; wrap 1 "sleeping... " Unix.sleep 3)] --stderr--> ["a.out: sleeping... "] {i (after 3 seconds...) } ["done"]

      See {!message} below for a million optional parameters.
  *)

  open Format
  (** {1 Destinations} *)
  (** [of_chan] is {!Format.formatter_of_out_channel}. *)
  let of_chan c = formatter_of_out_channel c

  (** [null] is the /dev/null of destinations; setting [~ff:null]
      results in no message being printed. *)
  let null = null

  (** {1 Defaults} *)
  (** [myself] is the {i program name}: the default value for
      {!message}'s [~myself] parameter.

      The default is [(Filename.basename argv0 |> some)].

      Setting [myself] to [None] results in no program name prefix;
      setting it to [(Some p)] changes the program name to [p]. *)
  let myself = ref (Filename.basename argv0 |> some)
  (**/**)
  let getmyself () = !myself
  (**/**)
  (** [sep] is the default string [(": ")] used to separate all the
      components of the message.  *)
  let sep = ": "

  (** {1 Message Printers} *)
  (** [(message ~ff fmt ARGS ...)] formats and prints a message as for
      [(Format.fprintf ff fmt ARGS ...)], with behavior modified by
      the many optional arguments.

      {[ (let fn = "foo" in message "%s: file not found" fn) ]}
      prints
      ["test: foo: file not found"], assuming [(!myself = Some "test")].

      [~ff] is the [Format.formatter] that the message is printed on; the default is [Format.err_formatter]

      If [~myself:m] is provided, it overrides {!myself} for this call
      and uses [m] for the program name; passing [~myself:""]
      results in no program name prefix at all.

      The printed result is composed of the following elements, in
      this order, separated by the value of [~sep]; elemants that are
      [None] are elided:
      - [~myself]
      - [~prefix]
      - the value of [(Format.sprintf fmt ARGS ...)]

      If [~quote] is given, it is applied to each of these elements
      before the message is assembled.  This is expected to be used to
      quote instances of [~sep] to assure that the message text is
      parseable, but the function can do anything you like.

      In any case, if [~nl:true] (the default), a terminating
      end-of-line string as appended to the formatted message, and if
      [~flush:true] (the default), the message is flushed.

      If [~exit:status] is provided, then the program is terminated by
      calling [(Stdlib.exit status)] after the message is printed.

      Examples, assuming [argv0] returns ["test"]:
      - [(message "starting up")] prints ["test: starting up"]
      - [(message ~prefix:"INFO" "starting up")] prints ["test: INFO: starting up"]
      - [(message ~myself:None "starting up")] prints ["starting up"]
      - [(message ~myself:None ~prefix:"INFO" "starting up")] prints ["INFO: starting up"]
      - [(message ~myself:"production" "starting up")] prints ["production: starting up"]
  *)
  let message ?myself ?(sep=sep) ?quote ?(nl=true) ?exit ?prefix ?(flush=true) ?(ff=err_formatter) fmt =
    let p str =
      let maybe f x = match f with None -> x | Some f -> f x in
      let myself = match myself with None -> getmyself () | Some "" -> None | m -> m in
      let pp x = maybe quote x |> pp_print_string ff in
      let o f = function None  -> [] | Some s -> [thunk f s] in
      let b f = function false -> [] | true   -> [thunk f ()] in
      [o pp myself; o pp prefix; o pp (Some str)]
      |> concat
      |> intersperse (thunk (pp_print_string ff) sep)
      |> snoc []
      |> flip List.append [b (pp_print_newline ff) nl; b (pp_print_flush ff) flush; o Stdlib.exit exit]
      |> concat
      |> iter (flip (@@) ())
    in
    ksprintf p fmt

  (** [(warning fmt ARGS ...)]  calls {!message}, setting
      [~prefix:"warning"]; all of {!message}'s optional arguments
      supported. *)
  let warning ?myself ?sep ?quote ?nl ?exit ?(prefix="warning") ?flush ?ff fmt =
    message ?myself ?sep ?quote ?nl ?exit ~prefix ?flush ?ff fmt

  (** [(syntax fn ln fmt)] is [(message ~prefix:(("%s:%d" % fn) ln) fmt)].

      All of [messages]'s other optional parameters are accepted. *)
  let syntax ?myself ?sep ?quote ?nl ?exit ?flush ?ff fn ln fmt =
    message ~myself:"" ?sep ?quote ?nl ?exit ~prefix:(("%s:%d" % fn) ln) ?flush ?ff fmt

  (** [(fatal fmt ARGS ...)] calls {!message}, setting
      [~prefix:"FATAL"] and [~exit:1] (thus terminating the program);
      all of {!message}'s optiional arguments supported. *)
  let fatal : ?myself:string -> ?sep:string -> ?quote:(string -> string) ->
              ?nl:bool -> ?exit:int -> ?prefix:string -> ?flush:bool ->
              ?ff:Format.formatter -> ('a, unit, string, unit) format4 -> 'b
    = fun ?myself ?sep ?quote ?nl ?(exit=1) ?(prefix="FATAL") ?flush ?ff fmt ->
    message ?myself ?sep ?quote ?nl ~exit ~prefix ?flush ?ff fmt

  (** {1 Verbose Messages} *)

  (** [verbosity] determines the level of messages printed by {!verbose} when
      [~verbosity:default_verbosity] (the default).

      The initial verbosity level is [0]; you can set it or change it:

      {[Message.verbosity := 10
      incr Message.verbosity
      decr Message.verbosity
      ]}
  *)
  let verbosity = ref 0

  (** [default_verbosity] is [(Message.of_ref verbosity)]. *)
  let default_verbosity () = !verbosity

  (** [(verbose ?verbosity level fmt ARGS ...)] calls ({!message} [fmt ARGS])
      (passing all optional parameters) but only if the current
      verbosity is [>= level].  All of {!message}'s optiional
      arguments are also supported.

      [~verbosity] is called to determine the current verbosity level;
      the default value is {!default_verbosity}, which fetches the
      current value of the ref {!verbosity}.  See
      {!predef} for some convenient functions. *)
  let verbose ?myself ?sep ?quote ?prefix ?(nl=true) ?flush ?exit ?(verbosity=default_verbosity) ?ff level fmt =
    if verbosity () >= level
    then message ?myself ?sep ?quote ?prefix ~nl ?flush ?exit ?ff fmt
    else message ?myself ?sep ?quote ?prefix ~nl ?flush ?exit ~ff:null fmt

  (** [(wrap ?after level msg f x)] returns [(f x)], wrapping the
      evaluation in two calls to {!verbose} but only if the current
      verbosity is [>= level].  All of {!verbose}'s optiional
      arguments are also supported.

      [(wrap level "%s" msg f x)] is equivalent to:
      {[verbose ~nl:false ~flush:true level "%s" msg;
      let result = f x in
      verbose ~myself:"" ~nl:true ~flush:true level "%s" Option.(default (k "done") after result);
      result ]}

      Both calls pass along all other optional parameters.

      The default value for [~after] is [k "done"].
  *)
  let wrap ?myself ?sep ?quote ?prefix ?(nl=false) ?(flush=true) ?exit ?verbosity ?ff ?after level msg f x =
   verbose ?myself ?sep ?quote ~nl ?exit ?prefix ~flush ?verbosity ?ff level "%s" msg;
    let result = f x in
    let a = match after with None -> "done" | Some f' -> f' result in
    verbose ~myself:"" ?sep ?quote ~nl:true ?exit ?prefix:None ~flush ?verbosity ?ff level "%s" a;
    result

  (** [(maybe notreally lvl fmt)] is [(verbose lvl fmt)] unless
      [(notreally = true)], in which case it is [(verbose ~prefix:"NOT" lvl fmt)].

      All of [verbose]'s other optional parameters are accepted.

      Many of my programs take a [-n] option meaning to {i not really}
      do anything destructive.  Some variable, say [notreally], is set
      to [true] when this option is set.  Now I can write destructive
      functions like so:

      {v let makefile notreally fn =
  Message.maybe notreally 1 "creating file %s" fn;
  if notreally
  then ()
  else writefile ~fn "hello, world" v}

      which is silent unless [!Message.verbosity] is >= 1, in which case we have:
      {v # makefile true "/tmp/nr";;
ocaml: NOT: creating file /tmp/nr
- : unit = ()
# makefile false"/tmp/nr";;
ocaml: creating file /tmp/nr
- : unit = ()
# v} *)
  let maybe ?myself ?sep ?quote ?nl ?exit ?flush ?verbosity ?ff notreally lvl fmt =
    if notreally
    then verbose ?myself ?sep ?quote ?nl ?exit ~prefix:"NOT" ?flush ?verbosity ?ff lvl fmt
    else verbose ?myself ?sep ?quote ?nl ?exit ?flush ?verbosity ?ff lvl fmt

  (* TODO DOC *)
  let debugging = ref false     (* TODO MOVE to vars section above *)
  let debug ?myself ?sep ?quote ?nl ?exit ?(prefix="DEBUG") ?flush ?ff fmt =
    let p str =
      if !debugging
      then message ?myself ?sep ?quote ?nl ?exit ~prefix ?flush ?ff "%s" str
      else message ?myself ?sep ?quote ?nl ?exit ~prefix ?flush ~ff:Format.null "%s" str
    in
    ksprintf p fmt

  (** {1:predef Predifined Verbosity Functions}
      Suitable for {!verbose}'s [~verbosity] parameter. *)

  (** [(of_ref r)] returns a function that uses ref [r] to define the verbosity level. *)
  let of_ref r = fun () -> !r

  (** [(default_verbosity)] uses ref {!verbosity} to define the verbosity level. *)
  let default_verbosity = default_verbosity

  (** [(env v)] returns a function that uses the value of environment
      variable [v] (after conversion to an [int]) to define the
      verbosity level. *)
  let env v = fun () -> Sys.getenv v |> default 0 int_of_string

  (** [zero] is the constant verbosity level of [0]. *)
  let zero () = 0

  (** [one] is the constant verbosity level of [1]. *)
  let one () = 1

end
(* this is for syserrors; see above *)
let () = forward_message := (fun ?ff ?prefix str -> Message.message ?ff ?prefix "%s" str)

(** {1:logging Logging} *)

(** Simple functions for log files with timestamps, log rotation, etc.

    Simple usage:
    {[
    module Log = Log.Make (Default)
    Log.(opens (Chan stderr))
    Log.log 0 "starting daemon"
    ]}

    @see <http://erratique.ch/software/logs> for a much more sophisticated package.
*)
(* TODO TESTS *)
module Log = struct
  (** @closed *)
  include Message

  (** The type of log file destinations. *)
  type dest = File of string | Chan of out_channel

  (** The type of logging configurations. *)
  module type CONFIG = sig

    (** The mode used to open the log file if the destination is a {!Prelude.Log.File}.

        Should be one of [open_append] or [open_trunc].*)
    val mode : open_flag

    (** If [(signal <> None)] then {!Make.reopen} will be called
        upon receipt of this signal.

        Typically used for log rotation with [Sys.sighup]. *)
    val signal : int option

    (** A function that returns a timestamp that is printed as the
        first component of the log message.

        The timestamp can be in any conceivable format (even one
        that's not a timestamp).  You might use e.g. [(Unix.time $ truncate)]
        for an unreadable but sortable timestamp; I would
        normally use [(localtime $ strftime w3c)]. *)
    val timestamp : (unit -> string) option

    (** If [(pid <> None)] the integer is presumed to be the
        pid of this process and is appended to {!myself} in the
        traditional [syslog] style.

        The usual value would be [(Some Unix.getpid)].

        Example: if [(myself = Some "myself")] and [(pid = Some (k 100))],
        then the log message will contain ["myself[100]"]. *)
    val pid : (unit -> int) option

    (** If [(line <> None)], this function will be applied to
        the text of the log message to quote any newlines, thus
        assuring that the log message will be one single line.

        For example, you could simply convert newlines to spaces by
        using [(String.replace "\n" " ")].

        Actually this function is simply composed with {!quote} and
        can do anything you like. *)
    val line : (string -> string) option

    (** This function is called to determine the global log level.

        For example, any of [Message.(of_ref, env, zero, or one)]
        would be suitable.  *)
    val level : unit -> int

    (** See {!Message.message}. *)
    val myself : string option

    (** See {!Message.message}. *)
    val flush : bool

    (** See {!Message.message}. *)
    val sep : string

    (** See {!Message.message}. *)
    val quote : (string -> string) option
  end

  (** Functor that mass-parameterizes a logging function in accordance with configuration [C].  *)
  module Make (C : CONFIG) = struct

    (**/**)
    let behavior = ref None
    let chan = ref None
    let dest = ref None
    (**/**)

    (** The exception raised when trying to use an unopened log. *)
    exception Unopened

    (** [(opens dest)] opens the log [dest] with mode {!C.mode}.

        You must call this function before your first call to {!log}. *)
    let rec opens d =
      dest := Some d;
      begin match d with
      | File fn -> chan := Some (open_out_gen [Open_creat;Open_wronly;C.mode] 0o666 fn)
      | Chan c  -> chan := Some c
      end;
      match d, C.signal with
      | _, None | Chan _, _ -> ()
      | File _, Some s -> behavior := Some (Sys.signal s (Sys.Signal_handle handler))

    (** [(close ())] closes the log; you don't need to call
        this if {!C.flush} is [true]. *)
    and close () =
      begin match !behavior with
      | None   -> ()
      | Some b -> Sys.set_signal (Option.get C.signal) b
      end;
      match !chan with | Some c -> close_out c; chan := None | None -> ()

    (** [(reopen ())] closes and then reopens the log.

        Typically used for log rotation.
        @raise Unopened if log has never been opened *)
    and reopen () = match !dest with Some d -> close (); opens d | None -> raise Unopened

    (** [(log prio fmt ARGS ...)] writes an entry in the log opened
        via {!opens} with [(Format.fprintd fmt ARGS)] iff the current
        log level is [(>= prio)].

        The optional parameters take their defaults from the
        configuration [C], but you can override them if necessary.

        {!Make.log} is built on {!Message.message}; see that function for
        details on the optional parameters.  *)
    and log ?(mode=C.mode) ?(level=C.level) ?(sep=C.sep) ?(quote=C.quote) ?timestamp
        ?(myself=C.myself) ?pid ?prefix ?(line=C.line) ?(flush=C.flush) ?exit prio fmt =
      let quote = match quote, line with
      | Some q, Some l -> Some (q $ l)
      | Some q, None   -> Some q
      | None,   Some l -> Some l
      | None,   None   -> None
      in
      let myself = match pid with
      | Some getpid -> Option.map (flip (sprintf "%s[%d]") (getpid ())) myself
      | None -> myself in

      let myself = match timestamp, myself with
      | Some t, Some m -> Some (t () ^ sep ^ m)
      | Some t, None   -> Some (t ())
      | None,   Some m -> Some m
      | None,   None   -> None
      in
      match !chan with None -> raise Unopened
      | Some c -> verbose ~verbosity:level ~sep ?quote ?myself ?prefix ~nl:true ~flush ?exit ~ff:(of_chan c) prio fmt
    (**/**)
    and handler _ = match !chan with Some c -> reopen () | None -> ()
    (**/**)
  end

  (** A default logging configuration

      See {!CONFIG} for details.

      Typical usage is to customize it by module inclusion, e.g.:
      {[ module Config = struct include Default let myself = Some "NAME" end ]}
  *)
  module Default  = struct

    (** [mylevel] is [ref 0]. *)
    let mylevel = ref 0

    (** [level] is ({!of_ref} {!mylevel}). *)
    let level = of_ref mylevel

    (** [timestamp] is [None] (no timestamp). *)
    let timestamp = None

    (** [pid] is [None] (no pid). *)
    let pid = None

    (** [line] assures single-line log messages by changing newlines to spaces. *)
    let line = Some (String.replace "\n" " ")

    (** [signal] is [None] i.e. {i not} to re-open the log in response to any signal. *)
    let signal = None

    (** [mode] is [Open_append] so that a restarted daemon doesn't clobber old logs. *)
    let mode = Open_append

    (** [myself] is tha basename of [argv0]. *)
    let myself = argv0 |> Filename.basename |> some

    (** [flush] is [true] i.e. to flush each logged message to ensure no log data is lost. *)
    let flush = true

    (** [sep] is [": "]. *)
    let sep = ": "

    (** [quote] is [None] i.e. not to quote occurrences of {!sep} in the logged data. *)
    let quote = None
  end
end

(**/**)
module Valid = struct
  open Result

  type 'a validator = string -> ('a,string) result

  exception Invalid of string

  let valid v = v >> to_bool
  let asserted ?thing v x = match v x with
    | Ok y -> y
    | Error err -> match thing with
                   | Some t -> raise (Invalid (sprintf "invalid %s: %s" t err))
                   | None   -> raise (Invalid (sprintf "invalid: %s" err))

  let any str = Ok str
  let never str = Error str
  let enum strs str = if mem str strs then Ok str else Error str

  module Bool = struct
    let ocaml str = trapc str bool_of_string str
    let asserted v x = asserted ~thing:"Boolean" v x
  end
  module Int = struct
    let int str = trapc str int_of_string str
    let inbounds (lo,hi) str = int str >>| fun n -> lo <= n && n < hi
    let asserted v x = asserted ~thing:"integer" v x
  end
  module N0 = struct
    let n0  str = trapc str int_of_string str >>= (N0.of_int >> of_option str)
    let asserted v x = asserted ~thing:"positive integer" v x
  end
  module N1 = struct
    let n1  str = trapc str int_of_string str >>= (N1.of_int >> of_option str)
    let asserted v x = asserted ~thing:"integer > 1" v x
  end

  module Play = struct
    type t = { i : int; b : bool }
    let make i b = { i = Int.(asserted int) i; b = Bool.(asserted ocaml) b }
  end
end
(**/**)

(** {1:refer Refer Data} *)

(** Parse and generate Extended Refer databases.

    Refer is an old low-noise, low-overhead, pleasant to edit, flat-file
    data format for non-recursive key-value data with repeating and
    optional fields.

    {i Extended Refer} is simply the classic Refer format but allowing
    key names of arbitrary length (classic refer key names had to be
    exactly 1 character long).

    Terminology: a Refer "database" is a collection of Refer records,
    typically in one or more files (but possibly in a string).

    Parsed Refer records are represented by this module as association
    lists (alists; see {!Lists.Assoc}).  Keys are represented without
    their attached percent-signs.  The order of the fields in a record
    is preserved by the parser.

    @see <http://www.kohala.com/start/troff/v7man/refer/refer.ps> M. E. Lesk.: {i Some Applications of Inverted Indexes on the UNIX System.} (\[1978\])
    @see "refer(1)" [refer] man page.
*)
(* TODO TESTS *)
module Refer = struct

  (** {1 Types and Exceptions} *)

  type t = (string * string) list
  (** [t] is the type of parsed refer records. *)

  type src = string Gen.t
  (** [src] is the type of input sources for the Refer parser {!fold}.

      Each [string] of a [src] is a line of text.
  *)

  type 'a folder = int -> (t, string) result -> 'a -> 'a
  (** The type of functions called by {!Refer.fold} for each Refer record in a [src].

      An [('a folder)] takes three parameters:
      - the line-number of the first line of the alist representing the Refer record
      - a [((t,string) result)] which is [(Ok t)] if there has been no syntax error,
        or [(Error line)] is there has been a syntax error; in this
        case [line] is the erroneous line.
      - the accumulator
  *)

  type 'a errhandler = int -> string -> 'a -> 'a
  (** The type of functions suitable as the first parameter of {!Refer.witherr}.

      An [('a errhandler)] takes three parameters:
      - the line-number of the error
      - the line of text which is in error
      - the accumulator
  *)

  exception Syntax of int * string
  (** [Syntax (ln,x)] is the type of syntax error exceptions raised by {!Refer.syntax}. *)

  (** {1 Generating Refer Databases From Alists} *)

  (** A Refer record can be generated from an association list
      (alist), and a refer database can be generated from a list of
      alists.

      Certain strings cannot be valid keys (field names): e.g. the
      empty string or any string containing whitespace.

      Certain strings cannot be valid field values: e.g. any string
      that contains a "blank" line, including trailing newlines.
   *)
  module Alist = struct
    (**/**)
    let good = [("f", "f"); ("foo", "foo")]

    let bad = [("", ""); ("\n", "\n"); ("\n ", "\n "); ("\n foo", "\n
                foo"); ("foo\nbar", "foo\nbar"); ("foo\n \tbar",
                "foo\n \tbar"); ("\n \t", "\n \t"); ("foo\n \t\nbar",
                "foo\n \t bar"); ("foo\n \t \nbar", "foo\n \t bar");
                ("foo\n \t\nbar", "foo\n \t bar"); ("foo\n\n\n\n",
                "foo\n ")]

    let keyerror =
      let open String in
      let p = anyof ~these:whitespace in
      fun str -> if p str || str = "" then failwith ("invalid refer key: %S" % str) else str
    (* TODO TESTS *)

    let valuecheck str =
      let white = String.(only ~these:whitespace) in
      let rec loop acc = function
        | []                           -> rev acc |> String.concat "\n"
        | x::xs when x = "" || white x -> loop acc xs
        | x::xs (* otherwise *)        -> loop (x::acc) xs
      in
      String.cuts ~sep:"\n" str |> loop []
    (* TODO TESTS *)

    let valueerr str = let fixed = valuecheck str in if fixed = str then str else failwith "valueerr"

    let valuefix str = valuecheck str
    (**/**)

    (** [(assemble ?(fix=false) alist)] generates a valid string-representation of a refer record.

          Raises [Failure] if any of the keys or values in the alist
          are invalid (see {!valid}), unless [(fix = true)], in
          which case the invalid values will be "fixed" by converting
          one or more troublesome newlines into spaces.  N.B. Only
          alist values can be fixed; invalid keys will always raise
          [Failure] (because keys are semantically meaningful in a
          stronger way than values).  You can use {!valid} ahead
          of time to be sure no exceptions will be raised, or
          {!check} to repair keys and values in a custom manner.

          Example: if [alists] is a [(Refer.t list)], then this will print a
          valid refer file to [stdout]:
          - [List.iter (assemble >> print) alists]

          Example: [(assemble [("A", "1"); ("B", "2"); ("B", "3"); ("C", "4")] |> print)]
          prints:
          {v
%A 1
%B 2
%B 3
%C 4
          v}
     *)
    let assemble ?(fix=false) alist =
      let key = keyerror in       (* TODO ENHANCE *)
      let value = if fix then valuefix else valueerr in
      let each b = let c,s = Buffer.(add_char b, add_string b) in
                   iter (fun (k,v) -> c '%'; s (key k); c ' '; s (String.trimright "\n" v |> value); c '\n') alist in
      withbuf 1024 each

    (** [(check alist)] returns a syntax-checked version of the
          given alist, which indicates whether or not it can be converted
          to a valid string representation of a refer record.

          [(check alist)] returns an element for each binding
          [(k,v)] in the alist in the original order.  There are four
          possible forms, each carrying a binding:

          - [(`Ok (k,v))]: both the key and value are valid
          - [(`K  (k,v))]: the key is {i invalid}
          - [(`V  (k,v))]: the value is {i invalid}
          - [(`KV (k,v))]: both the key and value are {i invalid}

          Invariant: [(check alist |> map (function `Ok b | `K b | `V b | `KV b -> b)) = alist)]

     *)
    let check alist =
      let each acc (k,v) =
        match (not << succeeds keyerror) k, valuecheck v <> v with
        | false, false -> `Ok (k,v) :: acc
        | false, true  -> `V (k,v)  :: acc
        | true,  false -> `K (k,v)  :: acc
        | true,  true  -> `KV (k,v) :: acc
      in
      foldl each [] alist |> rev
    (* TODO TESTS *)

    (** [(valid alist)] is [true] iff [alist] can be converted to
          a valid string representation of a refer record, and is [false]
          otherwise. *)
    let valid alist = check alist |> all (function `Ok _ -> true | _ -> false)
    (* TODO TESTS *)
  end

  (** [assemble] is {!Alist.assemble}. *)
  let assemble = Alist.assemble

  (** {1 Random Refer Records} *)

  (** Functions to generate random Refer records. *)
  module Random = struct

    (** [(key ())] is a random string suitable for a key in a refer record.

        These keys are strings of 1-10 random uppercase characters. *)
    let key () = String.random ~size:(fun() -> Random.int 10 + 1) ~charset:(Char.upto '!' '~') () (* no space! *)
    (* TODO TESTS *)

    (** [(value ?len ?lines ())] is a random string suitable for a value in a refer record.

        [?len] is the maximum length of a line in the value (default: 60).

        [?lines] is the maximum number of lines in the value (default: 10).
     *)
    let value ?(len=60) ?(lines=10) () =
      let str _ = String.random ~printable:true ~size:(thunk Random.int len) () in
      foldil (snocwith str) [] (1,Random.int lines) |> join ~sep:"\n"
    (* TODO TESTS *)

    (** [(keyset ?size ?key ())] is a random set of size [(size ())] (default: [< 10]) refer record keys.

        The keys are given by [(key ())] (default: {!key}).*)
    let keyset ?(size=fun () -> Random.int 9 + 1) ?key:keyfunc () = List.random ~size (Option.default key keyfunc) ()
    (* TODO TESTS *)

    (** [(record ?keys ())] generates a random refer record.

        The keys are given by [(keys ())] (default: {!keyset}) and all
        of them are used in the record.

        The values are given by [(values ())] (default: {!value}).
     *)
    let record ?keys ?(values=value) () =
      let keys = match keys with
        | None   -> List.random ~size:(fun () -> Random.int 9 + 1) key ()
        | Some f -> f ()
      in
      zip keys (List.random ~size:(k (len keys)) values ()) |> assemble ~fix:true
    (* TODO TESTS *)

    (** [(db ?size ?keys ?values ())] generates a random database of
        size [(size ())] (default: [< 100]) refer records.

        Each record is given by [(record ?keys ?values ())].

     *)
    let db ?size ?keys ?values =
      let keyset = match keys with None -> keyset () | Some f -> f () in
      List.random ?size (fun () -> record ?values ~keys:(fun () -> choose (Random.int (len keyset + 1) + 1) keyset |> shuffle) ())
    (* TODO TESTS *)
  end

  (** [random] is {!Random.record}. *)
  let random = Random.record

  (** {1 Lazy Sequences of Refer Records} *)

  (** Functions to generate lazy sequences of refer records. *)
  module Seq = struct (*$< Seq *)

    (** [(of_lines seq)] converts a sequence of strings (representing
        lines) to a sequence of Refer records.

        Common sources of string sequences include
        {!String.seq_of_lines} and {!lines_of_chan}.

        Each record is a [result]: [Ok] for valid records, [Error] for
        syntax errors.

        The [Ok] case is a pair [(int * t)] where the [int] is the
        line number of the first line of the record.

        The [Error] case is a pair [(int * t)] where the [int] is the
        line number of the syntax error.
     *)
    let of_lines s =
      let blank (_,x) = x = "" || String.(only ~these:whitespace x) in (* TODO HERE *)
      let rec skip seq = match seq () with (* skip empty lines *)
        | Seq.Nil         -> Seq.empty
        | Seq.Cons (x,xs) -> if blank x then skip xs else Seq.cons x xs
      in
      (* pop off the lines of the current record; lines returned in reverse order *)
      let rec reclines acc seq = match seq () with
        | Seq.Nil         -> acc, Seq.empty
        | Seq.Cons (x,xs) -> if blank x then acc, xs else reclines (x::acc) xs
      in
      (* parse record lines into line number paired with alist *)
      let fields (lines : (int * string) List1.t) =
        let each (alist, value) (ln,line) =
          if String.prefix "%" line
          then let k, v = String.(splitwhile (not << flip mem whitespace) line#.(1,0)) in
               let vs = map snd value in
               (k, String.(trimleft whitespace v) :: vs |> String.concat "\n") :: alist, []
          else alist, (ln,line)::value
        in
        match List1.foldl each ([], []) lines with (* ASSERT lines in reverse order *)
        | _,     (ln,v)::_ -> Error (ln, "continuation line at beginning of record: %S" % v)
        | alist, []        -> Ok List1.(rev lines |> hd |> fst, alist)
      in
      let rec loop s = match skip s |> reclines [] with
        | [],    _  -> Seq.empty
        | lines, s' -> Seq.cons (fields (List1.of_list lines)) (loop s')
      in
      Seq.index ~z:1 s |> loop
    (* TODO TESTS *)

    (** [(of_string str)] uses {!of_lines} to parse Refer records from the string [str]. *)
    let of_string = String.seq_of_lines >> of_lines
    (* TODO TESTS *)

  end (*$>*)

  (** {1 Input Sources of Refer Databases} *)

  (** [(of_channel chan)] creates a {!src} from the in_channel [chan]. *)
  let of_channel ?(readline=input_line) = Gen.catch readline

  (** [(of_string str)] creates a {!src} from the refer database
      represented by the string [str].*)
  let of_string = split ~elide:false ~sep:"\n" $ Gen.of_list

  (** {1 Parsing Refer Databases} *)

  (** [(fold f acc g)] folds over the Refer database on input source
      [g], calling [f] for each record.

      Example: count the number of records in a Refer database
      (ignoring syntax errors):

      - [(fold (witherr ignore (cons (fun _ -> succ))) 0))]

      Example: if [r] is ["%A 1\n%B 2\n%B 3\n%C 4"] then:
      {[(of_string r |> fold (witherr ignore (fun _ -> List.cons)) [])
      = [[("A", "1"); ("B", "2"); ("B", "3"); ("C", "4")]]]}
  *)
  let fold (f : 'a folder) acc (g : string Gen.t) =
    let module E = struct exception Err of string end in
    let blank = String.(dropwhile (contains whitespace) $ null) in
    let fields ln strs =
      let chop str = assert (str <> "" && str.[0] = '%');
        String.(splitwhile (not $. contains whitespace) (slice str (1,0)) |> Pair.cross id (trimleft whitespace)) in
      let each (f,fs) line =
        if String.prefix "%" line then [line], conswhen (not$.null) (rev f) fs else (line::f),fs in
      let each2 acc = function [] -> assert false | x::xs ->
        if String.prefix "%" x
        then let k,x = chop x in (k, String.concat "\n" (x::xs)) :: acc
        else raise (E.Err x)
      in
      foldl each ([],[]) strs |> fun (f,fs) ->
      (rev f) :: fs |> foldl each2 []
    in
    let rec skip ln = match g () with
    | None -> None | Some line -> if blank line then skip ~++ln else Some (ln,line) in
    let rec take (ln,xs) = match g () with
    | None -> ln,xs | Some x -> if blank x then ~++ln,xs else take (~++ln,x::xs) in
    let call f ln fs acc = match Result.trap id (fields ln) (rev fs) with
    | Ok alist        -> f ln (Ok alist) acc
    | Error (E.Err x) -> f ln (Error x)  acc
    | Error exn       -> raise_notrace exn
    in
    let rec loop (ln,acc) = match skip ln with
    | None           -> ln,acc
    | Some (ln,line) -> let ln',fs = take (ln,[line]) in loop (~++ln',(call f ~++ln fs acc))
    in
    loop (0,acc) |> snd

  (** [(iter f g)] iterates [f] across the Refer records on input source [g].

      Example: print the Refer database on [g] to [stdout]:
      - [(iter (fun _ln -> Result.get_ok >> assemble >> print_endline) g)]
  *)
  let iter f = fold (fun ln r () -> f ln r) ()

  (** {1 Fold Helpers} *)

  (** [(cons f)] makes an ['a folder] out of the {!List.cons}-like function [f].

      This makes it easy to ignore the line-number parameter.

      Example: this is the list of results from the [src] [g]:
      - [(fold (cons List.cons) [] g)]
  *)
  let cons   f : 'a folder = fun _  r acc -> f r acc

  (** [(snoc f)] makes an ['a folder] out of the {!Lists.snoc}-like function [f]. *)
  let snoc   f : 'a folder = fun _  r acc -> f acc r

  (** [(witherr e f)] makes an ['a folder] out of [e] (a {!errhandler}) and [f].

      {i N.B.} [f] is not a [('a folder)] because it takes a [t]
      rather than a [((t, string) Stdlib.result)]; this is the
      whole point of [witherr], but it's easy to get confused.

      The resulting [(folder ln r acc)] applies [f] to each [t] when
      [(r = (Ok t))] and applies [e] to each [x] when [(r = (Error x))].

      Example: this is the [(t list)] from the [src] [g], ignoring any [Error] results:
      - [(fold (witherr ignore (fun _ -> List.cons)) [] g)]

      Example: this is the [((int * string) list)] of all errors from
      the [src] [g], ignoring any [Ok] results:
      - [(fold (witherr (curry List.cons) ignore) [] g)]
  *)
  let witherr (e : 'a errhandler) f : 'a folder = fun ln r acc ->
    match r with Ok alist -> f ln alist acc | Error x -> e ln x acc

  (** {1 Predefined Error Handlers}
      For {!witherr}. *)

  (** [(syntax)] is an error-handler for {!witherr} that raises [Syntax]. *)
  let syntax ln x _ = raise (Syntax (ln,x))

  (** [(ignore)] is an error-handler for {!witherr} that silently ignores any syntax errors. *)
  let ignore _ _ acc = acc

  (** [(print ?msg fn)] is an error-handler for {!witherr} that prints an
      error message to [stderr] for each syntax error.

      - [fn] is the filename
      - [~msg] is the error message printed in front of the erroneous line (default: ["syntax error"])

      The error message is in the standard format understood by Unix
      compiler utilities and editors like Emacs, Vi, Vim, and the like.

      Example:
      {[let fn = "/etc/passwd" in
      within Refer.(fold (witherr (print fn) ignore) [] << of_channel) fn]}

      might print: [/etc/passwd:1:syntax error: root:x:0:0:root:/root:/bin/bash]
  *)
  let print ?(msg="syntax error") fn ln x acc = eprintf "%s:%d:%s: %s\n" fn ln msg x; acc

end

(** {1:macros Macros} *)

(* TODO 1. line no col offset *)
(* TODO 2. conversion to Result monad? *)
(** Simple macro expansion in strings.

    A {i macro} is a pattern in a string that is mapped to a named
    OCaml function that takes a variable number of parameters and
    returns some value as its expansion.

    A {i macro call} in a string looks like (in the default syntax)
    ["{name}"]; this example is nullary (i.e. it takes no parameters).
    Parameters are separated from the name, and from each other by
    ['|']; here is a macro being called with three parameters:
    ["{name|p1|p2|p3}"].

    Macro calls nest, so a parameter can be formed from the expansion
    of some other macro: ["A nested call: {foo|My parameter is {bar}.}"]

    Typcally, macros return string values which simply replace their
    {i calls} in the string, e.g.:

    {[ "I'm not {upcase|yelling}!" ]}

    might become:

    {[ "I'm not YELLING!" ]}

    if the implementation of [upcase] does the obvious thing.

    That being said, macros can actually expand to any kind of value,
    so instead of the simple:

    {[ "text {em|contained} in a paragraph" ]}

    expanding to the string:

    {[ "text <em>contained</em> in a paragraph" ]}

    the [em] macro might return a tree structure representing the HTML, as for
    Caml on the Web:

    {[[`Data "text"; `El ((("", "em"), []), [`Data "contained"]); `Data "in a paragraph"]]}

    A macro also takes an arbitrary data parameter that can be [()] in
    the simplest case, or something like an opened database handle or
    some complex data structure; this value is threaded though all the
    macro calls and so acts like the accumulator of a fold.

    A trivial example of a macro is this one that simply acts as a
    shorthand for a longer string:
    {[Macro.(define "inria" (fun d _ _ -> d, "Institut national de recherche en informatique et en automatique"))]}

    Note how the macro needs to return the data parameter [d].

    This can written more compactly, via the {!Macro.k} combinator, as:
    {[ Macro.(define "inria" (k "Institut national de recherche en
    informatique et en automatique")) ]}

    Here is a macro that does significant work -- file inclusion:
    {[ (fun d _ -> function [] -> d,"" | fn::_ -> d,readfile fn) ]}

    Quoting is done with macros that expand to metacharacters; see
    {!Macro.left}, {!Macro.sep}, and {!Macro.right} for quoters that support
    the default syntax..

    See the {!macrotutorial}.
    @see <http://mirage.github.io/ocaml-cow/> Caml on the Web
*)
(* TODO TESTS *)
module Macro = struct

  (** {1 Exceptions} *)

  (** Syntax error: unterminated macro call, i.e. missing {!right}. *)
  exception Syntax

  (** Bad macro name error: currently, a macro name can only be a literal string.

      I.e., computed macro names are not (currently) allowed.
  *)
  exception BadName of string

  (** Undefined macro: name not in environment. *)
  exception Undefined of string

  (** {1 Types} *)

  type ('a,'b) macro = 'a -> string -> 'b list -> ('a,'b) result
  (** A macro is a function [(fun data name parms -> ...)]; [data] is
      the [data] parameter passed to {!eval} or {!to_string}; [name]
      is the name of the macro (the first argument of {!define}), and
      [parms] is a list of the macro's parameters; the macro returns a
      {!result}. *)

  and ('a,'b) result = 'a * 'b
  (** The type of macro results, a pair of:
      - the data value (sometimes but not always modified)
      - the value which is the result of its expansion. *)

  and ('a,'b) env = (string * ('a,'b) macro) list
  (** The type of environments: your macro definitions are passed to
      {!eval} or {!to_string} as an alist mapping macro names to their
      functional values. *)

  (** The type of parsed macro expression nodes (as returned by {!parse}). *)
  type node = S of string | M of (string * node list list)

  (** {1 Defining Macros in Environments} *)

  (** [empty] is the empty environment. *)
  let empty : ('a,'b) env = []

  (** [(define name macro env)] binds [name] to [macro] in environment
      [env] and returns the new environment.

      N.B. the new binding goes at the front of the old environment
      and hence overrides any older macro of the same name.

      Example: if [env] is the environment:

      {[Macro.(define "a" (k "1") empty |> define "b" (k "2") |> define "a" (k "3"))]}

      then:

      {[(Stream.of_string "{a}" |> Macro.to_string env ()) = "3"]}
  *)
  let define name (m : ('a,'b) macro) (env : ('a,'b) env) : ('a,'b) env = (name,m) :: env

  (** {1 Evaluating Macro Functions (Implementations)} *)

  (** [(eval ?default env data name parms)] calls the {!macro} bound to
      [name] in the environment [env] with parameters [parms], exactly
      as it would be called if found in the string passed to
      {!to_string}.

      If [name] is not found in [env], [default] is called (with
      [name] as its name).  If [default] is not provided, {!Undefined}
      is raised.

      Example:
      - [(eval (define "a" (k "1") empty) () "a" []) = ((), "1")]

      @raise Undefined if there's no definition for the named macro in [env] and no [default]
  *)
  let eval : ?default:(('a,'b) macro) -> ('a,'b) env -> 'a -> string -> 'b list -> ('a,'b) result =
    fun ?default env data name parms -> match assoc name env with
    | f                   -> f data name parms
    | exception Not_found -> match default with
      | None   -> raise (Undefined name)
      | Some d -> d data name parms

  (** {1 Syntax}

      Within a string, a macro call is a parenthesized expresssion
      (that is, macro calls nest); the opening and closing
      "parentheses" can be any pair of characters; the defaults are
      ['{'] and ['}'].

      The string of characters beginning after the left paren is the
      {i name} of the macro; the name is terminated either by the
      matching right paren, or else by the first {i parameter
      separator character}, the default being ['|'].

      Parameters are separated from the name and from each other by
      this separator.

      Here are some calls:
      - ["{}"] -- calls the macro whose name is the empty string with no parameters
      - ["{foo}"] -- calls the macro named ["foo"] with no parameters
      - ["{foo|bar}"] -- calls the macro named ["foo"] with the parameter list [["bar"]]
      - ["{foo|bar|zap}"] -- calls the macro named ["foo"] with the parameter list [["bar";"zap"]]
      - ["{foo||zap}"] -- calls the macro named ["foo"] with the parameter list [["";"zap"]]
      - ["{foo|{bar}}"] -- calls the macro named ["foo"] with the
      parameter list whose [List.hd] is the value of the macro call ["{bar}"] *)

  (** The default left parenthesis character (['{']). *)
  let left = '{'

  (** The default parameter separator character (['|']). *)
  let sep = '|'

  (** The default right parenthesis character (['}']). *)
  let right = '}'

  (** {1 Parsing Strings that Contain Macro Calls} *)

  (**/**)
  let son = ref None
  (**/**)

  (** [(parse ?left ?sep ?right stream)] parses the string on [stream]
      and returns a list of {!node}'s.

      Example:
      {[ (Stream.of_string "Foo {bar|1|2}" |> parse) = [S "Foo "; M ("bar", [[S "1"]; [S "2"]])] ]}

      @raise Syntax upon a syntax error in a macro call
      @raise BadName upon an invalid macro name in a call
  *)
  let parse ?(left=left) ?(sep=sep) ?(right=right) st =
    let next,peek = Stream.(next,peek) in
    let create,addc,contents = Buffer.(create,add_char,contents) in
    let toss c = assert (next st = c) in
    let rec nodes acc = match peek st with
      | Some c when c = left -> toss left; nodes (call [] [] :: acc)
      | Some c               -> nodes (outstring (create 64) :: acc)
      | None                 -> rev acc
    and instring buf = match peek st with
      | Some c when c = left  -> S (contents buf)
      | Some c when c = sep   -> S (contents buf)
      | Some c when c = right -> S (contents buf)
      | Some c                -> next st |> addc buf; instring buf
      | None                  -> S (contents buf)
    and outstring buf = match peek st with
      | Some c when c = left  -> S (contents buf)
      | Some c                -> next st |> addc buf; outstring buf
      | None                  -> S (contents buf)
    and call ps ns = match peek st with
      | Some c when c = left  -> toss left;  call ps (call [] [] :: ns)
      | Some c when c = sep   -> toss sep;   call (ns :: ps) []
      | Some c when c = right -> toss right; macro ps ns
      | Some c                -> call ps (instring (create 64) :: ns)
      | None                  -> raise Syntax
    and macro ps ns = match ps, ns with
      | [], []      -> M ("", [])
      | ps, ns      -> match rev (rev ns :: ps) with
        | []        -> assert false
        | []::ps    -> M ("",ps)
        | [S n]::ps -> M (n,ps)
        | ns::ps    -> raise (BadName (Option.get !son ns)) (* TODO ultimately, allow computed macro names! *)
    in
    nodes []

  (* Example:

     Stream.of_string ("Hey, {name}!  Hel{define|name|Buddy}lo.  " ^ Test.text)
     |> parse |> defines "define" Test.env |> fun (env,ns) -> string_of_nodes env () ns
  *)
  (** [(defines ?preserve definer env nodes)] returns the node list
      [nodes] after adding in-line macro definitions in [nodes] to
      [env].  This allows the string that's being expanded to define
      its own macro definitions.

      [definer] is the name of the inline macro-defining macro.

      The inline definitions do {i not} have to precede their uses.

      Any [M]-nodes representing calls to [definer] are removed from
      the node list, unless [(preserve = true)].

      Example: given the input string:
      {[ let str = "Hey, {name}!  Hel{define|name|Buddy}lo." ]}

      we would have:
      {[ (Stream.of_string str
 |> parse
 |> defines "define" []
 |> fun (env,ns) -> string_of_nodes env () ns)
 = "Hey, Buddy!  Hello." ]}
  *)
  let defines ?(preserve=false) definer env nodes =
    let s (env,acc) str = env, S str :: acc in
    let m (env,acc) n ps =
      if n <> definer
      then env, M (n,ps) :: acc
      else match ps with
      | [S n']::[S exp]::[] -> define n' (fun d _ _ -> d,exp) env, conswhen (k preserve) (M (n,ps)) acc
      | _ -> failwith "defines"
    in
    let each acc = function
      | S str     -> s acc str
      | M (n, ps) -> m acc n ps
    in
    foldl each (env,[]) nodes |> (id &&& rev)

  (** {1 Expanding Macros} *)

  (** [(fold ?default s m env (data,z) nodes)] folds [s] and [m] over a list of
      {!node}'s, with [data] as initial macro data parameter and [z]
      as the initial accumulator for the fold.

      [s] is applied to the fold accumulator and the string value of
      each [S]-node.

      [m] is applied to the fold accumulator and the result of the
      evaluation of each [M]-node; the result of the evaluation
      includes the possibly updated data parameter.

      The macro definitions are in the environment [env].

      [data] is passed on to each macro call, along with the name the
      macro was called with, and its list of parameters.

      If provided, [~default] is a default macro that is called
      whenever a macro call with an unknown name is encountered.
      Otherwise, in such a case, {!Undefined} is raised with the name
      of the offending call.
  *)
  let rec fold ?default s m env (data,z) nodes =
    let call (data,acc) n ps =
      let each (data,acc) ns = fold ?default s m env (data,acc) ns in
      let data,parms = foldl each (data,[]) ps in
      rev parms |> eval ?default env data n |> m acc
    in
    let each (data,acc) = function
      | S str     -> data, s acc str
      | M (n, ps) -> call (data,acc) n ps
    in
    foldl each (data,z) nodes

  (* TEST
     Stream.of_string "AAA{a|b{c|d{e}f}g}BBB{a|b{c|d{e}f}g}CCC" |> parse |> fold ~default:Pre.call snoc (fun a (d,s) -> d, s::a) [] (0, []) |> (id&&&(rev$String.concat ""))
  *)

  (** [(string_of_nodes ?default env data nodes)] expands the macro
      calls in [nodes] and returns macro data and a string
      representation of the expansion.

      It's equivalent to:

      {[ fold snoc (fun a (d,s) -> d, s::a) env (data, []) ns |> fun (d,l) -> d, rev l |> String.concat "" ]}

      but more efficient.
  *)
  let rec string_of_nodes ?default env data nodes =
    let create,add,get = Buffer.(create,add_string,contents) in
    let s (data,b) s = add b s; data,b in
    let m (data,buf) n ps =
      let parms = foldl (fun acc ns -> string_of_nodes ?default env data ns :: acc) [] ps |> rev in
      let data,r = eval ?default env data n parms in
      add buf r;
      data,buf
    in
    let each acc = function
      | S str     -> s acc str
      | M (n, ps) -> m acc n ps
    in
    foldl each (data,create 64) nodes |> snd |> get

  (**/**)
  let _ =                       (* TODO replace this hack with mutual recursion! *)
    let default d n = function
      | [] -> d, sprintf "%c%s%c" left n right
      | ps -> d, sprintf "%c%s%c%s%c" left n sep (join ~sep:(String.make 1 sep) ps) right
    in
    let f = string_of_nodes ~default [] () in
    son := Some f
  (**/**)

  (** [(to_string ?define ?left ?sep ?right ?default env data stream)] is
      [(parse ~left ~sep ~right stream |> string_of_nodes ?default env data)]

      @raise Syntax upon a syntax error in a macro call
      @raise BadName upon an invalid macro name in a call
  *)
  let to_string ?define ?(left=left) ?(sep=sep) ?(right=right) ?default env data st =
    parse ~left ~sep ~right st |> string_of_nodes ?default env data

  (**/**)
  module Test = struct          (* TODO DELETE *)
    let env = [
      "t", (fun d _ -> function
      | x::y::[] -> d, ("<a href='%s'>%s</a>" % ("link:%s" % String.replace " " "_" x)) y
      | xs -> failwith (join xs |> sprintf "t: %d: %s" (len xs)));
      "sc", (fun d _ -> function
      | x::[] -> d, String.uppercase_ascii x
      | _ -> failwith "sc");
    ]
    let text = "Have you read {t|lord of the rings|{sc|lotr}}?"
    let st () = Stream.of_string text
  end
  (**/**)

  (** {1 Macro Combinators}

      These may make it easier to write macros. *)

  (** [(k x)] is the constant macro that always expands to [x]. *)
  let k x : ('a,'b) macro = fun d _ _ -> d,x

  (** [(skip1 f)] converts function [f] into a {!macro} by not passing
      on the [data] parameter.

      [f] skips the 1st parameter of the function.
  *)
  let skip1  f : ('a,'b) macro = fun d n ps -> d, f n ps

  (** [(skip2 f)] converts function [f] into a {!macro} by not passing
      on the [name] parameter.

      [f] skips the 2nd parameter of the macro.
  *)
  let skip2  f : ('a,'b) macro = fun d _ ps -> f d ps

  (** [(skip12 f)] converts function [f] into a {!macro} by passing
      on neither the [data] nor [name].

      [f] skips the 1st and 2nd parameters of the function.
  *)
  let skip12 f : ('a,'b) macro = fun d n ps -> d, f ps

  (** [(syntax ?def n f)] converts function [f] into a {!macro} that
      raises an error if it is not called with exactly [n] parameters,
      unless [~def] is given, in which case [def] is returned as the
      value.

      Note that the function [f] receives its parameters as an array
      of guaranteed length [n], for easier access.
  *)
  let syntax ?def nps f : ('a,'b) macro = fun d n ps ->
    if len ps = nps
    then Array.of_list ps |> f d n
    else match def with
    | Some def' -> d, def'
    | None     -> failwith (sprintf "macro %s takes %d parameters, got %d" n nps (len ps))

  (** {1 Predefined Macros} *)

  (** There is no default {!env}, but you can add any of these to your {!env}. *)
  module Pre = struct

    (** [ignore] is a macro that ignores all its parameters and returns the empty string.

        This is good for comments within a string: ["{ignore|This is a comment.}This is not."]
    *)
    let ignore : ('a,'b) macro = fun d _ _ -> d, ""

    (** [name] is a macro that returns its own name. *)
    let name : ('a,'b) macro = fun d n _ -> d, n

    (** [parms] is a macro that returns its parameter list as a
        string, with the parameters separated by a spaces. *)
    let parms : ('a,'b) macro = fun d _ ps -> d, String.concat " " ps

    (** [(self ?left ?sep ?right)] is a macro that returns a string
        representation of itself (with parameters expanded). *)
    let self ?(left=left) ?(sep=sep) ?(right=right) : ('a,'b) macro = fun d n -> function
      | [] -> d, sprintf "%c%s%c" left n right
      | ps -> d, sprintf "%c%s%c%s%c" left n sep (join ~sep:(String.make 1 sep) ps) right

    (**/**)
    let lc = String.make 1 left
    let sc = String.make 1 sep
    let rc = String.make 1 right
    (**/**)

    (** {1 Quoting Metacharacters in the Default Syntax}

        N.B. These macros only work for the default syntax; if you
        change the syntax, you might need to provide your own quoters.
    *)

    (** [left] is a macro that expands to {!left}.

        This means ["{left}"] is a way to quote the left parenthesis.
    *)
    let left  : ('a,'b) macro = fun d _ _ -> d, lc

    (** [sep] is a macro that expands to {!sep}.

        This means ["{sep}"] is a way to quote the parameter separator.
    *)
    let sep   : ('a,'b) macro = fun d _ _ -> d, sc

    (** [right] is a macro that expands to {!right}.

        This means ["{right}"] is a way to quote the right parenthesis.
    *)
    let right : ('a,'b) macro = fun d _ _ -> d, rc

    (** {1 String Manipulation} *)

    (** [trim] is a macro that trims leading and trailing characters from its first parameter.

        The default trimmed characters are {!Strings.whitespace}.  If
        you pass a second parameter, it is a string of the characters to be trimmed.

        This macro just calls {!Strings.trim} appropriately.
    *)
    let trim  : ('a,'b) macro = fun d _ -> function
    | []       -> d, ""
    | p::[]    -> d, Strings.(trim whitespace p)
    | p::cs::_ -> d, Strings.(trim cs p)
  end

  (**/**)
  module Example = struct
    let str =  "{define|a|Abel}{define|b|Baker}{a} says 'Hello, {b}!'."
  end
  (**/**)

  (* TODO DOC *)
  (** {1:macrotutorial Tutorial}

      Coming soon.
  *)
end

(** {1:isn International Standard Numbers} *)

(** {1 International Standard Numbers}

    Functions for manipulating ISBNs and ISSNs.
*)
module ISN = struct (*$< ISN *)


  (** [(norm str)] normalizes an ISN, by removing all insignificant
        punctuation (spaces, hyphens, etc), leaving only the significant characters
        (digits, ['x'], and ['X']). *)
  let norm str =
    let digx = String.digits ^ "X" ^ "x" in
    let each b =
      let eachc c = if String.contains digx c then Buffer.add_char b c in
      String.iter eachc str in
    withbuf 13 each
  (*$= norm
      (norm String.majuscules) "X"
      (norm String.miniscules) "x"
   *)
  (*$Q norm
      Q.numeral_string (fun s -> norm s = s)
   *)

  (**/**)
  let split str = if str = "" then invalid_arg "split" else  String.(slice str (0,-1), pyget str (-1))
  (*$T split
      not @@ succeeds split ""
   *)
  (*$Q split
      Q.string (fun s -> s = "" || let n = String.len s in split s = (String.sub s 0 (n-1), s.[n-1]))
   *)

  let strip str = split str |> fst
  (*$T strip
      not @@ succeeds strip ""
   *)
  (*$Q strip
      Q.string (fun s -> s = "" || strip s = String.(sub s 0 (len s - 1)))
   *)
  (**/**)

  (** International Standard Book Numbers (ISBNs) *)
  module ISBN = struct (*$< ISBN *)

    (** The type of ISBN flavors. *)
    type flavor =
      | SBN                     (** 9-digit Standard Book Number *)
      | ISBN10                  (** 10-digit ISBN *)
      | ISBN13                  (** 13-digit ISBN *)

    (** [string_of_flavor] converts a [flavor] value to a string. *)
    let string_of_flavor = function SBN -> "SBN" | ISBN10 -> "ISBN-10" | ISBN13 -> "ISBN-13"
    (*$T string_of_flavor
      map string_of_flavor [SBN;ISBN10;ISBN13] |> len = 3
    *)

    (** The type of parsed ISBNs. *)
    type t = {
      flavor : flavor;          (** variant type *)
      bn : string;              (** book number *)
      cd : char;                (** check digit *)
    }
    (* QCheck generator of t's *)
    (*$inject
      let gent =
        let open QCheck in
        let gen = let open Gen in oneofl ISN.ISBN.[SBN;ISBN10;ISBN13]
          >>= fun f ->
            pair (return f) (string_size ~gen:numeral (return (assoc f ISN.ISBN.[SBN,8;ISBN10,9;ISBN13,12])))
            |> map (fun (f,bn) -> ISN.ISBN.set { ISN.ISBN.flavor=f; bn; cd='Z'})
        in
        make ~print:dump gen
    *)

    (** [(flavor t)] is the flavor of the parsed ISBN [t]. *)
    let flavor t = t.flavor
    (*$Q flavor
      gent (fun t -> flavor t = t.flavor)
    *)

    (** [(bn t)] is the book number (exclusive of check digit) of the parsed ISBN [t]. *)
    let bn t = t.bn
    (*$Q bn
      gent (fun t -> bn t = t.bn)
    *)

    (** [(cd t)] is the check digit of the parsed ISBN [t]. *)
    let cd t = t.cd
    (*$Q cd
      gent (fun t -> cd t = t.cd)
    *)

    (** [to_string] converts a parsed ISBN to a string representation. *)
    let to_string = function { bn; cd } -> sprintf "%s%c" bn cd
    (*$Q to_string
      gent (fun t -> to_string t |> String.len > 1)
    *)

    (** [dump] converts a parsed ISBN to a string suitable for debugging and testing. *)
    let dump = function { flavor; bn; cd } -> sprintf "<%s %s %c>" (string_of_flavor flavor) bn cd
    (*$Q dump
      gent (fun t -> dump t |> String.len > 1)
    *)

    (** [norm] is {!Prelude.ISN.norm}. *)
    let norm = norm
    (*$Q norm;to_string;parse
      gent (fun t -> to_string t |> norm |> parse = t)
     *)

    (*$inject
      let gen_isbn = QCheck.(make ~print:Print.string Gen.(string_size ~gen:numeral (oneofl [9;10;13])))
      let bad_contents = QCheck.(make ~print:Print.string Gen.(string_size ~gen:(oneofa String.(to_array alphabet)) (oneofl [9;10;13])))
      let bad_len  = QCheck.(make ~print:Print.string Gen.(string_size ~gen:numeral (oneofl List.(diff (0--20) [9;10;13]))))
    *)

    (** [(parse str)] parses an ISBN.

        The ISBN must have a check digit, but its validity is not checked; see {!valid}.

        See {!make} to make a [t] from a book number (an ISBN without a check digit).

        The ISBN will first be normalized.

        @raise Invalid_argument if the normalized [bn] is not of length 8, 9, or 12.
     *)
    let parse str =
      if str = ""
      then invalid_arg "parse"
      else let n = norm str in
        let len = String.length n in
        let bn,cd = split n in
        if len = 9
        then { flavor=SBN; bn; cd }
        else if len = 10
        then { flavor=ISBN10; bn; cd }
        else if len = 13
        then { flavor=ISBN13; bn; cd }
        else invalid_arg "parse"
    (*$Q parse
      gen_isbn (fun s -> succeeds parse s)
      bad_len  (fun s -> not @@ succeeds parse s)
      bad_contents (fun s -> not @@ succeeds parse s)
    *)

    (** [(check910 bn)] computes the check digit from the book number
        part of a 9- or 10-digit ISBN.

        The "book number" is the ISBN with the check digit removed.
        You can acquire a book number from an ISBN via [(parse >> bn)]. *)
    let check910 bn =
      let each (i,n) c =
        pred i, (Char.Decimal.int c) * i + n
      in
      let sum = String.foldl each (10,0) bn |> snd in
      let r = (11 - sum mod 11) mod 11 in
      if r = 10 then 'X' else Char.Decimal.digit r
    (* test: see compute below *)

    (** [(check13 bn)] computes the check digit from the book number
        part of a 13-digit ISBN.

        The "book number" is the ISBN with the check digit removed.
        You can acquire a book number from an ISBN via [(parse >> bn)]. *)
    let check13 bn =
      let i13 n = if even n then 1 else 3 in
      let each (i,n) c =
        succ i, (Char.Decimal.int c) * i13 i + n
      in
      let sum = String.foldl each (0,0) bn |> snd in
      let r = 10 - sum mod 10 in
      if r = 10 then '0' else Char.Decimal.digit r
    (* test: see compute below *)

    (** [(compute t)] computes the check digit from a parsed ISBN.

        Invariant: ∀t . [(valid t) && (compute t = cd t)]. *)
    let compute t = match t.flavor with
    | SBN | ISBN10 -> check910 t.bn
    | ISBN13       -> check13  t.bn
    (* test: see valid below *)

    (*$inject
       let invalidate t =
         compute t |> fun d ->
         { t with cd = diff ('X'::Char.('0'--'9')) [d] |> choose 1 |> hd}
    *)

    (** [(valid t)] returns [true] if the parsed ISBN has a valid check digit and [false] otherwise. *)
    let valid t = compute t = t.cd
    (*$Q valid
      gent (fun t -> not @@ valid @@ invalidate t)
    *)
    (*$Q valid;compute
      gent (fun t -> valid t && compute t = cd t)
    *)

    (** [(set t)] computes and sets the check digit for the book number in [t].

        [(set t)] works whether or not [(valid t)].

        Invariant: ∀t . [(valid t) && (t = (set t))] *)
    let set t = { t with cd = compute t }
    (*$Q set; valid
      gent (fun t -> valid t && t = set t)
      gent (fun t -> valid t && invalidate t |> set |> valid)
    *)

    (** [(make bn)] makes a parsed ISBN from book number [bn].

        A book number has no check digit.  See {!parse} to parse an
        ISBN with a check digit.

        The book number will first be normalized.

        @raise Invalid_argument if the normalized [bn] is not of length 8, 9, or 12.
    *)
    let make bn =
      if bn = ""
      then invalid_arg "make"
      else let n = norm bn in
        let len = String.len n in
        if len = 9-1
        then set { flavor=SBN; bn; cd = 'Z' }
        else if len = 10-1
        then set { flavor=ISBN10; bn; cd = 'Z' }
        else if len = 13-1
        then set { flavor=ISBN13; bn; cd = 'Z' }
        else invalid_arg "make"
    (*$T make
      not @@ succeeds make ""
    *)
    (*$Q make
      gen_isbn (fun s -> succeeds make (String.slice s (0,-1)))
      Q.(string_gen_of_size Gen.(return 6) Gen.numeral) (fun s -> not @@ succeeds make s)
    *)

    (** [(to13 t)] converts any flavor of ISBN to a 13-digit [ISBN13].

        This potentially changes the flavor and recomputes the check digit, as appropriate.

        Invariants:
        - ∀t . [(flavor t) = ISBN13 && (t = to13 t)]
        - ∀t . [(valid (to13 t))]
        - ∀t . [(flavor (to13 t) = ISBN13)]
    *)
    let to13 t = match t.flavor with
    | SBN    -> set { t with flavor = ISBN13; bn = "0978"^t.bn }
    | ISBN10 -> set { t with flavor = ISBN13; bn =  "978"^t.bn }
    | ISBN13 -> t
    (*$Q to13
      gent (fun t -> flavor t <> ISBN13 || (t = to13 t))
      gent (fun t -> valid (to13 t))
      gent (fun t -> flavor (to13 t) = ISBN13)
    *)

    (**/**)
    module Test = struct (*$< Test *)
      let data10 = [
        "99921-58-10-7";
        "9971-5-0210-0";
        "960-425-059-0";
        "80-902734-1-6";
        "85-359-0277-5";
        "1-84356-028-3";
        "0-684-84328-5";
        "0-8044-2957-X";
        "0-85131-041-9";
        "0-943396-04-2";
        "0-9752298-0-X";
      ]
      let data13 = [
        "978-3-16-148410-0";
      ]
      let test = parse >> valid
      (*$T test
        data10 @ data13 |> all test
      *)
    end (*$>*)
    (**/**)
  end (*$>*)

  (** International Standard Serial Numbers (ISSNs) *)
  module ISSN = struct (*$< ISSN *)

    (** The type of parsed ISSNs. *)
    type t = {
      sn : string;              (** serial number *)
      cd : char;                (** check digit *)
    }
    (* QCheck generator of t's *)
    (*$inject
      let issn =
        let open QCheck in
        make ~print:Print.string Gen.(pair (string_size ~gen:numeral (return 4)) (string_size ~gen:numeral (return 4)) |> map (fun (a,b) -> a^"-"^b))
      let gent =
        let open QCheck in
        make ~print:to_string Gen.(pair (string_size ~gen:numeral (return 7)) (oneof [return 'X'; numeral]) |> map (fun (sn,cd) -> set { sn; cd}))

      let badlen =
        let open QCheck in
        make ~print:Print.string Gen.(string_size ~gen:numeral (oneof [int_range 0 7; int_range 9 12]))
      let bad_contents =
        let open QCheck in
        make ~print:id Gen.(string_size ~gen:(oneofl (String.(explode alphabet))) (return 8))
    *)

    (** [(sn t)] is the serial number (exclusive of check digit) of the parsed ISSN [t]. *)
    let sn t = t.sn
    (*$Q sn
      gent (fun t -> sn t = t.sn)
    *)

    (** [(cd t)] is the check digit of the parsed ISSN [t]. *)
    let cd t = t.cd
    (*$Q cd
      gent (fun t -> cd t = t.cd)
    *)

    (** [to_string] converts a parsed ISSN to a string representation. *)
    let to_string = function { sn; cd } -> String.(sprintf "%s%s%c" (slice sn (0,4)) (slice sn (4,0)) cd)
    (*$Q to_string
      gent (fun t -> String.slice (to_string t) (0,7) = t.sn && String.pyget (to_string t) 7 = t.cd)
      gent (fun t -> to_string t |> String.len = 8)
    *)

    (** [pretty] converts a parsed ISSN to a pretty string representation (includes hyphen). *)
    let pretty = function { sn; cd } -> String.(sprintf "%s-%s%c" (slice sn (0,4)) (slice sn (4,0)) cd)
    (*$Q pretty
      gent (fun t -> pretty t |> String.len = 9)
    *)

    (** [norm] is {!Prelude.ISN.norm}. *)
    let norm = norm

    (** [(parse str)] parses an ISSN.

        The ISSN must have a check digit, but its validity is not checked; see {!valid}.

        See {!make} to make a [t] from a serial number (an ISSN without a check digit).

        The ISSN will first be normalized.

        @raise Invalid_argument if the normalized [bn] is not of length 8.
     *)
    let parse str =
      if str = ""
      then invalid_arg "parse"
      else let n = norm str in
        let len = String.length n in
        let sn,cd = split n in
        if len = 8
        then { sn; cd }
        else invalid_arg "parse"
    (*$Q parse
      issn (fun s -> succeeds parse s)
      badlen  (fun s -> not @@ succeeds parse s)
      bad_contents (fun s -> not @@ succeeds parse s)
    *)

    (** [(compute t)] computes the check digit from a parsed ISSN.

        Invariant: ∀t . [(valid t) && (compute t = cd t)]. *)
    let compute t =
      let each (i,n) c =
        pred i, (Char.Decimal.int c) * i + n
      in
      let sum = String.foldl each (8,0) t.sn |> snd in
      let r = (11 - sum mod 11) mod 11 in
      if r = 10 then 'X' else Char.Decimal.digit r
      (* test: see valid below *)

    (*$inject
       let invalidate t =
         compute t |> fun d ->
         { t with cd = diff ('X'::Char.('0'--'9')) [d] |> choose 1 |> hd}
    *)

    (** [(valid t)] returns [true] if the parsed ISSN has a valid check digit and [false] otherwise. *)
    let valid t = compute t = t.cd
    (*$Q valid
      gent (fun t -> not @@ valid @@ invalidate t)
    *)
    (*$Q valid;compute
      gent (fun t -> valid t && compute t = cd t)
    *)

    (** [(set t)] computes and sets the check digit for the serial number in [t].

        [(set t)] works whether or not [(valid t)].

        Invariant: ∀t . [(valid t) && (t = (set t))] *)
    let set t = { t with cd = compute t }
    (*$Q set; valid
      gent (fun t -> valid t && t = set t)
      gent (fun t -> valid t && invalidate t |> set |> valid)
    *)

    (** [(make sn)] makes a parsed ISSN from serial number [sn].

        A serial number has no check digit.  See {!parse} to parse an
        ISSN with a check digit.

        The serial number will first be normalized.

        @raise Invalid_argument if the normalized [sn] is not of length 8.
    *)
    let make sn =
      if sn = ""
      then invalid_arg "make"
      else let n = norm sn in
        if String.len n = 8-1
        then set { sn; cd = 'Z' }
        else invalid_arg "make"
    (*$T make
      not @@ succeeds make ""
    *)
    (*$Q make
      issn (fun s -> succeeds make (String.slice (norm s) (0,-1)))
      Q.(string_gen_of_size Gen.(return 6) Gen.numeral) (fun s -> not @@ succeeds make s)
    *)

    (**/**)
    module Test = struct (*$< Test *)
      let data = [
          "0378-5955";
          "2049-3630";
          "0317-8471";
          "2434-561X";]
    end (*$>*)
    (**/**)

  end (*$>*)
end (*$>*)

(** {1:unix Unix} *)

(** Additional [Unix] functions. *)
(* TODO TESTS *)
module Unix = struct (*$< Unix *)
  (**/**)
  let pread = read
  let pwrite = write
  (**/**)
  (** @closed *)
  include Unix

  (** [(to_string exn)] produces a "better" (IMHO) representation of a
      [Unix_error] exception for end users.

      All other exceptions are re-raised (with no trace). *)
  let to_string = Pre.Unix.to_string

  (** [(year ())] is the current year as an integer. *)
  let year () = (time () |> localtime).tm_year + 1900

  (** [(restart_on_EINTR f x)] is [(f x)] but if evaluation is
        interrupted by [(Unix_error (EINTR, _, _))], [(f x)] will be
        evaluated again.

        This is necessary for system calls such as [Unix.waitpid],
        [Unix.select], etc, exactly as it's needed in C programming.  *)
  let rec restart_on_EINTR f x =
    try f x with Unix_error (EINTR, _, _) -> restart_on_EINTR f x

  (** Timer functions and similar *)
  module Timer = struct
    (**/**)
    exception Timeout
    let notimer = { it_interval = 0.0; it_value = 0.0 }
    let handler = Sys.(Signal_handle (fun _ ->
                           set_signal sigalrm Signal_default;
                           ignore (setitimer ITIMER_REAL notimer);
                           raise Timeout))
    (**/**)

    (** [(elapsed f x)] returns [(f x, t)], where [t] is the elapsed
          wall-clock time in seconds that it took to evaluate [(f x)]. *)
    let elapsed f x = let t1 = time () in let r = f x in r, time () -. t1

    (** [(timeout t f x)] evaluates [(f x)] but interrupts the
        evaluation if it takes longer than [t] wall-clock seconds.

        The result is [Ok (f x)] unless evaluation is interrupted,
        in which case the result is [(Error t)]. *)
    let timeout time f x =
      Sys.set_signal Sys.sigalrm handler;
      ignore (setitimer ITIMER_REAL { it_interval= 0. ; it_value = time });
      match f x with
      | r             -> ignore (setitimer ITIMER_REAL notimer); Ok r
      | exception exn -> match exn with
                         | Timeout       -> Error time
                         | _             -> ignore (setitimer ITIMER_REAL notimer); raise_notrace exn
  end

  (** [(home ())] is the current (effective) user's home directory
        according to the password database.

        Raises [Not_found] if no such entry exists.
   *)
  let home () = (getpwuid (geteuid ())).pw_dir

  (** [(mkdirp ?(perm=0o755) dir)] is like [mkdir(1)] with its [-p]
        option: there is no error if [dir] already exists, and we make parent
        directories as needed. *)
  let mkdirp ?(perm=0o755) fn =
    let sep = Filename.dir_sep in
    let mkd d = if Sys.file_exists d then () else Unix.mkdir d perm in
    let rec loop acc path = match rev acc, path with
      |       _,     [] -> ()
      |      [], ""::ys ->                                    loop ["/"] ys
      |      xs, ""::ys ->                                    loop acc ys
      |      xs,"."::ys ->                                    loop acc ys
      | "/"::xs,  y::ys -> "/" ^ join ~sep (xs @ [y]) |> mkd; loop (y::acc) ys
      |      xs,  y::ys ->       join ~sep (xs @ [y]) |> mkd; loop (y::acc) ys
    in
    String.cuts ~sep fn |> loop []

  (**  Functions for Manipulating the Environment

      [Unix.environment] is a low-level function that returns an array
      of strings formatted like ["PAGER=less"].  The functions in this
      module allow you to work with a friendlier environment
      represented as an association list of variable names and values
      (["PAGER", "less"]).  Also useful with [Sys.runtime_parameters]. *)
  module Env = struct (*$<Env *)

    (** [to_alist] converts a [Unix.environment]-like string array
        into an alist. *)
    let to_alist array = Array.(map (String.cut ~sep:"=") array |> map (id &&& Option.default "") |> to_list)
    (*$Q to_alist
      Q.(triple small_int (always "FOO") small_string) (fun (n,k,v) -> repeat n [(k,v)] = repeat n (to_alist [|k^"="^v|]))
     *)

    (** [(env ())] is [(Unix.environment () |> to_alist)] *)
    let env () = environment () |> to_alist
    (* TEST see of_alist below *)

    (** [of_alist] converts an alist to a [Unix.environment]-like
      string array; {i N.B.} it does {i NOT} actually change the
      environment!

      The result is suitable for passing to functions like
      [Unix.execve] and {!Proc.runfull}.
     *)
    let of_alist alist = alist |> map (fun (a,b) -> a ^ "=" ^ b) |> Array.of_list
    (*$Q of_alist;env
      Q.unit (fun () -> env () |> of_alist = Unix.environment ())
     *)
  end (*$>*)

  (** [signals] is an alist mapping OCaml signal numbers to their traditional Unix names.

      (OCaml signal numbers don't match the traditional Unix signal numbers!) *)
  let signals = Sys.[
        sigabrt   , "SIGABRT";
        sigalrm   , "SIGALRM";
        sigfpe    , "SIGFPE";
        sighup    , "SIGHUP";
        sigill    , "SIGILL";
        sigint    , "SIGINT";
        sigkill   , "SIGKILL";
        sigpipe   , "SIGPIPE";
        sigquit   , "SIGQUIT";
        sigsegv   , "SIGSEGV";
        sigterm   , "SIGTERM";
        sigusr1   , "SIGUSR1";
        sigusr2   , "SIGUSR2";
        sigchld   , "SIGCHLD";
        sigcont   , "SIGCONT";
        sigstop   , "SIGSTOP";
        sigtstp   , "SIGTSTP";
        sigttin   , "SIGTTIN";
        sigttou   , "SIGTTOU";
        sigvtalrm , "SIGVTALRM";
        sigprof   , "SIGPROF";
        sigbus    , "SIGBUS";
        sigpoll   , "SIGPOLL";
        sigsys    , "SIGSYS";
        sigtrap   , "SIGTRAP";
        sigurg    , "SIGURG";
        sigxcpu   , "SIGXCPU";
        sigxfsz   , "SIGXFSZ";]
  (* I was planning on relying on the assertion in this test but now
     I'm not so sure... *)
  (*$Q signals
    Q.unit (fun () -> Assoc.values signals |> map (String.take 3) |> nub = ["SIG"])
  *)

  (** [(string_of_signal signal)] returns the traditional name of the
      OCaml signal number [signal].

      In the event of an unknown signal, the name returned is just the
      integer.
  *)
  let string_of_signal signal = match assoc signal signals with
  | exception Not_found -> string_of_int signal
  | name -> name

  (** Running Subprocesses *)
  module Proc = struct (*$<Proc*)
    (** {1:proc Running Subprocesses}
        These functions allow you to run a subprocess, possibly writing
        data to its standard input, and optionally reading back its
        standard output and / or standard error.

        See the {!unixproctutorial}.
     *)

    (** {1 Functions to Run Subprocesses}

        Note that all these functions flush [stdout] and [stderr] to
        avoid duplicative output. *)

    (** [(runfull ?env ?usepath ?err ?writer ?reader argv)] runs the program
       designated by [(hd argv)] with [(tl argv)] as its command-line
       arguments.

       [argv] is a raw argument vector; it is {i not} passed to the
       shell, so its values do not need to be quoted, and there is no
       support for shell metacharacters such as I/O redirection or
       pipes.  (See {!Shell} for simple-minded pipeline support.)

       If [~writer] is provided, it is called to write data to the
       process's standard input.

       If [~reader] is provided, it is called to read data from the
       process's standard output.

       If [~err] is provided, it is called to read data from the
       process's standard error.

       The return value is a triple consisting of:
       - the {!process_status}
       - the stdout value returned by [~reader], if any, or [None] if [~reader] was not provided
       - the stderr value returned by [~err], if any, or [None] if [~err] was not provided

       If given, [~env] is used as the environment; the default is to
       use the value of [(environment ())]; [~env] must be in the same
       format.

       [~usepath] determines whether or not [(hd argv)] is searched in
       [PATH]; default: [true]

       If [(argv = [])], [Invalid_argument] is raised.

       Note that if the process produces output to either stdout or
       stderr, and you don't read it (via [~reader] or [~err]), the
       process status will be [(WSIGNALED (-8))], indicating SIGPIPE.
    *)
    let runfull ?env ?(usepath=true) ?err ?writer ?reader argv =
      let env = Option.default (environment ()) env in
      match argv with
      | [] -> invalid_arg "run"
      | argv0::_ ->
         let subin,subout = pipe () in
         let errin,errout = pipe () in
         let win,  wout   = pipe () in
         let parent sub w =
           close win; close wout; close subout; close errout;
           let result = match reader with
             | None -> close subin; None
             | Some f -> let chan = in_channel_of_descr subin in
                         match f chan with
                         | exception exn -> close_in chan; close errin; raise_notrace exn
                         | result -> close_in chan; Some result
           in
           let eresult = match err with
             | None -> close errin; None
             | Some f -> let chan = in_channel_of_descr errin in
                         match f chan with
                         | exception exn -> close_in chan; raise_notrace exn
                         | result -> close_in chan; Some result
           in
           (* wait for whichever subprocess finished first *)
           let p1,s1 = waitpid [] (if something writer then ~-1 else sub) in
           (* wait for the other one; ASSERT hd via assertions above *)
           match writer with
           | None   -> s1, result, eresult
           | Some _ ->
              assert (something w);
              let pids = [sub; Option.get w] in
              let p2,s2 = waitpid [] (filter ((<>) p1) pids |> hd) in
              (* return the status of the exec'd process *)
              if p1=sub then s1, result, eresult else s2, result, eresult
         in
         (* improve error handling for bad proc names *)
         (* TODO SIMPLIFY this impenetrable Boolean expr! *)
         if usepath && not (File.Command.exists argv0) || not usepath && not (File.executable argv0)
         then begin
             close win; close wout; close subin; close subout; close errin; close errout;
             invalid_arg ("command not found: %s" % argv0)
          end else
           flush Stdlib.stdout; flush Stdlib.stderr;
           match fork () with
           | 0 -> (* sub child  *)
              dup2 subout stdout; close subin;
              dup2 errout stderr; close errin;
              dup2 win    stdin;  close wout;
              (if usepath then execvpe else execve) argv0 (Array.of_list argv) env
           | sub -> (* parent *)
              match writer with
              | None -> parent sub None
              | Some w -> match fork () with
                          | 0 -> (* writer child *)
                             close win; close subin; close subout; close errin; close errout;
                             let chan = out_channel_of_descr wout in
                             w chan;
                             close_out chan;
                             exit 0
                          | writer -> parent sub (Some writer)
    (*$Q runfull
      Q.unit (fun () -> (WEXITED 0, None, None) = (runfull ["true"]))
      Q.unit (fun () -> (WEXITED 1, None, None) = (runfull ["false"]))
      Q.unit (fun () -> not @@ succeeds runfull ["fjhgfjhgfjhgfjhgfjhg"])
      Q.unit (fun () -> not @@ succeeds (runfull ~usepath:false) ["true"])
      Q.unit (fun () -> "" = (runfull ~env:[||] ~reader ["env"] |> stdout))
      Q.unit (fun () -> "FOO=bar\n" = (runfull ~env:(Env.of_alist ["FOO","bar"]) ~reader ["env"] |> stdout))
      Q.small_string (fun str -> str = (runfull ~writer:(string str) ~reader ["cat"] |> stdout))
      Q.unit (fun () -> not @@ succeeds (runfull ~usepath:false) ["ocamlc";"-v"])
    *)
    (**/**)
    module Tests = struct (*$<Tests*)
      let eq a b = a = b
      let tests = [
          (eq (WEXITED 0), eq (Some "foo\n"), eq (Some "")),
          (fun () -> runfull ~err:pread ~writer:ignore ~reader:pread ["date";"+foo"]);
          (eq (WEXITED 0), eq (Some "foo\n"), eq (Some "")),
          (fun () -> runfull ~err:pread ~reader:pread ["date";"+foo"]);
          (eq (WEXITED 0), eq (Some "foo\n"), nothing),
          (fun () -> runfull ~writer:ignore ~reader:pread ["date";"+foo"]);
          (eq (WEXITED 1), eq (Some ""), something),
          (fun () -> runfull ~err:pread ~writer:ignore ~reader:pread ["date";"foo"]); (* erroneous *)
          (eq (WEXITED 1), eq (Some ""), nothing),
          (fun () -> runfull ~writer:ignore ~reader:pread ["date";"foo"]);
          (eq (WEXITED 0), eq (Some "FOO"), nothing),
          (fun () -> runfull ~writer:(flip pwrite "FOO") ~reader:pread ["tr";"a-z";"A-Z"]);
          (eq (WEXITED 1), eq (Some ""), (function None -> false | Some x -> split ~sep:"\n" x |> len = 3)),
          (fun () -> runfull ~err:pread ~writer:(flip pwrite "foo") ~reader:pread ["tr";"a-z"]); (* erroneous *)
          (eq (WEXITED 0), (function None -> false | Some x -> String.len x > 1000), nothing),
          (fun () -> runfull ~writer:(fun c -> readfile "prelude.ml" |> pwrite c) ~reader:pread ["tr";"a-z";"A-Z"]);
          (eq (WEXITED 0), (function None -> false | Some x -> String.len x > 1000), nothing),
          (fun () -> runfull ~writer:(fun outchan -> within (copyto outchan) "prelude.ml") ~reader:pread ["tr";"a-z";"A-Z"]);
        ]
      let run () = map (fun (r,f) -> r,f ()) tests
      let checker (expected,(status,stdout,stderr)) =
        T3.fst expected status, T3.snd expected stdout, T3.thd expected stderr
      let okay = function true,true,true -> true | _ -> false
      (*$T run
        run () |> map checker |> all okay
      *)
    end (*$>*)
    (**/**)

    (** [run] is equivalent to {!runfull} except it returns only the
        stdout and stderr values.

        It does not return the process status.

        If the process exits with a non-zero [WEXITED] status, [Failure] is
        raised, unless [~oknon0:true] (default: [false]).

        [Failure] is also raised if the process was killed or stopped
        by a signal.
    *)
    let run ?(oknon0=false) ?env ?usepath ?err ?writer ?reader argv =
      match runfull ?env ?usepath ?err ?writer ?reader argv with
      | WEXITED   0, stdout, stderr             -> stdout, stderr
      | WEXITED   _, stdout, stderr when oknon0 -> stdout, stderr
      | WEXITED   n, stdout, stderr             -> failwith ("exit %d" % n)
      | WSIGNALED n, stdout, stderr             -> failwith ("signal %s" % string_of_signal n)
      | WSTOPPED  n, stdout, stderr             -> failwith ("stopped %s" % string_of_signal n)
    (*$Q run
      Q.unit (fun () ->        succeeds  run                ["true"])
      Q.unit (fun () -> not @@ succeeds  run                ["false"])
      Q.unit (fun () -> not @@ succeeds (run ~oknon0:false) ["false"])
      Q.unit (fun () ->        succeeds (run ~oknon0:true)  ["false"])
      Q.unit (fun () -> not @@ succeeds (run ~oknon0:true)  ["dkjhdkjhdkjh"])
      Q.unit (fun () -> not @@ succeeds (run ~oknon0:false) ["dkjhdkjhdkjh"])
      Q.unit (fun () -> not @@ succeeds (run ~oknon0:true)  ["dkjhdkjhdkjh"])
      Q.unit (fun () -> run ~reader ["date";"+foo"] = (Some "foo\n", None))
    *)

    (** {1 Reading from Processes} *)

    (** [readwith] is equivalent to {!run} except it returns only the stdout value. *)
    let readwith ?oknon0 ?env ?usepath reader argv = match run ?oknon0 ?usepath ?env ~reader argv with
      | Some stdout, None -> stdout
      | _ -> assert false
    (*$Q readwith
      Q.unit (fun () ->        succeeds (readwith               reader) ["true"])
      Q.unit (fun () -> not @@ succeeds (readwith               reader) ["false"])
      Q.unit (fun () -> not @@ succeeds (readwith ~oknon0:false reader) ["false"])
      Q.unit (fun () ->        succeeds (readwith ~oknon0:true  reader) ["false"])
      Q.unit (fun () -> not @@ succeeds (readwith ~oknon0:true  reader) ["dkjhdkjhdkjh"])
      Q.unit (fun () -> not @@ succeeds (readwith ~oknon0:false reader) ["dkjhdkjhdkjh"])
      Q.unit (fun () -> not @@ succeeds (readwith ~oknon0:true  reader) ["dkjhdkjhdkjh"])
      Q.unit (fun () -> readwith reader ["date";"+foo"] = "foo\n")
    *)

    (** [read] is [(readwith ~reader:Prelude.read)]. *)
    let read ?oknon0 ?env ?usepath argv = match run ?oknon0 ?usepath ?env ~reader:pread argv with
      | Some stdout, None -> stdout
      | _ -> assert false
    (*$Q read
      Q.unit (fun () -> read ["date";"+foo"] = "foo\n")
      Q.unit (fun () -> let s = "foo\nbar\nbaz" in read ["date";"+"^s] = s^"\n")
    *)

    (** [readline ?oknon0 ?env ?usepath argv] is specially designed to read one line from stdin.

        It reads only one line, and ignores any SIGPIPE from the process.

        Example: [readline ["emacs"; "--version"]] would return
        something like ["GNU Emacs 28.2"] without the additional lines
        explaining the copyright and lack of warranty.
     *)
    let readline ?(oknon0=false) ?env ?usepath argv = match runfull ?usepath ?env ~reader:readline argv with
      | WEXITED 0,      Some stdout, _      -> stdout
      | WEXITED n,      stdout, _           -> if oknon0 then Option.default "" stdout else failwith ("exit %d" % n)
      | WSIGNALED (-8), Some stdout, _      -> stdout
      | WSIGNALED n,    None,        _
      | WSIGNALED n,    _,           _      -> failwith ("signal %s" % string_of_signal n)
      | WSTOPPED  n,    _,           _      -> failwith ("stopped %s" % string_of_signal n)
    (*$Q readline
      Q.unit (fun () -> not (succeeds readline ["cat";"/dev/null"]))
      Q.unit (fun () -> readline ["date";"+foo"] = "foo")
      Q.unit (fun () -> readline ["date";"+foo\nbar\nbaz"] = "foo")
    *)

    (**/**)
    let preadlines = readlines
    (**/**)
    (** [readlines] is [(readwith ~reader:Prelude.readlines)]. *)
    let readlines ?oknon0 ?env ?usepath argv = match run ?oknon0 ?usepath ?env ~reader:readlines argv with
      | Some stdout, None -> stdout
      | _ -> assert false
    (*$Q readlines
      Q.unit (fun () -> readlines ["date";"+foo"] = ["foo"])
      Q.unit (fun () -> readlines ["date";"+foo\nbar\nbaz"] = ["foo";"bar";"baz"])
    *)

    (**/**)
    let presink chan =
      let buf = Bytes.create blocksize in
      Gen.(optional (readblock ~buf) $ iter ignore) chan
    (**/**)

    (** {1 Writing to Processes} *)

    (** [(write ?oknon0 ?env ?usepath argv str)] writes [str] to the stdin of the subprocess.

        All optional parameters are as for {!run}.

        This is useful for programs that read stdin and produce no output.  Example:
        {[write ["mail"; "-s"; "Your mail is here"; "luser@example.com"] msg]}
        sends mail to the user; [msg] would be a string containing an
        RFC-822 email message, complete with headers and body.

     *)
    let write ?oknon0 ?env ?usepath argv str =
      match run ?oknon0 ?usepath ?env ~reader:presink ~err:presink ~writer:(flip pwrite str) argv with
      | Some (), Some () -> ()
      | _ -> assert false
    (*$Q write
      Q.small_string (succeeds (write ["cat"]))
    *)

    (** [(writelines argv)] is [(String.concat "\n" $ write argv)].

        All optional parameters are as for {!run}.  *)
    let writelines ?oknon0 ?env ?usepath argv lines =
      match run ?oknon0 ?usepath ?env ~reader:presink ~err:presink ~writer:(fun c -> iter (writeline c) lines) argv with
      | Some (), Some () -> ()
      | _ -> assert false
    (*$Q writelines
      Q.(small_list small_string) (succeeds (writelines ["cat"]))
    *)

    (** {1 Bidirectional I/O} *)

    (**[(rw ?oknon0 ?env ?usepath argv str)] writes [str] to the
       process specified by [argv] and returns the process's stdout.

       All optional parameters are as for {!run}.

       Example: [rw ["tr"; "a-z"; "A-Z"] "ocaml"] is an expensive way to get the value ["OCAML"].

     *)
    let rw ?oknon0 ?env ?usepath argv str =
      match run ?oknon0 ?usepath ?env ~writer:(flip pwrite str) ~reader:pread ~err:presink argv with
      | Some stdout, _ -> stdout
      | None,        _ -> assert false
    (*$Q rw
      Q.small_string (fun s -> rw ["cat"] s = s)
      Q.small_string (fun s -> rw ["tr";"a-z";"A-Z"] s = String.uppercase_ascii s)
    *)

    (**[(rwlines ?oknon0 ?env ?usepath argv lines)] writes the [lines]
       to the process specified by [argv] and returns the process's output.

        All optional parameters are as for {!run}. *)
    let rwlines ?oknon0 ?env ?usepath argv lines =
      match run ?oknon0 ?usepath ?env ~writer:(fun c -> iter (writeline c) lines) ~reader:pread ~err:presink argv with
      | Some stdout, _ -> stdout
      | None,        _ -> assert false
    (*$Q rwlines
      Q.(small_list small_string) (fun xs -> xs = [""] || rwlines ["cat"] xs = (map (String.postpend "\n") xs |> String.concat ""))
      Q.(small_list small_string) (fun xs -> xs = [""] || rwlines ["tr";"a-z";"A-Z"] xs = (map String.(uppercase_ascii $ postpend "\n") xs |> String.concat ""))
    *)

    (** {1 Helper Functions} *)

    (** {2 Reader Functions}

        Suitable for [~reader] and [~err].
     *)

    (** [(reader chan)] is {!Prelude.read} and thus returns all the text from [chan].

        [reader] is suitable for a [~reader] parameter; because of the
        pun, when using a local open this shorthand:
        - [Unix.Proc.(runfull ~reader ["date"])]

        is equivalent to:
        - [Unix.Proc.(runfull ~reader:reader ["date"])]

        and thus also to:
        - [Unix.Proc.runfull ~reader:Prelude.read ["date"]]
     *)
    let reader = pread
    (*$Q reader
      Q.unit (fun () -> within reader "/dev/null" = "")
      Q.unit (fun () -> within reader "/etc/passwd" = readfile "/etc/passwd")
    *)

    (** [(err chan)] is {!Prelude.read} and thus returns all the text from [chan].

        This function is useful for local opens as for {!reader} above.
     *)
    let err = pread
    (*$Q err
      Q.unit (fun () -> within err "/dev/null" = "")
      Q.unit (fun () -> within err "/etc/passwd" = readfile "/etc/passwd")
    *)

    (** [(sink chan)] reads all data from [chan] and discards it.

        This function is suitable for use with [~reader] or [~err] in order to avoid a SIGPIPE signal. *)
    let sink = presink
    (*$Q sink
      Q.unit (fun () -> within sink "/etc/passwd" = ())
    *)

    (** {2 Writer Functions}

        Suitable for [~writer].
     *)

    (** [(string str chan)] writes [str] to the process via [chan]. *)
    let string str chan = pwrite chan str
    (* TODO TEST make this a real test *)
    (*$Q string
      Q.small_string (fun s -> succeeds (without (string s)) "/dev/null")
    *)

    (** [(lines list chan)] writes each element of [list] to the
        process via [chan], adding a terminating newline to each element. *)
    let lines list chan = iter (writeline chan) list
    (* TODO TEST make this a real test *)
    (*$Q lines
      Q.(small_list small_string) (fun xs -> succeeds (without (lines xs)) "/dev/null")
    *)

    (** [(channel ic oc)] writes the data on the input channel [ic] to the process via [oc]. *)
    let channel ic oc = copyto oc ic
    (* TODO TEST make this a real test *)
    (*$Q channel
      Q.unit (fun () -> succeeds (without (fun oc -> within (fun ic -> channel ic oc) "prelude.ml")) "/dev/null")
     *)

    (** [(file fn oc)] writes the contents of the file [fn] to the process via [oc]. *)
    let file fn oc = within (copyto oc) fn
    (* TODO TEST make this a real test *)
    (*$Q file
      Q.unit (fun () -> succeeds (without (file "prelude.ml")) "/dev/null")
     *)

    (** {2 Return-value Helpers} *)

    (** [status] converts the triple return value of {!runfull} to the
        exit status, i.e. the value [n] of process status [(WEXITED n)].

        If the process status is either [WSIGNALED] or [WSTOPPED],
        [Failure] is raised.
    *)
    let status = function
      | WEXITED   n, _, _ -> n
      | WSIGNALED n, _, _ -> failwith ("signal %s" % string_of_signal n)
      | WSTOPPED  n, _, _ -> failwith ("stopped with %s" % string_of_signal n)
    (*$Q status
      Q.unit (fun () -> 0 = (runfull ["true"] |> status))
      Q.unit (fun () -> 1 = (runfull ["false"] |> status))
    *)

    (** [explanation] returns a pair of explanations for the termination of the proces.

        The first explanation is short, the second may be longer (possibly multiple lines).
     *)
    let explanation = function
      | WEXITED   0, _, _                -> "exit 0",      "exited successfully"
      | WEXITED   n, _, (None | Some "") -> "exit %d" % n, "exited with status %d" % n
      | WEXITED   n, _, Some err         -> "exit %d" % n, sprintf "exited with status %d:\n%s" n err
      | WSIGNALED n, _, _                -> Pair.dup ("signal %s" % string_of_signal n)
      | WSTOPPED  n, _, _                -> Pair.dup ("stopped with %s" % string_of_signal n)
    (*$Q explanation
      Q.unit (fun () -> "exit 0" = (runfull ["true"]  |> explanation |> fst))
      Q.unit (fun () -> "exit 1" = (runfull ["false"] |> explanation |> fst))
    *)

    (** [successful] converts the triple return value of {!runfull} to
        [true] if it contains [WEXITED 0] and [false] otherwise. *)
    let successful = function
      | WEXITED   0, _, _ -> true
      | WEXITED   _, _, _
      | WSIGNALED _, _, _
      | WSTOPPED  _, _, _ -> false
    (*$Q successful
      Q.unit (fun () -> (runfull ["true"] |> successful))
      Q.unit (fun () -> not (runfull ["false"] |> successful))
    *)

    (** [stdout] converts the triple return value of {!runfull} to the
        stdout value. *)
    let stdout = function _, None, _ -> invalid_arg "stdout" |  _, Some stdout, _ -> stdout
    (*$Q stdout
      Q.small_string (fun s -> runfull ~writer:(string s) ~reader ["cat"] |> stdout = s)
    *)

    (** [stderr] converts the triple return value of {!runfull} to the
        stderr value. *)
    let stderr = function _, _, None -> invalid_arg "stderr" |  _, _, Some stderr -> stderr
    (*$Q stderr
      Q.unit (fun () -> runfull ~err:Prelude.readlines ["ocamlc";"-djhgdjhgdjg"] |> stderr |> len > 0)
    *)

    (** {1:unixproctutorial Tutorial}

        {2 Reading Data from a Process}
        The most common use case is probably to read data back from
        the process's stdout; you would typically use one of {!read},
        {!readline}, or {!readlines}.

        [read] returns all of the process's stdout as a string:
        - [read ["date";"+%Y"] = "2018\n"]
        - [read ["ocamlc";"-v"] = "The OCaml compiler, version 4.06.1\nStandard library directory: /usr/app/lib/opam/4.06.1/lib/ocaml\n"]

        Sometimes you know the subprocess will only return a single
        line; in this case, you might use [readline] to save you from
        having to trim a trailing newline:
        - [readline ["date";"+%Y"] = "2018"]

        [readline] can also be used to simulate piping the process's
        stdout to [head -1]:
        - [readline ["ocamlc";"-v"]
= "The OCaml compiler, version 4.06.1"]

        [readlines] returns all of the process's stdout as a list of lines:
        - [readlines [ "ocamlc";"-v" ] = ["The OCaml compiler, version 4.06.1";
"Standard library directory: /usr/app/lib/opam/4.06.1/lib/ocaml"]]
        - [read ["ocamlc";"-v"] |> split ~sep:"\n" = readlines ["ocamlc";"-v"]]

        {2 Writing Data to a Process}
        Sometimes you only need to write data to a process; we can
        send an email with [write]:
        - [Unix.Proc.write ["mailx"; "-s"; "Email from OCaml"; "keith"] "Greetings from OCaml!"]

        [writelines] works analogously:
        - [1--10 |> map string_of_int |> Unix.Proc.writelines ["mailx"; "-s"; "Some Integers"; "keith"]]

        {2 Reading and Writing to and from a Process}
        Sometimes you need to both write data to a process and then
        read data back from it.  You can do this with {!rw} and
        {!rwlines}:
        - [rw [ "tr"; "a-z"; "A-Z" ] "foobar" = "FOOBAR"]
        - [1--10 |> map string_of_int |> Unix.Proc.rwlines [ "wc"; "-l" ] = "10\n"]

        {2 Space Efficiency with [run]}
        In order to avoid reading all of a potentially large amount of
        data into memory, you can do all your processing inside [~reader]:
        - [1099 = (run ~reader:(foldlines (fun n _ -> succ n) 0) ["curl"; "-Ls"; "https://www2.lib.uchicago.edu/keith/software/prelude/Prelude.html"] |> fst |> Option.get)]

        {2 Accessing Standard Error with [run]}
        Sometimes you want to see the stderr output of a process as
        well the stdout, perhaps so that you can display it when
        there's an error:
        {[ match run ~oknon0:true ~err:Prelude.read ["ocamlc";"-c";"foo.ml"] with
| None, Some ""  -> print "compilation successful"
| None, Some err -> print err ]}

        {2 Accessing the Process State with [runfull]}
        [runfull] is just like [run] but also returns the process
        state, so you can examine the exit status or detect that the
        process was killed by a signal.

        - [runfull ["true"] = (WEXITED 0, None, None)]

        The [status] function will extract just the exit status:
        - [runfull ["false"] |> status = 1]

        {2 A Warning About SIGPIPE}

        Whenever you interact with a process that produces output to
        stdout or stderr, if you don't read all of that output, you
        will get a SIGPIPE exception.  For example,
        /usr/share/dict/words is 1,185,564 bytes on my system; this
        command looks like a reasonable way to get the first line of
        the file:
        {[
 Unix.Proc.run ~reader:readline ["cat"; "/usr/share/dict/words"] ]}

        but in fact it raises SIGPIPE, because we neglected to read
        the rest of the output.

        To solve this problem, you need to either read all the output
        (which may be inefficient), or ignore the SIGPIPE.

        The {!readline} function takes care of ignoring SIGPIPE for you.
     *)
  end (*$>*)

  (** Simple functions to interact with shell pipelines. *)
  module Shell :  sig
    (** {1:shell Shell Pipelines}
      {!Shell.input} [f (cmd list)] is analogous to:

      {[
      let cmd  = "echo" in
      let list = ["black and white"] in
      let chan = Unix.open_process_in (join (cmd::list))  in
      let result = f chan in
      Unix.close_process_in chan, result ]}

      but the former makes sure to close the channel even if an
      exception is raised in [f].

      In addition, {!Shell.input} is somewhat safer than
      {!Unix.open_process_in} in that it quotes the components of
      [list], thus preventing some errors and security holes due to
      shell metacharacters, filenames with spaces in them, and the
      like.

      For example, compare:
      - {v
# let o,(!) = Unix.(open_process_in, close_process_in) in
  let c = o "echo black & white" in let r = read c in !c |> ignore; r;;
/bin/sh: white: command not found
- : string = "black\n" v}
      - {v
# Unix.Shell.(read @@ cmd ["echo";"black & white"]);;
- : string = "black & white\n" v}

      {!Shell.output} is analogous.

      Pipelines of commands are also supported, and several
      higher-level convenience functions are built upon {!val-input} and {!val-output}
      and so inherit their properties. *)

    (** {1 Types} *)

    (** The type of simple shell commands. *)
    type cmd = string list

    (** The type of pipelines of shell commands. *)
    type pipeline = private string

    (** {1 Representing Commands and Pipelines} *)

    (** [(cmd argv)] makes a [pipeline] out of a list of strings,
        representing a single shell command.

        The list is the argument vector representing the command, including argv0.

        Examples:
        - [["echo"; "foo and (bar)"]]
        - [["date"; "+%Y-%m-%d"]]
        - [["cat"; "foo"; "bar"; "baz"]]
    *)
    val cmd : cmd -> pipeline

    (** [(pipeline argvs)] makes a [pipeline] out of a list of shell commands.

        For example:
        - [[["cat";"/etc/passwd"]; ["wc";"-l"]]] represents:
        - ["cat /etc/passwd | wc -l"]
    *)
    val pipeline : cmd list -> pipeline

    (** {1 Input: Reading from Shell Piplelines} *)

    (** [(input f pipeline)] executes the shell pipeline (or simple
        command), applies [f] to a channel connected to the standard
        output, and returns the exit status and the result of [f]'s
        evaluation.

        Example:
        - [input Prelude.readline @@ cmd ["uname";"-s"]]

        returns [(Prelude.Unix.WEXITED 0, "Linux")] on my system.
    *)
    val input : (in_channel -> string) -> pipeline -> process_status * string

    (** [(read ?ign pipeline)] executes the shell pipeline and returns
        just its standard output; any non-zero exit status raises
        [Failure] unless you pass [~ign:true], in which case the empty
        string is returned. *)
    val read : ?ign:bool -> pipeline -> string

    (** [(readline ?(ign=false) pipeline)] executes the shell pipeline and
        returns just the first line of its standard output (sans
        end-of-line characters); any non-zero exit status raises [Failure]
        unless you pass [~ign:true], in which case the empty
        string is returned. *)
    val readline : ?ign:bool -> pipeline -> string

    (** [(readlines ?(ign=false) pipeline)] executes the shell
        pipeline and returns just the list of the lines of its standard
        output; any non-zero exit status raises [Failure] unless you
        pass [~ign:true], in which case the empty list is returned. *)
    val readlines : ?ign:bool -> pipeline -> string list

    (** {1 Output: Writing to Shell Piplelines} *)

    (** [(output f pipeline)] executes the shell pipeline (or simple
       command), applies [f] to a channel connected to the pipeline's
       standard input, and returns the exit status.

        Example: - [output (fun c -> foreach (fprintf c "%d\n") (1,10)) (cmd ["dd";"of=/tmp/foo"])]

        returns Prelude.Unix.WEXITED 0, having written 10 lines to the file ["/tmp/foo"].  *)
    val output : (out_channel -> unit) -> pipeline -> process_status

    (** [(write ?ign pipeline str)] executes the shell [pipeline],
        writing [str] to its standard input.

        Any non-zero exit status raises [Failure] unless you pass
        [~ign:true], in which case the exit status is ignored. *)
    val write : ?ign:bool -> pipeline -> string -> unit

    (** [(writelines ?ign pipeline lines)] executes the shell [pipeline] writing
        each element of [lines] to its status input, appending a newline to each.
     *)
    val writelines : ?ign:bool -> pipeline -> string list -> unit

    (** {1 Bidrectional I/O: Writing to and Reading from Shell Piplelines} *)

    (** [(inout ?env pipeline writer reader)] calls [writer] to write
        data to the standard input of the [pipeline], and then calls
        [reader so se] to read [so] (its standard output) and [se]
        (its standard error), returning the result.

        [env] is the environment for the pipeline.
     *)
    val inout :?env:(string array) -> pipeline -> (out_channel -> unit) -> (in_channel -> in_channel -> 'a) -> 'a

    (** [(sh ?env pipeline str)] writes [str] to the standard input of
        the [pipeline], and then reads its standard output (ignoring
        stderr), returning the result.

        [env] is the environment for the pipeline.
     *)
    val sh : ?env:string array -> pipeline -> string -> string
  end = struct (*$< Shell *)
    type cmd = string list      (* TODO SAFETY make private type *)
    type pipeline = string      (* TODO SAFETY make private type *)

    (* in a Bourne-derived shell, backslash quotes any character; the
       safest (albeit bulkiest) way to quote a shell argument is to
       backslashify every character, rather than to only quote "all
       the known" metacharacters, which may differ from shell to
       shell. *)
    (* TODO OPTIMIZE we could skip quoting [A-Za-z0-9]; NB this will break the tests. *)
    let quote c = {|\|} ^ String.make 1 c
    (* TODO TEST can't test this because it's not exposed in the sig... *)
    (* TODO $Q quote
      Q.char (fun c -> String.len (quote c) = 2)
      Q.char (fun c -> (quote c).[0] = '\\')
      Q.char (fun c -> (quote c).[1] = c)
    *)
    let cmd list =
      map (String.maps quote) list |> join
    (* TODO TEST can't test this because of the sig... *)
    (* TODO $Q cmd
      Q.(list string) (fun xs -> sum (intersperse 1 (map (String.len $ ( * ) 2) xs)) = String.len (cmd xs))
    *)
    let pipeline lists =
      let cmd list =
        map (String.maps quote) list |> join
      in
      let rec loop acc = function
      | []      -> rev acc
      | xs::xss -> loop (cmd xs :: acc) xss
      in
      loop [] lists |> join ~sep:" | "
    (* TODO TEST can't test this because of the sig... *)
    (* TODO $Q cmd
      Q.(list string) (fun xs -> sum (intersperse 1 (map (String.len $ ( * ) 2) xs)) = String.len (cmd xs))
    *)
    let input read (cmd : pipeline) =
      let chan = open_process_in cmd in
      match read chan with
      | str           -> close_process_in chan, str
      | exception exn -> close_process_in chan |> ignore; raise_notrace exn
    (*$Q input
      Q.unit (fun () -> let s = "foo bar\\" in (WEXITED 0, s) = input Prelude.readline (cmd ["echo";s]))
      Q.unit (fun () -> (WEXITED 1, "") = input Prelude.read (cmd ["false"]))
      Q.small_string (fun s -> let s = String.translate "\000\n" "  " s in (WEXITED 0, String.uppercase_ascii ("x"^s^"\n")) = input Prelude.read (pipeline [["echo";"x"^s];["tr";"a-z";"A-Z"]]))
    *)

    let reader f cmd = match input f cmd with
    | WEXITED 0,   str -> str
    | WEXITED n,   str -> failwith (sprintf "exit %d: %s" n cmd)
    | WSIGNALED n, str -> failwith (sprintf "killed by signal %d: %s" n cmd)
    | WSTOPPED n,  str -> failwith (sprintf "stopped by signal %d: %s" n cmd)
    let read ?(ign=false) = (if ign then default "" else apply) (reader pread)
    (*$Q read
      (Q.unit) (fun () -> not @@ succeeds read (cmd ["false"]))
      (Q.unit) (fun () -> "" = read ~ign:true (cmd ["false"]))
      (Q.unit) (fun () -> "" = read (cmd ["true"]))
      (Q.unit) (fun () -> let s = "foo bar" in s^"\n" = read (cmd ["echo";s]))
    *)
    let readline  ?(ign=false) = (if ign then default "" else apply) (reader readline)
    (* TODO TESTS *)
    let readlines ?(ign=false) = (if ign then default [] else apply) (reader readlines)
    (* TODO TESTS *)
    let output write (cmd : pipeline) =
      let chan = open_process_out cmd in
      match write chan with
      | ()            -> close_process_out chan
      | exception exn -> close_process_out chan |> ignore; raise_notrace exn
    (* TODO TESTS *)

    let writer f cmd = match output f cmd with
    | WEXITED 0   -> ()
    | WEXITED n   -> failwith (sprintf "exit %d: %s" n cmd)
    | WSIGNALED n -> failwith (sprintf "killed by signal %d: %s" n cmd)
    | WSTOPPED n  -> failwith (sprintf "stopped by signal %d: %s" n cmd)
    let write ?(ign=false) cmd str = match writer (flip pwrite str) cmd with
      | exception exn -> if ign then () else raise_notrace exn
      | () -> ()
    (* TODO TEST dd with status=none option won't work on every OS;
       should be replaced with special test executable *)
    (*$Q write
      Q.small_string (fun s -> () = write (cmd ["dd";"of=/dev/null";"status=none"]) s)
     *)
    let writelines ?(ign=false) cmd lines =
      let eachline c l = pwrite c l; pwrite c "\n" in
      (if ign then default () else apply) (writer (fun c -> iter (eachline c) lines)) cmd
    (* TODO TESTS *)
    let inout ?env pipeline (w : out_channel -> unit) r =
      let env = match env with None -> Unix.environment () | Some e -> e in
      let fromc,toc,err = Unix.open_process_full pipeline env in
      try
        w toc; close_out toc;
        let result = r fromc err in
        close_in fromc;
        close_in err;
        result
      with
      | exn -> default () close_in fromc; default () close_in err; default () close_out toc; raise_notrace exn
    (* TODO TESTS *)
    let sh ?env pipeline str =
      inout ?env pipeline (if str = "" then ignore else (flip pwrite str)) (fun so _ -> pread so)
    (* TODO TESTS *)

  end (*$>*)
end (*$>*)

(** {1:prereq Prerequisites} *)

(** Functions to test for the availability of external prerequisite commands. *)
(* TODO TESTS *)
module Prereq = struct (*$< Prereq *)

  (** {1 Types} *)

  (** Different ways to check for the availability of a command. *)
  type validator =
    | Exists of string          (** tests mere existence of an executable by name (perhaps in [$PATH]) *)
    | Succeeds of string list   (** tests that the execution of the given argv actually succeeds with exit status 0 *)
    | Stdout of string list * (in_channel -> bool) (** executes the argv and applies the function to the standard output *)
    | Stderr of string list * (in_channel -> bool) (** executes the argv and applies the function to the standard error *)

  (** {1 Prerequisite Checking } *)

  (** [(check prereqs)] tests for the existence of all the commands in [prereqs].

      In the list of results, [(Ok cmd)] indicates that [cmd] passed the
      existence check, and [(Error cmd)] indicates that it failed.

      If a [Stdout] or [Stderr] validator raised an unexpected
      exception, then the exception is appended to the command name.*)
  let check =
    let check cmd f x = match f x with
      | exception (End_of_file | Failure _ | Invalid_argument _) -> Error cmd
      | exception exn -> Error String.(Exn.to_string exn |> replace "\n" " "  |> replace "\r" " " |>sprintf "%s: %s" cmd)
      | _, Some true, _ -> Ok cmd | _ -> Error cmd in
    let each = function
      | Succeeds [] | Stdout ([],_) | Stderr ([],_)         -> invalid_arg "check"
      | Exists cmd                             -> File.Command.exists cmd |> Result.of_bool cmd
      | Succeeds ((cmd::_) as argv)            ->
         if default false Unix.Proc.(runfull ~reader:sink ~err:sink $ successful) argv then Ok cmd else Error cmd
      | Stdout (((cmd::_) as argv), r) -> check cmd Unix.Proc.(runfull ~reader:r) argv
      | Stderr (((cmd::_) as argv), e) -> check cmd Unix.Proc.(runfull ~err:e)    argv
    in
    map each
  (* TODO need test for succesful stderr *)
  (*$T check
    check [] = []
    check [Exists "ocamlc"] = [Ok "ocamlc"]
    check [Exists ""] = [Error ""]
    let cmd = "djghsdghfdhgfdhgfd" in check [Exists cmd] = [Error cmd]
    let argv = ["true"] in check [Succeeds argv] = [Ok (hd argv)]
    let argv = ["false"] in check [Succeeds argv] = [Error (hd argv)]
    let argv = ["ocamlc";"-v"] in check [Succeeds argv] = [Ok (hd argv)]
    let argv = ["echo";"foo"] in check [Stdout (argv, readline $ String.eq (nth argv 1))] = [Ok (hd argv)]
    let argv = ["echo";"foo"] in check [Stderr (argv, readline $ String.eq (nth argv 1))] = [Error (hd argv)]
   *)

  (** {1 Validation Result Helpers} *)

  (** [succeeds] is [true] iff all the validation results are [(Ok _)]. *)
  let succeeds results = all Result.to_bool results
  (*$T succeeds
    check (repeat 3 (Exists "ocamlc")) |> succeeds
    check [Exists "ocamlc"; Exists "djhgdjhgdjhgdjhg"; Exists "ocamlc"] |> succeeds |> not
    check [Exists "djhgdjhgdjhgdjhg"; Exists "ocamlc"; Exists "ocamlc"] |> succeeds |> not
    check [Exists "ocamlc"; Exists "ocamlc"; Exists "djhgdjhgdjhgdjhg"] |> succeeds |> not
   *)

  (** [to_alist] converts a validation result to an association list. *)
  let to_alist results =
    let each r acc = match r with
      | Ok cmd    -> (cmd, true)  :: acc
      | Error cmd -> (cmd, false) :: acc
    in
    foldr each [] results
  (*$= to_alist
    (check (repeat 3 (Exists "ocamlc")) |> to_alist) (repeat 3 ("ocamlc",true))
    (check (Exists "" :: repeat 3 (Exists "ocamlc")) |> to_alist) (("",false) :: repeat 3 ("ocamlc",true))
   *)

  (** [fails] raises a {!Failed_prerequisite} exception if any of the
      validation results are [Error _]; otherwise it returns [()].

      The payload of the exception is a list of all the failed command names.

      N.B. the exception will be caught and printed nicely by {!syserrors}.
   *)
  let fails results =
    let each r acc = match r with
      | Ok _    -> acc
      | Error e -> e :: acc
    in
    foldr each [] results |> function [] -> () | errs -> raise (Failed_prerequisite (errs))
  (*$T fails
    Prelude.succeeds (fun () -> check (repeat 3 (Exists "ocamlc")) |> fails) ()
    not (Prelude.succeeds (fun () -> check (Exists "" :: repeat 3 (Exists "ocamlc")) |> fails) ())
   *)

  (** {1 Validator Helpers}

      Function to help in writing validation functions for [Stdout] or [Stderr]. *)

  (** [(minversion vnum)] is a validation function to check that the
      command has a version number [>= vnum].

      The validator reads the first line (only) of the output on the
      channel and expects it to be exactly a version string, no more no
      less.  This means you need to compose your argv to return just
      the version number.

      Both [vnum] and the program's output will be processed with {!version_of_string}. *)
  let minversion vnum chan = version_of_string @@ input_line chan >= version_of_string vnum

  (** {1 Cmdliner Helpers}

      Functions to interoperate with Daniel Bünzli's [Cmdliner].
   *)

  (** [(cmdliner ?escape ?sections prereqs)] returns a string suitable
      for a [`P] element in a [Cmdliner] man page (say in a [(`S "PREREQUISITES")]
      section).

      [escape] is a function that will be used to escape strings so
      that they don't get interpreted by the documentation markup
      language (default: no escaping).  [Cmdliner.Manpage.escape] is
      recommended.

      [sections] is a list of man page section numbers; it must be
      either the empty list (the default) or a parallel list of the
      same length as [prereqs].  Section numbers should be of the form
      ["1"], ["8"], ["1p"], and the like.
  *)
  let cmdliner ?escape ?sections prereqs =
    let (!) = Option.default id escape in
    let format name section = sprintf "$(b,%s)(%s)" !name !section in
    let each name section = match section, name with
      | s, Exists n | s, Succeeds (n::_) | s, Stdout (n::_, _) | s, Stderr (n::_, _) -> format n s
      | _ -> invalid_arg "cmdliner"
    in
    let sections = Option.default (repeat (len prereqs) "1") sections in
    if prereqs = []
    then "None."
    else map2 each prereqs sections |> join ~sep:", " |> String.postpend "."

end (*$>*)

(** {1:extensions Extensions}
    Extensions to modules we don't want to depend upon.

    - {!Extended.Benchmark}: additional functions for Christophe Troestler's [Benchmark] module.
    - {!Extended.Parmap}: additional functions for [Parmap].
    - {!Extended.Re}: additional regular expression functions for [Re].
    - {!Extended.Uri}: additional [Uri] functions.
    - {!Extended.Xmlm}: additional XML functions for Daniel Bünzli's [Xmlm].
*)

(** Extensions.

    Each of these modules is a functor and needs to be applied to the
    external module it extends; this avoids creating a dependency on
    those modules. *)
module Extended = struct

  (* To add a new Extended module, position point and M-x kw-prelude-extended *)

  (** {1:benchmark Extended.Benchmark} *)

  module type BENCHMARK = sig
    type style
    type samples
    val throughputN : ?min_count:Int64.t -> ?style:style -> ?fwidth:int ->
      ?fdigits:int -> ?repeat:int -> int -> (string * ('a -> 'b) * 'a) list -> samples
    val tabulate : ?no_parent:bool -> ?confidence:float -> samples -> unit
  end

  (** Additional functions for Christophe Troestler's [Benchmark] module.

      Typical usage:

      {[ module Benchmark = struct include Benchmark include Extended.Benchmark (Benchmark) end ]}

      @see <http://ocaml-benchmark.forge.ocamlcore.org/>
  *)
  module Benchmark (Benchmark : BENCHMARK) = struct
    open Benchmark

    (** [(quick ~n [f1;f2;f3] x)] =
        [(throughputN n ["1", f1, x; "2", f2, x; "3", f3, x] |> tabulate)] *)
    let quick ?(n=1) fs x =
      mapi (fun i f -> string_of_int ~++i, f, x) fs |> throughputN n |> tabulate
  end

  (** {1:parmap Extended.Parmap} *)

  module type PARMAP = sig
    type 'a sequence =
      | L of 'a list
      | A of 'a array
    val parfold: ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> ('a -> 'b -> 'b) -> 'a sequence -> 'b -> ('b->'b->'b) -> 'b
    val pariter : ?init:(int -> unit) -> ?finalize:(unit -> unit) -> ?ncores:int -> ?chunksize:int -> ('a -> unit) -> 'a sequence -> unit
  end

  (** Additional [Parmap] functions.

      Typical usage:

      {[ module Parmap = struct include Parmap include Extended.Parmap (Parmap) end ]}

      @see <http://www.dicosmo.org/code/parmap/>
  *)
  module Parmap (Parmap : PARMAP) = struct (* TODO TESTS *)
    open Parmap
    let of_list l = L l
    let of_array a = A a
    let flatmap ?init ?finalize ?ncores ?chunksize f seq =
      parfold ?ncores ?chunksize (fun x xs -> f x @ xs) seq [] List.append
    module List = struct          (* TODO flesh out; add equivalent Array module *)
      let foldr ?init ?finalize ?ncores ?chunksize concat f acc list =
        parfold ?ncores ?chunksize f (L list) acc concat
      let foldl ?init ?finalize ?ncores ?chunksize concat f =
        foldr ?init ?finalize ?ncores ?chunksize concat (flip f)
    end
    let pariter ?init ?finalize ?ncores ?chunksize config f list =
      pariter ?init ?finalize ?ncores ?chunksize f (L list)
  end

  (** {1:re Extended.Re} *)

  module type RE = sig
    type t
    type re
    val compile : t -> re
    val execp : ?pos:int -> ?len:int -> re -> string -> bool
    val glob :
      ?anchored:bool ->
      ?pathname:bool ->
      ?match_backslashes:bool ->
      ?period:bool ->
      ?expand_braces:bool -> ?double_asterisk:bool -> string -> t  end

  (** Additional [Re] functions.

      Typical usage:

      {[ module Re = struct include Re include Extended.Re (struct include Re include Re.Glob end) end ]}

      @see <https://github.com/ocaml/ocaml-re>
  *)
  module Re (Re : RE) = struct
    open Re

    (** [(fileglob ?path ?dots ?anchored pat)] returns a predicate that matches the
        pattern [pat] interpreted as a "classical file glob pattern".

        A "classical file glob pattern" supports the shell's [*], [?],
        [[]], [[!]], and [{}] metacharacters; leading dots need to be
        explicitly matched (unless [~dots:false]).

        [?path] determines if slashes need to be matched explicitly
        (default: [true]).

        [?anchored] controls whether the regular expression will only
        match entire strings. (default: true).

        Partially apply for efficiency.

        {i N.B.} in many cases, you want to use e.g. [(Filename.basename >> fileglob "*.c")].

        Please note these subtle distinctions:

        - [File.glob Re.(fileglob "*.ml") "." = []] because [glob] is
        matching against names of the form ["./prelude.ml"].

        - [File.glob Re.(Filename.basename >> fileglob "*.ml") "." = ["./prelude.ml"]]

        - [File.glob Re.(fileglob ~anchored:false "*.ml") "." = ["./prelude.ml~"; "./prelude.ml"; "./prelude.mllib"]]
    *)
    let fileglob ?path ?dots ?(anchored=true) pat =
      let re = Re.glob ~expand_braces:true ?pathname:path ~anchored ?period:dots pat |> Re.compile in
      fun str -> Re.execp re str
      (* TODO TEST *)
  end

  (** {1:uri Extended.Uri} *)

  (** Additional [Uri] functions.

      Typical usage:

      {[ module Uri = struct include Uri include Extended.Uri (Uri) end ]}
  *)
  module type URI = sig
    type t
    val remove_query_param : t -> string -> t
    (* TODO TEST *)
    val add_query_param' : t -> string * string -> t
    (* TODO TEST *)
  end

  module Uri (Uri : URI) = struct
    open Uri

    (** [(replace_query_param uri (k,v))] replaces the value of the existing query
        parameter [k] in [uri] with [v]; if [k] does not exist, [(k,v)] is added. *)
    let replace_query_param t (k,v) = remove_query_param t k |> flip add_query_param' (k,v)
    (* TODO TEST *)
  end

  (** {1:xmlm Extended.Xmlm} *)

  module type XMLM = sig
    type name = string * string
    type attribute = name * string
    type tag = name * attribute list
    type dtd = string option
    type signal = [ `Data of string | `Dtd of dtd | `El_end | `El_start of tag ]
    type output
    val output : output -> signal -> unit
  end

  (** Additional [Xmlm] functions.

      Simple base-type constructors that make it much easier to avoid
      trivial type errors.

      Typical usage:

      {[ module Xmlm = struct include Xmlm include Extended.Xmlm (Xmlm) end ]}

      @see <http://erratique.ch/software/xmlm>
   *)
  (* TODO TESTS *)
  module Xmlm (Xmlm : XMLM) = struct
    open Xmlm

    (** [(name ?uri str)] is the {i expanded name} [(uri,local)]; the
        default [uri] is [""]. *)
    let name ?(uri="") str : name = uri, str

    (** [(string_of_name (uri,n))] is ["n"] iff [(uri = "")] and is ["uri:n"] otherwise. *)
    let string_of_name = function
    | ("",str)   -> str
    | (uri, str) -> sprintf "%s:%s" uri str

    (** [(attr name str)] is the attribute [name] with value [str]. *)
    let attr name str : attribute = name, str

    (** [(tag ?attrs name)] is the tag [name] with attributes [attrs];
        the default [attrs] is [[]]. *)
    let tag ?(attrs=[]) name : tag = name,attrs

    (** [(wrap ?uri ?attrs o n f x)] applies [(f x)], which is
        presumed to output XML on [o], in between a pair of
        [(`El_start (tag ?attrs (name ?uri n)))] and [`El_end]
        signals. *)
    let wrap ?uri ?attrs o n f x =
      output o (`El_start (tag ?attrs (name ?uri n)));
      f x;
      output o `El_end

    (** [(wrapc ?uri ?attrs o n data)] outputs on [o] [(`El_start (tag ?attrs (name ?uri n)))],
        followed by [(`Data data)], followed by [`El_end]. *)
    let wrapc ?uri ?attrs o n data = wrap ?uri ?attrs o n (fun () -> output o (`Data data)) ()
  end

end

(** {1:example Example}

    Here's a quick example to give you an idea of what things looks
    like.  This program isn't intended to be necessarily the most
    readable, or the most terse, but rather to illustrate as many
    Prelude features as possible in a small amount of code.

    I've added line numbers; see below for a line-by-line explanation.

    This program reads the file named as its first command-line
    argument, or, if there is no such argument, the file
    /usr/share/dict/words as a default.  It splits each line into
    whitespace-separated words, and adds each word to a multiset (bag)
    which gathers together strings case-insensitively.  Then it prints,
    in case-insensitive alphabetical order, all the words that have
    more than one spelling.  The output looks something like this:

    {[
2: [blackberry's; BlackBerry's]
2: [cordilleras; Cordilleras]
2: [debs; Debs]
2: [dot's; Dot's]
3: [ms; Ms; MS]
3: [teleprompter; TelePrompter; TelePrompTer]
    ]}

    A brief error message (no stack trace) is printed if there's a
    problem opening a named file, in which case the program exits with
    status [125].  The error message might look like this:

    - example: FATAL: xyzzy: No such file or directory

    {[
 1 open Prelude
 2 module MS = Multiset.Make (AIString)
 3 let main () =
 4   argv
 5   |> head
 6   |> Option.default "/usr/share/dict/words"
 7   |> within (foldlines (fun ms -> split >> foldr MS.add ms) MS.empty)
 8   |> flip (MS.fold (conswhen (len >> gt 1))) []
 9   |> rev
10   |> iter (fun ws -> to_string id ws |> printf "%d: %s\n" (len ws))
11 let () = syserrors main ()
    ]}

    {2 Explanation}

    + Opening Prelude is the intended usage.
    + We'll use a {!Multiset} to group together equivalent strings;
      the {!AIString} module implements [OrderedType] for ASCII
      case-Insensitive strings.
    + Define a main function.
    + Get the command-line arguments (excluding argv0) as a list.
    + Take the first arg (as a [string option]).
    + If it's [None] (because [argv] is empty) replace it with
      ["/usr/share/dict/words"].
    + Feed the filename to [within (...)] to be opened, and passed as
      an [in_channel] to the function -- and guaranteed to be closed
      when the function terminates, even if an exception was raised.
      The function (see below) returns a {!Multiset.S.t} of all the
      words in the file.
    + Now we fold over the Multiset and return a list of the
      equivalent words, as a [string list list].  Each sublist is
      the different spellings of a word.
    + Because we consed-up the list, we reverse it to get alphabetical
      order.
    + Now we print each sublist in a notation similar to the way the
      top-level prints a list.
    + Finally we call our [main], catching any exceptions (we only
      expect I/O errors) and printing them attractively, with no stack
      trace.

    {3 Line 7:}
    {[foldlines (fun ms -> split >> foldr MS.add ms) MS.empty]}

    This function adds all the words of the input to our multiset.
    {!foldlines} folds a flipped-cons-like function over the lines of
    an [in_channel].  [MS.empty] is our initial accumulator.  Our
    function uses [split] to split the line into words
    (a [string list]) and we fold over that list with [foldr], which
    is a synonym for {!List.fold_right}.  [MS.add] adds a word to our
    multiset.  {!(>>)} is left-to-right function composition.


    {3 Line 8:}
    {[flip (MS.fold (conswhen (len >> gt 1))) []]}

    This function folds over the equivalence-classes of word-spellings
    that make up our multiset.  The multiset stores each
    equivalence-class as a list of strings that are equal according to
    {!AIString.compare}.

    [(len >> gt 1)] is a predicate that's true iff a word-list has
    length greater than 1 -- in other words, if the word has multiple
    spellings.  {!len} is [List.length] and {!gt} is [(flip (>))].

    {!conswhen} is a [(::)] that only conses when it's predicate is
    true.  So [(MS.fold (conswhen (len >> gt 1)))] is a function that
    takes a multiset and an initial accumulator (the empty list [[]])
    and conses an equivalence-class onto the accumulator if it has
    multiple spellings.  The order of that function's two parameters
    is awkward so we {!flip} them (and avoid writing a lambda).  The
    result of this whole function is a list of lists: a list of
    non-trivial equivalence-classes.


    {3 Line 10:}
    {[iter (fun ws -> to_string id ws |> printf "%d: %s\n" (len ws))]}

    Finally we iterate over the list of equivalence-classes and print
    each list on a line, prefixed by its length.  {!iter} is
    [List.iter].  Prelude does [(include Printf)] since its functions
    are so useful and have unambiguous names.  Since Prelude does
    [(include Prelude.Lists)], [to_string] is {!Lists.to_string}.
    Prelude defines a flexible [to_string] function for all its data
    types and for most of the OCaml base types.  [to_string] takes a
    function to convert a list element to a string -- since we have a
    list of strings, we just use {!id}, the identity function.
    {!Lists.to_string} takes a number of optional parameters but it's
    default is to format a list the way the top-level would.

*)


(*
 * Copyright 2017 Keith Waclena
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *)
