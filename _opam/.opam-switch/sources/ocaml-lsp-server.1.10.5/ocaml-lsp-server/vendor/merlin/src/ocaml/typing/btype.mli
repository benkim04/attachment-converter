(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Basic operations on core types *)

open Asttypes
open Types

(**** Sets, maps and hashtables of types ****)

module TypeSet  : Set.S with type elt = type_expr
module TypeMap  : Map.S with type key = type_expr
module TypeHash : Hashtbl.S with type key = type_expr

(**** Levels ****)

val generic_level: int

val newty2: int -> type_desc -> type_expr
        (* Create a type *)
val newgenty: type_desc -> type_expr
        (* Create a generic type *)
val newgenvar: ?name:string -> unit -> type_expr
        (* Return a fresh generic variable *)

(* Use Tsubst instead
val newmarkedvar: int -> type_expr
        (* Return a fresh marked variable *)
val newmarkedgenvar: unit -> type_expr
        (* Return a fresh marked generic variable *)
*)

(**** Types ****)

val is_Tvar: type_expr -> bool
val is_Tunivar: type_expr -> bool
val is_Tconstr: type_expr -> bool
val dummy_method: label

val repr: type_expr -> type_expr
        (* Return the canonical representative of a type. *)

val field_kind_repr: field_kind -> field_kind
        (* Return the canonical representative of an object field
           kind. *)

val commu_repr: commutable -> commutable
        (* Return the canonical representative of a commutation lock *)

(**** polymorphic variants ****)

val row_repr: row_desc -> row_desc
        (* Return the canonical representative of a row description *)
val row_field_repr: row_field -> row_field
val row_field: label -> row_desc -> row_field
        (* Return the canonical representative of a row field *)
val row_more: row_desc -> type_expr
        (* Return the extension variable of the row *)

val is_fixed: row_desc -> bool
(* Return whether the row is directly marked as fixed or not *)

val row_fixed: row_desc -> bool
(* Return whether the row should be treated as fixed or not.
   In particular, [is_fixed row] implies [row_fixed row].
*)

val fixed_explanation: row_desc -> fixed_explanation option
(* Return the potential explanation for the fixed row *)

val merge_fixed_explanation:
  fixed_explanation option -> fixed_explanation option
  -> fixed_explanation option
(* Merge two explanations for a fixed row *)

val static_row: row_desc -> bool
        (* Return whether the row is static or not *)
val hash_variant: label -> int
        (* Hash function for variant tags *)

val proxy: type_expr -> type_expr
        (* Return the proxy representative of the type: either itself
           or a row variable *)

(**** Utilities for private abbreviations with fixed rows ****)
val row_of_type: type_expr -> type_expr
val has_constr_row: type_expr -> bool
val is_row_name: string -> bool
val is_constr_row: allow_ident:bool -> type_expr -> bool

(* Set the polymorphic variant row_name field *)
val set_row_name : type_declaration -> Path.t -> unit

(**** Utilities for type traversal ****)

val iter_type_expr: (type_expr -> unit) -> type_expr -> unit
        (* Iteration on types *)
val fold_type_expr: ('a -> type_expr -> 'a) -> 'a -> type_expr -> 'a
val iter_row: (type_expr -> unit) -> row_desc -> unit
        (* Iteration on types in a row *)
val fold_row: ('a -> type_expr -> 'a) -> 'a -> row_desc -> 'a
val iter_abbrev: (type_expr -> unit) -> abbrev_memo -> unit
        (* Iteration on types in an abbreviation list *)

type type_iterators =
  { it_signature: type_iterators -> signature -> unit;
    it_signature_item: type_iterators -> signature_item -> unit;
    it_value_description: type_iterators -> value_description -> unit;
    it_type_declaration: type_iterators -> type_declaration -> unit;
    it_extension_constructor: type_iterators -> extension_constructor -> unit;
    it_module_declaration: type_iterators -> module_declaration -> unit;
    it_modtype_declaration: type_iterators -> modtype_declaration -> unit;
    it_class_declaration: type_iterators -> class_declaration -> unit;
    it_class_type_declaration: type_iterators -> class_type_declaration -> unit;
    it_functor_param: type_iterators -> functor_parameter -> unit;
    it_module_type: type_iterators -> module_type -> unit;
    it_class_type: type_iterators -> class_type -> unit;
    it_type_kind: type_iterators -> type_decl_kind -> unit;
    it_do_type_expr: type_iterators -> type_expr -> unit;
    it_type_expr: type_iterators -> type_expr -> unit;
    it_path: Path.t -> unit; }
val type_iterators: type_iterators
        (* Iteration on arbitrary type information.
           [it_type_expr] calls [mark_node] to avoid loops. *)
val unmark_iterators: type_iterators
        (* Unmark any structure containing types. See [unmark_type] below. *)

val copy_type_desc:
    ?keep_names:bool -> (type_expr -> type_expr) -> type_desc -> type_desc
        (* Copy on types *)
val copy_row:
    (type_expr -> type_expr) ->
    bool -> row_desc -> bool -> type_expr -> row_desc
val copy_kind: field_kind -> field_kind

module For_copy : sig

  type copy_scope
        (* The private state that the primitives below are mutating, it should
           remain scoped within a single [with_scope] call.

           While it is possible to circumvent that discipline in various
           ways, you should NOT do that. *)

  val save_desc: copy_scope -> type_expr -> type_desc -> unit
        (* Save a type description *)

  val dup_kind: copy_scope -> field_kind option ref -> unit
        (* Save a None field_kind, and make it point to a fresh Fvar *)

  val with_scope: (copy_scope -> 'a) -> 'a
        (* [with_scope f] calls [f] and restores saved type descriptions
           before returning its result. *)
end

val lowest_level: int
        (* Marked type: ty.level < lowest_level *)

val not_marked_node: type_expr -> bool
        (* Return true if a type node is not yet marked *)

val logged_mark_node: type_expr -> unit
        (* Mark a type node, logging the marking so it can be backtracked.
           No [repr]'ing *)
val try_logged_mark_node: type_expr -> bool
        (* Mark a type node if it is not yet marked, logging the marking so it
           can be backtracked.
           Return false if it was already marked *)

val flip_mark_node: type_expr -> unit
        (* Mark a type node. No [repr]'ing.
           The marking is not logged and will have to be manually undone using
           one of the various [unmark]'ing functions below. *)
val try_mark_node: type_expr -> bool
        (* Mark a type node if it is not yet marked.
           The marking is not logged and will have to be manually undone using
           one of the various [unmark]'ing functions below.

           Return false if it was already marked *)
val mark_type: type_expr -> unit
        (* Mark a type recursively *)
val mark_type_params: type_expr -> unit
        (* Mark the sons of a type node recursively *)

val unmark_type: type_expr -> unit
val unmark_type_decl: type_declaration -> unit
val unmark_extension_constructor: extension_constructor -> unit
val unmark_class_type: class_type -> unit
val unmark_class_signature: class_signature -> unit
        (* Remove marks from a type *)

(**** Memorization of abbreviation expansion ****)

val find_expans: private_flag -> Path.t -> abbrev_memo -> type_expr option
        (* Look up a memorized abbreviation *)
val cleanup_abbrev: unit -> unit
        (* Flush the cache of abbreviation expansions.
           When some types are saved (using [output_value]), this
           function MUST be called just before. *)
val memorize_abbrev:
        abbrev_memo ref ->
        private_flag -> Path.t -> type_expr -> type_expr -> unit
        (* Add an expansion in the cache *)
val forget_abbrev:
        abbrev_memo ref -> Path.t -> unit
        (* Remove an abbreviation from the cache *)

(**** Utilities for labels ****)

val is_optional : arg_label -> bool
val label_name : arg_label -> label

(* Returns the label name with first character '?' or '~' as appropriate. *)
val prefixed_label_name : arg_label -> label

val extract_label :
    label -> (arg_label * 'a) list ->
    (arg_label * 'a * bool * (arg_label * 'a) list) option
(* actual label,
   value,
   whether (label, value) was at the head of the list,
   list without the extracted (label, value) *)

(**** Utilities for backtracking ****)

type snapshot
        (* A snapshot for backtracking *)
val snapshot: unit -> snapshot
        (* Make a snapshot for later backtracking. Costs nothing *)
val backtrack: snapshot -> unit
        (* Backtrack to a given snapshot. Only possible if you have
           not already backtracked to a previous snapshot.
           Calls [cleanup_abbrev] internally *)
val undo_compress: snapshot -> unit
        (* Backtrack only path compression. Only meaningful if you have
           not already backtracked to a previous snapshot.
           Does not call [cleanup_abbrev] *)

(* Functions to use when modifying a type (only Ctype?) *)
val link_type: type_expr -> type_expr -> unit
        (* Set the desc field of [t1] to [Tlink t2], logging the old
           value if there is an active snapshot *)
val set_type_desc: type_expr -> type_desc -> unit
        (* Set directly the desc field, without sharing *)
val set_level: type_expr -> int -> unit
val set_scope: type_expr -> int -> unit
val set_name:
    (Path.t * type_expr list) option ref ->
    (Path.t * type_expr list) option -> unit
val set_row_field: row_field option ref -> row_field -> unit
val set_univar: type_expr option ref -> type_expr -> unit
val set_kind: field_kind option ref -> field_kind -> unit
val set_commu: commutable ref -> commutable -> unit
        (* Set references, logging the old value *)

(**** Forward declarations ****)
val print_raw: (Format.formatter -> type_expr -> unit) ref

val iter_type_expr_kind: (type_expr -> unit) -> (type_decl_kind -> unit)

val iter_type_expr_cstr_args: (type_expr -> unit) ->
  (constructor_arguments -> unit)
val map_type_expr_cstr_args: (type_expr -> type_expr) ->
  (constructor_arguments -> constructor_arguments)

(** merlin: check if a snapshot has been invalidated *)
val is_valid: snapshot -> bool

(** merlin: also register changes to arbitrary references *)
val on_backtrack: (unit -> unit) -> unit

(** merlin: Number of unification variables that have been linked so far.
   Used to estimate the "cost" of unification. *)
val linked_variables: unit -> int
