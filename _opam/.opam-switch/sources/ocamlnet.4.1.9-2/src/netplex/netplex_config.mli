(* $Id$ *)

(** Read the configuration file *)

open Netplex_types

exception Config_error of string

val read_config_file : string -> config_file
  (** Reads a filename and returns the representation object *)

val repr_config_file : string -> config_tree -> config_file
  (** [repr_config_file name tree]: converts the [tree] to a full
      [config_file] object. [name] is the filename reported by the
      object.
   *)

val read_netplex_config : 
      parallelization_type ->
      logger_factory list ->
      workload_manager_factory list ->
      processor_factory list -> 
      config_file ->
        netplex_config
  (** Reads a Netplex configuration file:
    *
    * {[ netplex {
    *      <settings>
    *    }
    * ]}
    *
    * The configuration options are now documented in {!Netplex_admin}.
    *
    * {b More documentation}: See {!Netplex_intro.webserver} for a 
    * complete example of a config file. See {!Netplex_intro.crsock}
    * for explanations how to specify sockets in the config file.
   *)


val read_tls_config :
      ?verify : ((module Netsys_crypto_types.TLS_ENDPOINT) -> 
                 bool -> bool -> bool) ->
      config_file ->
      address ->
      (module Netsys_crypto_types.TLS_PROVIDER) option ->
        (module Netsys_crypto_types.TLS_CONFIG) option
  (** Reads the TLS section of a configuration file: At the passed location
      there must be [tls] section (or [None] is returned).

      The TLS options are now documented in {!Nethttpd_plex.tls}.
   *)
