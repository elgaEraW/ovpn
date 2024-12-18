type config =
  { mutable country : string
  ; mutable location : string
  ; mutable tp_layer : string
  ; mutable is_manual : bool
  }

val check_and_insert
  :  config_map:(string, string Base.Hash_set.t) Base.Hashtbl.t
  -> string
  -> unit

val prepare_config_list : unit -> (string, string Base.Hash_set.t) Base.Hashtbl.t
val pp : ('a, Format.formatter, unit) format -> 'a
val flush : unit -> unit
val sp : ('a, unit, string) format -> 'a
val print_config : config -> unit
val remove_dns_from_interface : unit -> unit
val connect : config -> unit
val connect_vpn : config -> unit
val check_modes : int -> string -> conf:config -> unit
val take_input : string list -> string -> string
val main : unit -> unit
