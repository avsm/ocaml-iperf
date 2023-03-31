

val cookie_size: int
(** Length of the initial cookie sent on a new connection. *)

val wire_value_of_state : State.t -> Cstruct.t
(** Get a byte integer to write to the network.
    @see <https://github.com/esnet/iperf/issues/1467> This is supposed to be network endian but is actually host endian. *)

val state_of_wire_value: Cstruct.t -> State.t
(** Get a {!State.t} from a received network integer.
    @see <https://github.com/esnet/iperf/issues/1467> This is supposed to be network endian but is actually host endian.
    @raise Failure on an unknown integer *)

val parse_params : string -> Yojson.Safe.t
(** [parse_params s] decodes the JSON string [s]. *)

val params_length : Yojson.Safe.t -> int
(** [params_length j] gets the [len] length from the params. *)

val params_parallel : Yojson.Safe.t -> int
(** [params_parallel j] gets the amount of parallel connections from the params. *)

