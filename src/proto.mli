(** Iperfv3 protocol handling functions. *)

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

(*{{{ Copyright (c) 2023 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)
