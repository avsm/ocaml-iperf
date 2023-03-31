(** Iperf v3 protocol states. *)

type t =
 | TEST_START
 | TEST_RUNNING
 | RESULT_REQUEST
 | TEST_END
 | STREAM_BEGIN
 | STREAM_RUNNING
 | STREAM_END
 | ALL_STREAMS_END
 | PARAM_EXCHANGE
 | CREATE_STREAMS
 | SERVER_TERMINATE
 | CLIENT_TERMINATE
 | EXCHANGE_RESULTS
 | DISPLAY_RESULTS
 | IPERF_START
 | IPERF_DONE
(** Protocol states in the V3 protocol *)

val to_string : t -> string
(** [to_string t] returns a human-readable string of the protocol state. *)

val to_int : t -> int
(** [to_int t] returns a wire-protocol integer of the state [t]. *)

val of_int : int -> t
(** [of_int i] converts the wire integer to the corresponding {!t} value.
    @raise Failure on an unknown integer. *)

type error =
 | ACCESS_DENIED
 | SERVER_ERROR
(** Error codes in the V3 protocol *)

val error_to_int : error -> int
(** [error_to_int e] returns a wire-protocol integer of the error [e]. *)

val error_of_int : int -> error
(** [int_of_error i] returns the error [e] from the wire-protocol integer [i].
    @raise Failure on an unknown integer. *)

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
