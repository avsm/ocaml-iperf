module IS = Iperf.V3.State
module IP = Iperf.V3.Proto

type t = {
  cookie: string;
  mutable state: IS.t;
  mutable len: int;
  mutable parallel: int;
  incr_streams: unit -> unit;
  mutable sw: Eio.Switch.t;
  mutable start_times: Unix.process_times;
  mutable start_wallclock: float;
}

let sessions = Hashtbl.create 7

open Eio

let set_state t flow state =
  traceln "Server: state %s -> %s" (IS.to_string t.state) (IS.to_string state);
  t.state <- state;
  Eio.Flow.write flow [ (IP.wire_value_of_state state) ]

let wait_state t flow state =
  traceln "Server: waiting for state message %s" (IS.to_string t.state);
  let c = Cstruct.create 1 in
  Eio.Flow.read_exact flow c;
  let s = IP.state_of_wire_value c in
  if s <> state then
    raise (Failure "expected wait_state received");
  traceln "Server: received state %s" (IS.to_string state);
  t.state <- state

let read_json flow =
  let buf = Cstruct.create 4 in
  Flow.read_exact flow buf;
  let v = Cstruct.BE.get_uint32 buf 0 in
  traceln "Server: read %lu" v;
  let jbuf = Cstruct.create (Int32.to_int v) in
  Flow.read_exact flow jbuf;
  let params = IP.parse_params (Cstruct.to_string jbuf) in
  traceln "Server: params %a" Yojson.Safe.pp params;
  params

let write_json flow json =
  let buf = Cstruct.of_string @@ Yojson.Safe.to_string json in
  let len = Cstruct.create 4 in
  Cstruct.BE.set_uint32 len 0 (Int32.of_int @@ Cstruct.length buf);
  Eio.Flow.write flow [len; buf]

let diff_times sw ew stms etms =
  let open Unix in
  let timediff = (ew -. sw) *. 1e6 in
  let user = (etms.tms_utime -. stms.tms_utime) *. 1e6 in
  let sys = (etms.tms_stime -. stms.tms_stime) *. 1e6 in
  ((user +. sys) /. timediff *. 100.), (user /. timediff *. 100.), (sys /. timediff *. 100.)

let drain_stream flow len =
  let buf = Cstruct.create len in
  while true do
    Flow.read_exact flow buf
  done

let handle_client ~clock flow _addr =
  traceln "Server: got connection from iperf client";
  let cookie = Cstruct.create IP.cookie_size in
  Eio.Flow.read_exact flow cookie;
  let cookie = Cstruct.to_string cookie in
  (* Check if we have this session already *)
  match Hashtbl.find_opt sessions cookie with
  | None ->
    traceln "Server: new iperf3 session started, cookie %S" cookie;
    Eio.Switch.run @@ fun sw ->
    let cond = Condition.create () in
    let mut = Mutex.create () in
    let nstreams = Atomic.make 0 in
    let incr_streams () =
      Mutex.use_rw ~protect:false mut (fun () -> Atomic.incr nstreams);
      Condition.broadcast cond
    in
    let start_times = Unix.times () in
    let t = { state = IS.TEST_START; cookie; len=0; incr_streams; parallel=0; sw; start_times; start_wallclock=0. } in
    set_state t flow IS.PARAM_EXCHANGE;
    traceln "Server: reading JSON parameters";
    let params = read_json flow in
    let len = IP.params_length params in
    t.len <- len;
    let parallel = IP.params_parallel params in
    t.parallel <- parallel;
    Hashtbl.add sessions cookie t;
    Eio.Switch.run @@ fun sw ->
    t.sw <- sw;
    set_state t flow IS.CREATE_STREAMS;
    traceln "Waiting for streams";
    Mutex.use_ro mut (fun () ->
       while Atomic.get nstreams < parallel do
         Condition.await cond mut
       done
    );
    t.start_times <- Unix.times ();
    t.start_wallclock <- Eio.Time.now clock;
    set_state t flow IS.TEST_START;
    set_state t flow IS.TEST_RUNNING;
    wait_state t flow IS.TEST_END;
    let end_wallclock = Eio.Time.now clock in
    let end_times = Unix.times () in
    let total, user, system = diff_times t.start_wallclock end_wallclock t.start_times end_times in
    set_state t flow IS.EXCHANGE_RESULTS;
    let _ = read_json flow in
    (* just dummy results from server *)
    write_json flow (`Assoc [
      "cpu_util_total", `Float total;
      "cpu_util_user", `Float user;
      "cpu_util_system", `Float system;
      "sender_has_retransmits", `Int (-1);
      "streams", `List []]);
    set_state t flow IS.DISPLAY_RESULTS;
    wait_state t flow IS.IPERF_DONE
  | Some t ->
    traceln "Server: new session on cookie %S" cookie;
    Fiber.fork ~sw:t.sw (fun () -> drain_stream flow t.len);
    t.incr_streams ();
    Fiber.await_cancel ()

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
