module IS = Iperf.V3.State
module IP = Iperf.V3.Proto
open Printf

type t = {
  flow: Unix.file_descr;
  mutable conns: Unix.file_descr list;
  mutable cookie: string;
  mutable state: IS.t;
  mutable len: int;
  incr_streams: unit -> unit;
  mutable parallel: int;
  mutable start_times: Unix.process_times;
  mutable start_wallclock: float;
}

let listen_fd () =
  let open Unix in
  let s = socket PF_INET SOCK_STREAM 0 in
  setsockopt s SO_REUSEADDR true;
  bind s (ADDR_INET ((inet_addr_of_string "127.0.0.1"), 5201));
  listen s 10;
  s

let sessions = Hashtbl.create 7

let jid = ref 0
let get_jid () = incr jid; !jid

let is_valid_cookie cookie =
  String.length cookie = IP.cookie_size

let read_exactly fd len =
  let buf = Bytes.create len in
  let rec fn fd buf off len =
    let n = Unix.read fd buf off len in
    if n = len then () else fn fd buf (off+n) (len-n) in
  fn fd buf 0 len;
  buf

let set_state t fd state =
  eprintf "Server: state %s -> %s\n%!" (IS.to_string t.state) (IS.to_string state);
  t.state <- state;
  let b = IP.wire_value_of_state state |> Cstruct.to_bytes in
  match Unix.write fd (IP.wire_value_of_state state |> Cstruct.to_bytes) 0 (Bytes.length b) with
  | 0 | (-1) -> failwith "set_state"
  | n when n = Bytes.length b -> ()
  | _ -> failwith "state_state/partial"

let wait_state t fd state =
  eprintf "Server: waiting for state message %s\n%!" (IS.to_string state);
  let buf = read_exactly fd 1 in
  let s = Cstruct.of_bytes buf |> IP.state_of_wire_value in
  if s <> state then
    raise (Failure "expected wait_state received");
  eprintf "Server: received state %s\n%!" (IS.to_string state);
  t.state <- state

let expect_state t buf state =
  eprintf "Server: expecting state message %s\n%!" (IS.to_string state);
  let s = IP.state_of_wire_value buf in
  if s <> state then
    raise (Failure "expected wait_state received");
  eprintf "Server: received state %s\n%!" (IS.to_string state);
  t.state <- state

let read_json fd =
  let buf = read_exactly fd 4 in
  eprintf "r %S\n%!" (Bytes.to_string buf);
  let v = Cstruct.of_bytes buf |> fun b -> Cstruct.BE.get_uint32 b 0 in
  eprintf "Server: read %lu\n%!" v;
  let buf = read_exactly fd (Int32.to_int v) in
  let params = IP.parse_params (Bytes.to_string buf) in
  Format.eprintf "Server: params %a\n%!" Yojson.Safe.pp params;
  params

let write_json fd json =
  let buf = Cstruct.of_string @@ Yojson.Safe.to_string json in
  let len = Cstruct.create 4 in
  Cstruct.BE.set_uint32 len 0 (Int32.of_int @@ Cstruct.length buf);
  let buf = Cstruct.concat [len; buf] in
  match Unix.write fd (Cstruct.to_bytes buf) 0 (Cstruct.length buf) with
  | 0 | (-1) -> failwith "write_json"
  | n when n = Cstruct.length buf -> eprintf "wrote: %S\n%!" (Cstruct.to_string buf);
  | n -> failwith "write_json: short"

let diff_times sw ew stms etms =
  let open Unix in
  let timediff = (ew -. sw) *. 1e6 in
  let user = (etms.tms_utime -. stms.tms_utime) *. 1e6 in
  let sys = (etms.tms_stime -. stms.tms_stime) *. 1e6 in
  ((user +. sys) /. timediff *. 100.), (user /. timediff *. 100.), (sys /. timediff *. 100.)

let complete_tests t =
  let end_wallclock = Unix.gettimeofday () in
  let end_times = Unix.times () in
  let total, user, system =
    diff_times t.start_wallclock end_wallclock t.start_times end_times in
    set_state t t.flow IS.EXCHANGE_RESULTS;
  let _ = read_json t.flow in
  (* just dummy results from server *)
  write_json t.flow (`Assoc [
    "cpu_util_total", `Float total;
    "cpu_util_user", `Float user;
    "cpu_util_system", `Float system;
    "sender_has_retransmits", `Int (-1);
    "streams", `List []]);
  set_state t t.flow IS.DISPLAY_RESULTS;
  wait_state t t.flow IS.IPERF_DONE
 
let reqs = Hashtbl.create 7
let push_one_read uring region fd =
  let id = get_jid () in
  let chunk = Uring.Region.alloc region in
  match Uring.read_chunk uring ~file_offset:Optint.Int63.zero fd chunk id with
  | None -> failwith "push_one_read"
  | Some j -> Hashtbl.add reqs id (`IO (id,chunk,fd))

let push_control_read uring region fd =
  let id = get_jid () in
  let buf = Cstruct.create 1 in
  match Uring.read uring ~file_offset:Optint.Int63.zero fd buf id with
  | None -> failwith "push_control_read"
  | Some j -> Hashtbl.add reqs id (`Control buf)

let push_reads t uring region =
  push_control_read uring region t.flow;
  List.iter (fun fd -> push_one_read uring region fd) t.conns;
  let _ = Uring.submit uring in
  let wait_nonblock () = Uring.get_cqe_nonblocking uring in
  let wait_block () = Uring.wait uring in
  let rec fn (wait: unit -> int Uring.completion_option) =
    match wait () with
    | None -> fn wait_block
    | Some { result; data } -> begin
        match Hashtbl.find_opt reqs data with
        | Some (`IO (id,chunk,fd)) ->
            push_one_read uring region fd;
            let _ = Uring.submit uring in
            Hashtbl.remove reqs id;
            Uring.Region.free chunk; 
            fn wait_nonblock
        | Some (`Control buf) ->
            expect_state t buf IS.TEST_END;
            complete_tests t
        | None -> failwith "uring: unknown id"
    end
  in fn wait_nonblock

let start_test t =
  eprintf "Server: starting test\n%!";
  let block_size = 4096 in
  let slots = 64 in
  let uring = Uring.create ~queue_depth:64 () in
  let buf = Cstruct.create (block_size * slots) in
  let region = Uring.Region.init ~block_size buf.Cstruct.buffer slots in
  t.start_times <- Unix.times ();
  t.start_wallclock <- Unix.gettimeofday ();
  set_state t t.flow IS.TEST_START;
  set_state t t.flow IS.TEST_RUNNING;
  (* start uring reading - a finished read on the control channel will be TEST_END *)
  match Uring.set_fixed_buffer uring buf.Cstruct.buffer with
  | Error (`ENOMEM) -> failwith "ENOMEM"
  | Ok () -> push_reads t uring region

let handle_client flow _addr =
  eprintf "Server: got connection from iperf client\n%!";
  let cookie = read_exactly flow IP.cookie_size |> Bytes.to_string in
  (* Check if we have this session already *)
  match Hashtbl.find_opt sessions cookie with
  | None ->
    eprintf "Server: new iperf3 session started, cookie %S\n%!" cookie;
    let nstreams = Atomic.make 0 in
    let incr_streams () =
      Atomic.incr nstreams;
    in
    let start_times = Unix.times () in
    let t = { flow; conns=[]; state = IS.TEST_START; cookie; len=0;
      incr_streams; parallel=0; start_times;
      start_wallclock=0. } in
    set_state t flow IS.PARAM_EXCHANGE;
    eprintf "Server: reading JSON parameters\n%!";
    let params = read_json flow in
    let len = IP.params_length params in
    t.len <- len;
    let parallel = IP.params_parallel params in
    t.parallel <- parallel;
    Hashtbl.add sessions cookie t;
    set_state t flow IS.CREATE_STREAMS;
    eprintf "Waiting for streams\n%!";
  | Some t ->
    eprintf "Server: new session on cookie %S\n%!" cookie;
    t.incr_streams ();
    t.conns <- flow :: t.conns;
    if List.length t.conns = t.parallel then
      start_test t

let () =
  let lfd = listen_fd () in
  let rec fn () =
    let fd, sa = Unix.accept lfd in
    handle_client fd sa;
    fn ()
  in
  fn ()

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
