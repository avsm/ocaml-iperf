## ocaml-iperf: support the IPerfv3 protocol

[Iperf v3](https://github.com/esnet/iperf) is a TCP network bandwidth
measurement tool.  This repository implements the V3 server protocol, so
that the existing Iperf client can be pointed at an OCaml server and
benchmark it.

There is an Eio-based server implementation in the `iperf-eio` package
that can be used as follows.

On the server:
```
$ dune exec -- iperfv3-server
```

On the client:
```
# install iperfv3
$ iperf -c 127.0.0.1
```

There are a number of options for the iperf client; not all are implemented
in the server, but parallel connections are.  PRs to implement other pieces
are welcome.
