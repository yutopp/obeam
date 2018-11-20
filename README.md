OBEAM
-
**WIP**
obeam (御-BEAM) is a utility library for parsing BEAM format(and Erlang External Term Format, etc) which is written in OCaml.

### Requirements
```
# `ppx_bitstring` will require `time` command which is not a builtin function
opam install ppx_bitstring ppx_deriving camlzip
```

### OPAM
#### Pin
```
opam pin add obeam .
opam install obeam.0.0.3
```

### Run Example
```
make test
erlc test/test01.erl
_build/default/example/read_beam.exe test01.beam
```

### Authors

- [@yutopp](https://github.com/yutopp)
- [@amutake](https://github.com/amutake)

obeam has been greatly improved by [many contributors](https://github.com/yutopp/obeam/graphs/contributors)!
