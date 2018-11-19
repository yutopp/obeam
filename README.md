# OBEAM

[![CircleCI](https://circleci.com/gh/yutopp/obeam.svg?style=svg)](https://circleci.com/gh/yutopp/obeam)

**WIP**
obeam (御-BEAM) is a utility library for parsing BEAM format(and Erlang External Term Format, etc) which is written in OCaml.

## Installation
### Using opam pin

```
opam pin add obeam .
```

## Run examples

```
make test
erlc test/test01.erl
_build/default/example/read_beam.exe test01.beam
```
