.PHONY: build example test install clean

build:
	dune build

test:
	dune runtest

install:
	dune build @install

clean:
	dune clean
