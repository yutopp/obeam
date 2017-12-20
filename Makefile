.PHONY: build example test install clean

build:
	jbuilder build

test:
	jbuilder runtest

install:
	jbuilder build @install

clean:
	jbuilder clean
