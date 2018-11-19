.PHONY: build
build:
	dune build

.PHONY: build-beams
build-beams:
	make -C test compile

.PHONY: test-only
test-only:
	dune runtest

.PHONY: test
test: build-beams test-only

.PHONY: install
install:
	dune build @install

.PHONY: clean
clean:
	make -C test clean
	dune clean

.PHONY: docker-publish
docker-publish:
	# NOTE: Execute "docker login" before "make docker-publish"
	docker build -t yutopp/obeam-dev-ocaml:latest .circleci/images/ocaml
	docker push yutopp/obeam-dev-ocaml:latest
