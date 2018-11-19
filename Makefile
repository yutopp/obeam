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
docker-publish: docker-publish-ocaml docker-publish-erlang-otp18
	# NOTE: Execute "docker login" before "make docker-publish"

.PHONY: docker-publish-ocaml
docker-publish-ocaml:
	docker build -t yutopp/obeam-dev-ocaml:latest .circleci/images/ocaml
	docker push yutopp/obeam-dev-ocaml:latest

.PHONY: docker-publish-erlang-otp18
docker-publish-erlang-otp18:
	docker build -t yutopp/obeam-dev-erlang:otp-18 .circleci/images/erlang/ \
		-f .circleci/images/erlang/Dockerfile.otp-18
	docker push yutopp/obeam-dev-erlang:otp-18
