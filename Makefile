
all:
	dune build @install

.PHONY: opt
opt:
	dune build @install --profile release

.PHONY: install
install:
	dune install

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	rm -rf _build
