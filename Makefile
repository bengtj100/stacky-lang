
PROJECT    = stacky
EXECUTABLE = $(shell find . -type f -executable -name $(PROJECT))
EXE_ARGS   =

INST_BIN   = ~/bin

.PHONY: foo
foo:
	@echo $(EXECUTABLE)

all: build

build:
	cabal build

run: build
	$(EXECUTABLE) $(EXE_ARGS)

test: build
	cabal test

clean:
	cabal clean

install: test
	cp $(EXECUTABLE) $(INST_BIN)/
