
PROJECT    = stacky
EXECUTABLE = $(shell find . -type f -executable -name $(PROJECT))
EXE_ARGS   =

.PHONY: foo
foo:
	@echo $(EXECUTABLE)

all: build

build:
	cabal build

run: build
	$(EXECUTABLE) $(EXE_ARGS)

test:
	cabal test

clean:
	cabal clean

