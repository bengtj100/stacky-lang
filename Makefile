
PROJECT    = stacky
EXECUTABLE = $(shell find dist-newstyle -type f -executable -name $(PROJECT))
EXE_ARGS   = 

HASKTAGS   = ~/.cabal/bin/hasktags
HASKTAGS_ARGS = -e .

INST_BIN   = ~/bin

all: build

build: tags
	@echo ">>>>>>>>>>>>    Building executable ..."
	cabal build

run: build
	@echo ">>>>>>>>>>>>    Executing executable ..."
	$(EXECUTABLE) $(EXE_ARGS)

test: build
	@echo ">>>>>>>>>>>>    Running unit tests ..."
	cabal test

clean:
	@echo ">>>>>>>>>>>>    Taking out the trash ..."
	cabal clean
	rm -f TAGS

install: test
	@echo ">>>>>>>>>>>>    Installing to: $(INST_BIN) ..."
	cp $(EXECUTABLE) $(INST_BIN)/

tags:
	@echo ">>>>>>>>>>>>    (Re)generating TAGS file ..."
	$(HASKTAGS) $(HASKTAGS_ARGS)
