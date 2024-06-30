## ====================================================================================================
##
## Copyright (c) 2024 Bengt Johansson <bengtj100 at gmail dot com>.
## All rights reserved.
##
## This software is part of the stacky project and its use is
## regulated by the conditions stipulated in the file named 'LICENCE',
## located in the top directory of said project.
##
## ====================================================================================================

SRC = ./src

CABAL = cd $(SRC) && cabal

PROJECT    = stacky
CABAL_FILE = $(SRC)/$(PROJECT).cabal

EXECUTABLE = $(shell $(CABAL) list-bin $(PROJECT))
EXE_ARGS   = --prelude ~/src/stacky/Prelude.sy

HASKTAGS   = cd $(SRC) && ~/.cabal/bin/hasktags
HASKTAGS_ARGS = -e .

INST_BIN   = ~/bin
INST_LIB   = ~/lib

VERSION_FILE       = $(SRC)/Version.hs
VERSION_TEMPLATE   = ./templates/Version.hs.template
MAKE_VERSION_PATH  = ~/src/build-tools/bin/make-version-file
MAKE_VERSION_ARGS  = --cabal $(CABAL_FILE)
MAKE_VERSION_ARGS += --template $(VERSION_TEMPLATE)
MAKE_VERSION_ARGS += --output $(VERSION_FILE)

all: build

build: tags $(VERSION_FILE)
	@echo ">>>>>>>>>>>>    Building executable ..."
	$(CABAL) build

run: build
	@echo ">>>>>>>>>>>>    Running executable '$(shell basename $(EXECUTABLE))'..."
	@$(EXECUTABLE) $(EXE_ARGS)

test: build
	@echo ">>>>>>>>>>>>    Running unit tests ..."
	$(CABAL) test

clean:
	@echo ">>>>>>>>>>>>    Taking out the trash ..."
	$(CABAL) clean
	rm -f TAGS $(VERSION_FILE)

install: version test
	@echo ">>>>>>>>>>>>    Installing to: $(INST_BIN) ..."
	mkdir -p $(INST_BIN) $(INST_LIB)
	cp $(EXECUTABLE) $(INST_BIN)/
	cp ./Prelude.sy $(INST_LIB)/

tags:
	@echo ">>>>>>>>>>>>    (Re)generating TAGS file ..."
	$(HASKTAGS) $(HASKTAGS_ARGS)

.PHONY: version
version:
	@echo ">>>>>>>>>>>>    Generating new build version ..."
	$(MAKE_VERSION_PATH) $(MAKE_VERSION_ARGS)

$(VERSION_FILE): $(VERSION_TEMPLATE) $(CABAL_FILE)
	@echo ">>>>>>>>>>>>    Initializing build versioning ..."
	$(MAKE_VERSION_PATH) $(MAKE_VERSION_ARGS)
