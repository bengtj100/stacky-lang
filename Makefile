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

PROJECT    = stacky
EXECUTABLE = $(shell find dist-newstyle -type f -executable -name $(PROJECT))
EXE_ARGS   = 

HASKTAGS   = ~/.cabal/bin/hasktags
HASKTAGS_ARGS = -e .

INST_BIN   = ~/bin

VERSION_FILE = Version.hs
VERSION_TEMPLATE = $(VERSION_FILE).template
MAKE_VERSION_FILE = ~/src/build-tools/bin/make-version-file
MAKE_VERSION_ARGS = --template $(VERSION_TEMPLATE) --output $(VERSION_FILE)

all: build

build: tags $(VERSION_FILE)
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
	rm -f TAGS $(VERSION_FILE)

install: version test
	@echo ">>>>>>>>>>>>    Installing to: $(INST_BIN) ..."
	cp $(EXECUTABLE) $(INST_BIN)/

tags:
	@echo ">>>>>>>>>>>>    (Re)generating TAGS file ..."
	$(HASKTAGS) $(HASKTAGS_ARGS)

.PHONY: version
version:
	@echo ">>>>>>>>>>>>    Generating new build version ..."
	$(MAKE_VERSION_FILE) $(MAKE_VERSION_ARGS)

$(VERSION_FILE): $(VERSION_TEMPLATE)
	@echo ">>>>>>>>>>>>    Initializing build versioning ..."
	$(MAKE_VERSION_FILE) $(MAKE_VERSION_ARGS)
