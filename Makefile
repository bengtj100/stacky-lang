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

SRC                = ./src
PRELUDE            = ./prelude
RELEASES           = ./releases
TOOLS              = ./tools
DOC                = ./doc
PDF		   = ./pdf
TEST               = ./test

SRC_FULL           = $(shell readlink -f $(SRC))
TEST_FULL          = $(shell readlink -f $(TEST))

DOC_FILES          = $(wildcard $(DOC)/*.md)

PDF_FILES	   = $(DOC_FILES:.md=.pdf)
PDF_FILES	  += $(DOC)/Prelude.pdf

CABAL              = cd $(SRC_FULL) && ~/.cabal/bin/cabal

PROJECT            = stacky
CABAL_FILE         = $(SRC)/$(PROJECT).cabal

EXECUTABLE         = $(shell $(CABAL) list-bin $(PROJECT))
EXE_ARGS           = -IA $(PRELUDE)

HASKTAGS           = cd $(SRC) && ~/.cabal/bin/hasktags
HASKTAGS_ARGS      = -e .

INST_BIN           = /usr/local/bin
INST_LIB           = /usr/local/lib/$(PROJECT)

VERSION_FILE_HS    = $(SRC)/main/Version.hs
VERSION_FILE_SY    = $(PRELUDE)/Version.sy
MAKE_VERSION_PATH  = ./tools/make-version-file

MAKE_VERSION_ARGS  = $(CABAL_FILE) $(VERSION_FILE_HS) $(VERSION_FILE_SY)

BUILD_RELEASE      = $(TOOLS)/build-release

## ====================================================================================================

all: build

## ----------------------------------------------------------------------------------------------------

build:  tags $(VERSION_FILE_HS) $(VERSION_FILE_SY)
	@echo ">>>>>>>>>>>>    Building executable ..."
	@echo 'Entering directory `'"$(SRC_FULL)'"
	$(CABAL) build
	@echo 'Leaving directory `'"$(SRC_FULL)'"

## ----------------------------------------------------------------------------------------------------

run: build
	@echo ">>>>>>>>>>>>    Running executable '$(shell basename $(EXECUTABLE))'..."
	$(EXECUTABLE) $(EXE_ARGS)

## ----------------------------------------------------------------------------------------------------

test: build
	@echo ">>>>>>>>>>>>    Running Haskell unit tests ..."
	@$(CABAL) test
	@echo ">>>>>>>>>>>>    Running Stacky unit tests ..."
	@echo 'Entering directory `'$(TEST_FULL)"'"
	@$(EXECUTABLE) $(EXE_ARGS) -b -IA $(TEST_FULL) 'RunTests'
	@echo 'Leaving directory `'$(TEST_FULL)"'"

## ----------------------------------------------------------------------------------------------------

.PHONY: ghciInput/output
ghci:
	ghci $(shell find src -maxdepth 1 -type d -exec printf '-i%s ' '{}' +)

## ----------------------------------------------------------------------------------------------------

.PHONY: clean
clean:
	@echo ">>>>>>>>>>>>    Taking out the trash ..."
	$(CABAL) clean
	rm -rf TAGS $(VERSION_FILE_HS) $(VERSION_FILE_SY) $(RELEASES)/ $(PDF_FILES)

## ----------------------------------------------------------------------------------------------------

install: version all test doc
	@echo ">>>>>>>>>>>>    Installing to: $(INST_BIN) ..."
	sudo mkdir -p $(INST_BIN) $(INST_LIB)
	sudo cp $(EXECUTABLE) $(INST_BIN)/
	sudo cp -pPrv $(PRELUDE)/ $(INST_LIB)/

## ----------------------------------------------------------------------------------------------------

.PHONY: release
release: version all test doc
	@echo ">>>>>>>>>>>>    Building release tar-ball ..."
	$(BUILD_RELEASE)

## ----------------------------------------------------------------------------------------------------

tags:
	@echo ">>>>>>>>>>>>    (Re)generating TAGS file ..."
	$(HASKTAGS) $(HASKTAGS_ARGS)

## ----------------------------------------------------------------------------------------------------

doc: doc_print $(PDF_FILES)

doc_print:
	@echo ">>>>>>>>>>>>    Generating documentation files ..."

%.pdf: %.md
	pandoc -f markdown -t pdf -o $@ $<
$(DOC)/%.pdf: $(PRELUDE)/%.sy
	pandoc -f markdown -t pdf -o $@ $<

## ----------------------------------------------------------------------------------------------------

.PHONY: version
version:
	@echo ">>>>>>>>>>>>    Generating new build version ..."
	$(MAKE_VERSION_PATH) $(MAKE_VERSION_ARGS)

## ----------------------------------------------------------------------------------------------------

$(VERSION_FILE_HS): $(VERSION_TEMPLATE) $(CABAL_FILE)
	@echo ">>>>>>>>>>>>    Initializing build versioning for Haskell ..."
	$(MAKE_VERSION_PATH) $(MAKE_VERSION_ARGS)

$(VERSION_FILE_SY): $(VERSION_TEMPLATE) $(CABAL_FILE)
	@echo ">>>>>>>>>>>>    Initializing build versioning for Stacky..."
	$(MAKE_VERSION_PATH) $(MAKE_VERSION_ARGS)

## ====================================================================================================
