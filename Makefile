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
BIN		   = ./bin
PRELUDE            = ./prelude
RELEASES           = ./releases
TOOLS              = ./tools
DOC                = ./doc
PDF		   = ./pdf
TEST               = ./test

SRC_FULL           = $(shell readlink -f $(SRC))
TEST_FULL          = $(shell readlink -f $(TEST))
TEST_ARGS          = foo bar "cicada 3301"
DOC_FILES          = $(wildcard $(DOC)/*.md)

PDF_FILES	   = $(DOC_FILES:.md=.pdf)
PDF_FILES	  += $(DOC)/Prelude.pdf

PANDOC             = pandoc
PANDOC_ARGS        = -f markdown -t pdf

CABAL              = cd $(SRC_FULL) && ~/.cabal/bin/cabal

PROJECT            = stacky
CABAL_FILE         = $(SRC)/$(PROJECT).cabal

EXECUTABLE         = $(shell $(TOOLS)/get-executable)
EXE_ARGS           = -IA $(PRELUDE)

MKTAGS             = $(TOOLS)/mktags
MKTAGS_ARGS        = $(SRC) -e .

INST_BIN           = /usr/local/bin
INST_LIB           = /usr/local/lib/$(PROJECT)

VERSION_FILE_HS    = $(SRC)/main/Version.hs
VERSION_FILE_SY    = $(PRELUDE)/Version.sy
MAKE_VERSION_PATH  = ./tools/make-version-file

MAKE_VERSION_ARGS  = $(CABAL_FILE) $(VERSION_FILE_HS) $(VERSION_FILE_SY)

BUILD_RELEASE      = $(TOOLS)/build-release

## ====================================================================================================

## Include misc.mk for the use build warning
include ~/src/build-tools/makefiles/common/misc.mk

## ----------------------------------------------------------------------------------------------------

build:  tags $(VERSION_FILE_HS) $(VERSION_FILE_SY)
	@echo ">>>>>>>>>>>>    Building executable ..."
	@echo 'Entering directory `'"$(SRC_FULL)'"
	$(CABAL) build
	@echo 'Leaving directory `'"$(SRC_FULL)'"

__COMMENT__build:
	@echo 'Build the application using cabal.'

## ----------------------------------------------------------------------------------------------------

run: build
	@echo ">>>>>>>>>>>>    Running executable '$(shell basename $(EXECUTABLE))'..."
	$(EXECUTABLE) $(EXE_ARGS)

__COMMENT__run:
	@echo 'Run application locally as a process on the dev. machine.'

## ----------------------------------------------------------------------------------------------------

test: build
	@echo ">>>>>>>>>>>>    Running Haskell unit tests ..."
	@$(CABAL) test
	@echo ">>>>>>>>>>>>    Running Stacky unit tests ..."
	@echo 'Entering directory `'$(TEST_FULL)"'"
	@$(EXECUTABLE) $(EXE_ARGS) -b -IA $(TEST_FULL) 'RunTests' -- $(TEST_ARGS)
	@echo 'Leaving directory `'$(TEST_FULL)"'"

__COMMENT__test:
	@echo 'Run unit tests using the test tool based on cabal.'

## ----------------------------------------------------------------------------------------------------

.PHONY: ghci
ghci:
	ghci $(shell find src -maxdepth 1 -type d -exec printf '-i%s ' '{}' +)

__COMMENT__ghci:
	@echo 'Run the application in ghci for debugging.'

## ----------------------------------------------------------------------------------------------------

.PHONY: clean
clean:
	@echo ">>>>>>>>>>>>    Taking out the trash ..."
	$(CABAL) clean
	rm -rf TAGS $(VERSION_FILE_HS) $(VERSION_FILE_SY) $(RELEASES)/ $(PDF_FILES)

__COMMENT__clean:
	@echo 'Clean all build and temporary files.'

## ----------------------------------------------------------------------------------------------------

install: version test doc
	@echo ">>>>>>>>>>>>    Installing to: $(INST_BIN) ..."
	sudo mkdir -p $(INST_BIN) $(INST_LIB)
	sudo cp $(EXECUTABLE) $(INST_BIN)/
	sudo cp $(BIN)/* $(INST_BIN)/
	sudo cp -pPrv $(PRELUDE)/ $(INST_LIB)/

__COMMENT__install:
	@echo 'Install application to $(INST_BIN)'
	@echo 'This requires sudo rights during the install'
	@echo 'DO NOT ISSUE THIS COMMAND AS ROOT!'

## ----------------------------------------------------------------------------------------------------

.PHONY: release
release: version test doc
	@echo ">>>>>>>>>>>>    Building release tar-ball ..."
	$(BUILD_RELEASE)

__COMMENT__release:
	@echo 'Build a new release tar ball'

## ----------------------------------------------------------------------------------------------------

doc: doc_print $(PDF_FILES)

__COMMENT__doc:
	@echo 'Generate documentation files.'

doc_print:
	@echo ">>>>>>>>>>>>    Generating documentation files ..."

.PHONY __NO_LIST__doc_print:
__NO_LIST__doc_print:

%.pdf: %.md
	$(PANDOC) $(PANDOC_ARGS) -o $@ $<
$(DOC)/%.pdf: $(PRELUDE)/%.sy
	$(PANDOC) $(PANDOC_ARGS) -o $@ $<

## ----------------------------------------------------------------------------------------------------

.PHONY: version
version:
	@echo ">>>>>>>>>>>>    Generating new build version ..."
	$(MAKE_VERSION_PATH) $(MAKE_VERSION_ARGS)

__COMMENT__version:
	@echo 'Generate a new version tag.'
	@echo 'Use command: $(MAKE_VERSION_PATH)'

## ----------------------------------------------------------------------------------------------------

$(VERSION_FILE_HS): $(VERSION_TEMPLATE) $(CABAL_FILE)
	@echo ">>>>>>>>>>>>    Initializing build versioning for Haskell ..."
	$(MAKE_VERSION_PATH) $(MAKE_VERSION_ARGS)

$(VERSION_FILE_SY): $(VERSION_TEMPLATE) $(CABAL_FILE)
	@echo ">>>>>>>>>>>>    Initializing build versioning for Stacky..."
	$(MAKE_VERSION_PATH) $(MAKE_VERSION_ARGS)

## ====================================================================================================

## Include TAGS
TAGS_LANG=haskell
include ~/src/build-tools/makefiles/common/tags.mk
