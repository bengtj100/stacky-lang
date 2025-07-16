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

all: build

all__COMMENT__:
	@echo 'Build all components.'

## ----------------------------------------------------------------------------------------------------

build:  tags $(VERSION_FILE_HS) $(VERSION_FILE_SY)
	@echo ">>>>>>>>>>>>    Building executable ..."
	@echo 'Entering directory `'"$(SRC_FULL)'"
	$(CABAL) build
	@echo 'Leaving directory `'"$(SRC_FULL)'"

build__COMMENT__:
	@echo 'Build the application using cabal.'

## ----------------------------------------------------------------------------------------------------

run: build
	@echo ">>>>>>>>>>>>    Running executable '$(shell basename $(EXECUTABLE))'..."
	$(EXECUTABLE) $(EXE_ARGS)

run__COMMENT__:
	@echo 'Run application locally as a process on the dev. machine.'

## ----------------------------------------------------------------------------------------------------

test: build
	@echo ">>>>>>>>>>>>    Running Haskell unit tests ..."
	@$(CABAL) test
	@echo ">>>>>>>>>>>>    Running Stacky unit tests ..."
	@echo 'Entering directory `'$(TEST_FULL)"'"
	@$(EXECUTABLE) $(EXE_ARGS) -b -IA $(TEST_FULL) 'RunTests' -- $(TEST_ARGS)
	@echo 'Leaving directory `'$(TEST_FULL)"'"

test__COMMENT__:
	@echo 'Run unit tests using the test tool based on cabal.'

## ----------------------------------------------------------------------------------------------------

.PHONY: ghci
ghci:
	ghci $(shell find src -maxdepth 1 -type d -exec printf '-i%s ' '{}' +)

ghci__COMMENT__:
	@echo 'Run the application in ghci for debugging.'

## ----------------------------------------------------------------------------------------------------

.PHONY: clean
clean:
	@echo ">>>>>>>>>>>>    Taking out the trash ..."
	$(CABAL) clean
	rm -rf TAGS $(VERSION_FILE_HS) $(VERSION_FILE_SY) $(RELEASES)/ $(PDF_FILES)

clean__COMMENT__:
	@echo 'Clean all build and temporary files.'

## ----------------------------------------------------------------------------------------------------

install: version all test doc
	@echo ">>>>>>>>>>>>    Installing to: $(INST_BIN) ..."
	sudo mkdir -p $(INST_BIN) $(INST_LIB)
	sudo cp $(EXECUTABLE) $(INST_BIN)/
	sudo cp $(BIN)/* $(INST_BIN)/
	sudo cp -pPrv $(PRELUDE)/ $(INST_LIB)/

install__COMMENT__:
	@echo 'Install application to $(INST_BIN)'

## ----------------------------------------------------------------------------------------------------

.PHONY: release
release: version all test doc
	@echo ">>>>>>>>>>>>    Building release tar-ball ..."
	$(BUILD_RELEASE)

release__COMMENT__:
	@echo 'Build a new release tar ball'

## ----------------------------------------------------------------------------------------------------

tags:
	$(MKTAGS) $(MKTAGS_ARGS)

tags__COMMENT__:
	@echo '(Re)generate TAGS file for emacs.'

## ----------------------------------------------------------------------------------------------------

doc: doc_print $(PDF_FILES)

doc__COMMENT__:
	@echo 'Generate documentation files.'

doc_print:
	@echo ">>>>>>>>>>>>    Generating documentation files ..."

.PHONY doc_print__NO_LIST__:
doc_print__NO_LIST__:

%.pdf: %.md
	$(PANDOC) $(PANDOC_ARGS) -o $@ $<
$(DOC)/%.pdf: $(PRELUDE)/%.sy
	$(PANDOC) $(PANDOC_ARGS) -o $@ $<

## ----------------------------------------------------------------------------------------------------

.PHONY: version
version:
	@echo ">>>>>>>>>>>>    Generating new build version ..."
	$(MAKE_VERSION_PATH) $(MAKE_VERSION_ARGS)

version__COMMENT__:
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
