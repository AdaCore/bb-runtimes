#
# Copyright (C) 2026, AdaCore
#

DOCDIR = doc

CLI_ARGS ?= --all
USE_SYMLINKS ?= true
SOURCE_SEARCH_PATHS ?=

.PHONY: help build-doc check-doc-requirements show-doc lint test gen-runtimes

help:
	@echo "Available targets:"
	@echo "  build-doc      Build the bb-runtimes HTML documentation"
	@echo "  show-doc       Build then open the documentation in a browser"
	@echo "  lint           Run pre-commit on all files"
	@echo "  test           Run the testsuite through tox"
	@echo "  gen-runtimes   Quick runtime generation for testing purposes under generated/"
	@echo "    CLI_ARGS         CLI arguments (default: --all)"
	@echo "    SOURCE_SEARCH_PATHS Extra source directories, separated by spaces (may be needed)"
	@echo "    USE_SYMLINKS       Use symlinks unless false (default: true)"
	@echo "    Example: make gen-runtimes CLI_ARGS='cortex-m0'"
	@echo "    Copy files: make gen-runtimes CLI_ARGS='cortex-m0' USE_SYMLINKS=false"

build-doc: check-doc-requirements
	$(MAKE) -C $(DOCDIR) html

check-doc-requirements:
	@python3 -m pip install --dry-run --no-deps --no-index \
	    -r $(DOCDIR)/requirements.txt >/dev/null 2>&1 \
	    || { echo "missing doc requirements:" \
	              "pip install -r $(DOCDIR)/requirements.txt" >&2; exit 1; }

show-doc: build-doc
	$(MAKE) -C $(DOCDIR) view_html

lint:
	pre-commit run --all-files

test:
	tox

gen-runtimes:
	PYTHONPATH=$(CURDIR)/support/rts_prebuilder_project:$(CURDIR)/support/bb_runtimes_targets_gen \
	    python3 -m bb_runtimes_targets_gen.targets $(CLI_ARGS) \
	    --output-dir $(CURDIR)/generated --overwrite $(if $(filter false,$(USE_SYMLINKS)),,--link) \
	    --source-search-path $(CURDIR)/src \
	    $(foreach dir,$(SOURCE_SEARCH_PATHS),--source-search-path $(dir))
