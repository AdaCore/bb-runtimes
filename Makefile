#
# Copyright (C) 2026, AdaCore
#

DOCDIR = doc

.PHONY: help build-doc check-doc-requirements show-doc lint test

help:
	@echo "Available targets:"
	@echo "  build-doc  Build the bb-runtimes HTML documentation"
	@echo "  show-doc   Build then open the documentation in a browser"
	@echo "  lint       Run pre-commit on all files"
	@echo "  test       Run the testsuite through tox"

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
