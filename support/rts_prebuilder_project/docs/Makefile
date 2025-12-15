#
# Copyright (C) 2025-2026, AdaCore
#

# Minimal makefile for Sphinx documentation

SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = .
BUILDDIR      = _build

help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)"

.PHONY: help Makefile view

# View the built documentation in a browser
view_html:
	@if [ -f "$(BUILDDIR)/html/index.html" ]; then \
		echo "Opening documentation in browser..."; \
		if command -v xdg-open >/dev/null 2>&1; then \
			xdg-open $(BUILDDIR)/html/index.html; \
		elif command -v open >/dev/null 2>&1; then \
			open $(BUILDDIR)/html/index.html; \
		elif command -v firefox >/dev/null 2>&1; then \
			firefox $(BUILDDIR)/html/index.html & \
		else \
			echo "Please open: $$(pwd)/$(BUILDDIR)/html/index.html"; \
		fi; \
	else \
		echo "Documentation not built. Run 'make html' first."; \
		exit 1; \
	fi

%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(O)
