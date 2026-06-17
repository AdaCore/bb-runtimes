#
# Copyright (C) 2025-2026, AdaCore
#

# Configuration file for the Sphinx documentation builder.

import os
import sys

# Docs are now at rts_prebuilder/docs, so we need to go up two levels
# to reach the project root
sys.path.insert(0, os.path.abspath("../.."))

# -- Project information -----------------------------------------------------
project = "RTS Prebuilder"
project_copyright = "2025, AdaCore"
author = "AdaCore"

# -- General configuration ---------------------------------------------------
extensions = [
    "sphinx.ext.autodoc",  # To include docstrings from code
    "sphinx.ext.viewcode",  # To add links to source code
    "sphinx.ext.autosummary",  # To generate summary tables
    "sphinxcontrib.mermaid",  # To support Mermaid diagrams
]

exclude_patterns = ["_build", "Thumbs.db", ".DS_Store"]

# -- Options for HTML output -------------------------------------------------
html_theme = "sphinx_rtd_theme"
html_static_path = ["_static"]
html_css_files = ["custom.css"]

# -- Extension configuration -------------------------------------------------
intersphinx_mapping = {
    "python": ("https://docs.python.org/3", None),
}
# Autosummary settings
autosummary_generate = True
