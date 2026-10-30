#
# Copyright (C) 2025-2026, AdaCore
#

# Configuration file for the Sphinx documentation builder.

from pathlib import Path
import sys

#  Ahead of any installed copy: autodoc must document this checkout, not
#  whatever version the environment happens to have on its path.
repo_root = Path(__file__).resolve().parents[1]
sys.path[:0] = [
    str(repo_root / "support" / "rts_prebuilder_project"),
    str(repo_root / "support" / "bb_runtimes_targets_gen"),
]

# -- Project information -----------------------------------------------------
project = "bb-runtimes"
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
html_theme_options = {
    # Keep child pages (the extender reference) visible in the sidebar instead
    # of only when their parent is the current page.
    "collapse_navigation": False,
    "navigation_depth": 3,
}
html_static_path = ["_static"]
html_css_files = ["custom.css"]

# -- Mermaid -----------------------------------------------------------------
# Tracks the newest release on the CDN instead of the version the extension
# happens to pin.
mermaid_version = "latest"

# securityLevel "loose" is what makes the `click` directives in the routing
# diagram work; the default drops them. wrappingWidth caps a label's width, so
# long labels break into lines instead of running out of their box.
mermaid_init_js = """mermaid.initialize({
    startOnLoad: true,
    securityLevel: 'loose',
    flowchart: {useMaxWidth: true, wrappingWidth: 260},
    themeVariables: {fontFamily: 'Lato, Helvetica, Arial, sans-serif'},
});"""

# -- Extension configuration -------------------------------------------------
intersphinx_mapping = {
    "python": ("https://docs.python.org/3", None),
}
# Autosummary settings
autosummary_generate = True

# Signatures show the bare name. The module is already the section heading.
add_module_names = False
