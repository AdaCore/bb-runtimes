# Style_Check:Python_Fragment

import os
import re
import sys
import time

sys.path.append('.')

root_source_dir = os.path.dirname(os.path.abspath(__file__))
gnatvsn_spec = os.path.join(root_source_dir, 'gnatvsn.ads')

try:
    with open(gnatvsn_spec, 'r') as fd:
        gnatvsn_content = fd.read()
except Exception:
    print('cannot find gnatvsn.ads')
    sys.exit(1)


def get_copyright():
    return u'2008-%s, AdaCore' % time.strftime('%Y')


def get_gnat_version():
    m = re.search(r'Gnat_Static_Version_String : ' +
                  r'constant String := "([^\(\)]+)\(.*\)?";',
                  gnatvsn_content)
    if m:
        return m.group(1).strip()

    print('cannot find GNAT version in gnatvsn.ads')
    sys.exit(1)


def get_gnat_build_type():
    m = re.search(r'Build_Type : constant Gnat_Build_Type := (.+);',
                  gnatvsn_content)
    if m:
        return {'Gnatpro': 'PRO',
                'FSF': 'FSF',
                'GPL': 'GPL'}[m.group(1).strip()]
    else:
        print('cannot compute GNAT build type')
        sys.exit(1)


# Exclude sources that are not part of the current documentation
exclude_patterns = []
for d in os.listdir(root_source_dir):
    if d not in ('share', 'index.rst'):
        exclude_patterns.append(d)

extensions = []
templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'index'

# General information about the project
project = u'GNAT %s for @target@ run-times' % get_gnat_build_type()

copyright = get_copyright()

version = get_gnat_version()
release = get_gnat_version()

pygments_style = 'sphinx'
tags.add(get_gnat_build_type())
html_theme = 'sphinxdoc'
if os.path.isfile('adacore_transparent.png'):
    html_logo = 'adacore_transparent.png'
if os.path.isfile('favicon.ico'):
    html_favicon = 'favicon.ico'

html_static_path = ['_static']

if os.path.isfile('gnat.sty'):
    latex_additional_files = ['gnat.sty']

latex_documents = [
    (master_doc, 'gnat_runtimes.tex', project, u'AdaCore', 'howto')]

try:
    import latex_elements

    copyright_macros = {
        'date': time.strftime("%b %d, %Y"),
        'edition': 'GNAT %s Edition' % (
            'Pro' if get_gnat_build_type() == 'PRO'
            else 'GPL'),
        'name': u'GNU Ada',
        'tool': u'GNAT',
        'version': version,
        'build_type': get_gnat_build_type()}

    # Experimenting with the two forms of LateX documents available in Sphinx
    # that is 'manual' and 'howto'.
    if latex_documents[0][4] == 'manual':
        latex_elements = {
            'preamble': '\\usepackage{gnat}\n' +
            latex_elements.TOC_DEPTH +
            latex_elements.PAGE_BLANK +
            latex_elements.TOC_CMD +
            latex_elements.LATEX_HYPHEN +
            latex_elements.doc_settings(project,
                                        get_gnat_version()),
            'tableofcontents': latex_elements.TOC % copyright_macros}
    else:
        latex_elements = {
            'preamble': '\\usepackage{gnat}\n' +
            latex_elements.TOC_DEPTH +
            latex_elements.PAGE_BLANK +
            latex_elements.doc_settings(project,
                                        get_gnat_version())}
except Exception:
    pass
