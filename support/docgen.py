import shutil
import os
import os.path
import re
import datetime
from files_holder import FilesHolder

from . import fullpath


def docgen(boards, target, dest):
    if not os.path.exists(dest):
        os.makedirs(dest)

    # Check gnat version number and variant if possible
    gnatdir = fullpath(FilesHolder.gnatdir)
    if os.path.exists(gnatdir):
        gnatvsn = os.path.join(gnatdir, 'gnatvsn.ads')
        if not os.path.exists(gnatvsn):
            gnatvsn = None
    today = datetime.date.today()
    gnat_version = "%4i.%2i%2i" % (today.year, today.month, today.day)
    gnat_variant = "GPL"
    if gnatvsn is not None:
        with open(gnatvsn, 'r') as fp:
            cnt = fp.read()
        for line in cnt.splitlines():
            line = line.strip()
            match = re.match(
                r'Gnat_Static_Version_String.*"([0-9.w]*) .*', line)
            if match is not None:
                gnat_version = match.group(1)
            elif line.startswith('Build_Type'):
                if 'Gnatpro' in line:
                    gnat_variant = 'Pro'
                elif 'FSF' in line:
                    gnat_variant = 'FSF'
                else:
                    gnat_variant = 'GPL'

    # Copy support files
    PWD = os.path.dirname(__file__)
    src = os.path.join(PWD, 'data', 'doc')
    for base in ('Makefile', 'reconfigurablerts.rst'):
        srcfile = os.path.join(src, base)
        shutil.copy(srcfile, dest)
    conf = os.path.join(src, 'conf.py')
    with open(conf, 'r') as fp:
        cnt = fp.read()
    if target is not None:
        cnt = cnt.replace('@target@', 'for %s' % target)
    else:
        cnt = cnt.replace(' @target@', '')
    cnt = cnt.replace(
        '@gnat_variant@', gnat_variant).replace(
        '@gnat_version@', gnat_version).replace(
        '@year@', '%4i' % today.year)
    conf = os.path.join(dest, 'conf.py')
    with open(conf, 'w') as fp:
        fp.write(cnt)

    readmes = {}
    files = {}
    runtimes = {}
    for board in boards:
        if board.readme_file is not None:
            if board.readme_file not in files:
                files[board.readme_file] = board.name
            readmes[board.name] = board.readme_file
        if board.name not in runtimes:
            runtimes[board.name] = []
        for profile in board.runtimes:
            runtimes[board.name].append(profile)

    overall_readme = os.path.join(dest, "index.rst")
    with open(overall_readme, "w") as fp:
        title = "GNAT %s Bare Metal Runtimes documentation" % gnat_variant
        fp.write("%s\n" % ("=" * len(title)))
        fp.write("%s\n" % title)
        fp.write("%s\n\n" % ("=" * len(title)))
        fp.write(".. contents:: Table of Contents\n")
        fp.write("   :depth: 2\n\n")

        # List of runtimes:

        if target is not None:
            title = "Runtimes available with the %s compiler" % target
        else:
            title = "Runtimes available in this package"
        fp.write("%s\n" % title)
        fp.write("%s\n\n" % ("=" * len(title)))
        if target is not None:
            fp.write(("The %s compiler comes with"
                      " the following runtimes:\n\n") % target)
        else:
            fp.write(("This package adds support for"
                      " the following runtimes:\n\n"))
        for board in sorted(runtimes.keys()):
            fp.write("* %s" % board)
            if board in readmes.keys():
                target = files[readmes[board]]
                fp.write(" (see :ref:`%s`)" % target)
            fp.write("\n\n")
            for rts in sorted(runtimes[board]):
                fp.write('  - %s\n' % rts)
            fp.write("\n")

        # General description of runtime location, usage, and rebuild procedure

        fp.write(".. include:: reconfigurablerts.rst\n\n")

        # Detailed description of the BSP

        for f in sorted(files):
            fname = "bsp-%s.rst" % files[f]
            fp.write(".. _%s:\n" % files[f])
            fp.write(".. include:: %s\n\n" % fname)
            shutil.copy(fullpath(f), os.path.join(dest, "%s" % fname))
        fp.write("\n")
