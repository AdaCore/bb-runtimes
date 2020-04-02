import shutil
import os
import os.path

from support.files_holder import FilesHolder
from support import fullpath


def docgen(boards, target, dest):
    if not os.path.exists(dest):
        os.makedirs(dest)

    # Check gnat version number and variant if possible
    gnatdir = fullpath(FilesHolder.gnatdir)

    # Copy support files
    PWD = os.path.dirname(__file__)
    src = os.path.join(PWD, 'data', 'doc')
    for base in ('Makefile', 'reconfigurablerts.rst'):
        srcfile = os.path.join(src, base)
        shutil.copy(srcfile, dest)
    conf = os.path.join(src, 'conf.py')
    with open(conf, 'r') as fp:
        cnt = fp.read()
    cnt = cnt.replace('@target@', target)
    conf = os.path.join(dest, 'conf.py')
    with open(conf, 'w') as fp:
        fp.write(cnt)
    gnatvsn = os.path.join(gnatdir, 'gnatvsn.ads')
    if os.path.exists(gnatvsn):
        shutil.copy(gnatvsn, dest)

    readmes = {}
    files = {}
    runtimes = {}
    for board in boards:
        if board.readme_file is not None:
            if board.readme_file not in files:
                files[board.readme_file] = board.name
            readmes[board.name] = board.readme_file
        if board.name not in runtimes:
            runtimes[board.name] = board.runtimes

    overall_readme = os.path.join(dest, "index.rst")
    with open(overall_readme, "w") as fp:
        title = "GNAT for %s run-times documentation" % target
        fp.write("%s\n" % ("=" * len(title)))
        fp.write("%s\n" % title)
        fp.write("%s\n\n" % ("=" * len(title)))
        fp.write(".. only:: not latex\n\n")
        fp.write("   .. only:: PRO\n\n")
        fp.write("      *GNAT Pro Edition*\n\n")
        fp.write("   .. only:: GPL\n\n")
        fp.write("      *GNAT GPL Edition*\n\n")
        fp.write("   | Version |version|\n")
        fp.write("   | Date: |today|\n\n")
        fp.write("   .. contents:: Table of Contents\n")
        fp.write("      :depth: 2\n\n")

        # List of runtimes:

        if target is not None:
            title = "Run-times available with the %s compiler" % target
        else:
            title = "Run-times available in this package"
        fp.write("%s\n" % title)
        fp.write("%s\n\n" % ("=" * len(title)))
        if target is not None:
            fp.write(("The %s compiler comes with"
                      " the following run-times:\n\n") % target)
        else:
            fp.write(("This package adds support for"
                      " the following run-times:\n\n"))
        for board in sorted(runtimes.keys()):
            fp.write("* %s" % board)
            if board in readmes.keys():
                ref = files[readmes[board]]
                fp.write(" (see :ref:`%s`)" % ref)
            fp.write("\n\n")
            for rts in sorted(runtimes[board]):
                fp.write('  - %s\n' % rts)
            fp.write("\n\n")

        # General description of runtime location, usage, and rebuild procedure

        fp.write(".. include:: reconfigurablerts.rst\n\n")

        # Detailed description of the BSP

        for f in sorted(files):
            fname = "bsp-%s.rst" % files[f]
            fp.write(".. _%s:\n" % files[f])
            fp.write(".. include:: %s\n\n" % fname)
            shutil.copy(fullpath(f), os.path.join(dest, "%s" % fname))
        fp.write("\n")
        print("documentation successfully generated in %s" % dest)
