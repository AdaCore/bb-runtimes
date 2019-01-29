import os
import re
import shutil
import sys

from support import fullpath

class CopyFileInstaller(object):
    def install(self, src, dst, installed_files, target):
        "Copy (or symlink) src to dst"

        if not os.path.isfile(src):
            print "runtime file " + src + " does not exists"
            sys.exit(4)

        already_exists = False

        if os.path.isfile(dst):
            with open(dst, 'r') as fp:
                cnt1 = fp.read()
            with open(src, 'r') as fp:
                cnt2 = fp.read()
            if cnt1 != cnt2:
                print "runtime file " + dst + " already exists"
                print "cannot install " + src
                sys.exit(5)
            else:
                already_exists = True

        if installed_files is not None:
            if os.path.basename(dst) in installed_files:
                print "runtime file " + dst + " installed multiple times"
                sys.exit(6)

            installed_files.append(os.path.basename(dst))

        if already_exists:
            if target.verbose:
                print "same file, skip: " + src + ", " + dst
        else:
            if target.verbose:
                print "copy " + src + " to " + dst
            if target.link:
                try:
                    os.symlink(os.path.abspath(src), dst)
                except os.error, e:
                    print "symlink error for " + src
                    print "msg: " + str(e)
                    sys.exit(2)
            else:
                shutil.copy(src, dst)

    def install_pair(self, dst, srcfile, destdir, installed_files, target):
        "Copy after substitution with pairs"

        dstdir = os.path.join(destdir, os.path.basename(dst))

        # Find the sourcedir
        if srcfile is None:
            print "No source file for %s" % dst
            sys.exit(2)

        # Full path to the source file
        src = None

        if '/' not in srcfile:
            # Files without path elements are in gnat
            assert FilesHolder.manifest, "Error: MANIFEST file not found"
            assert srcfile in FilesHolder.manifest, \
                "Error: source file %s not in MANIFEST" % srcfile
            src = os.path.join(self.gnatdir, srcfile)

        elif srcfile.split('/')[0] in ('hie', 'libgnarl', 'libgnat'):
            # BB-specific file in gnat/hie
            src = os.path.join(self.gnatdir, srcfile)
            assert os.path.exists(src), \
                "Error: source file %s not found in gnat" % srcfile

        else:
            # Look into the current repository
            src = fullpath(srcfile)

            if not os.path.exists(src):
                # Look into gcc
                src = os.path.join(self.gccdir, srcfile)

        if src is None or not os.path.exists(src):
            print "Cannot find source dir for %s" % srcfile
            sys.exit(2)
            sys.exit(2)

        self.install(src, dstdir, installed_files, target)

class TemplateFileInstaller(CopyFileInstaller):
    def __init__(self, vars):
        self._vars = vars

    def install(self, src, dst, installed_files, target):
        "Copy src to dst, replacing instances of ${var} in src"

        if not os.path.isfile(src):
            print "runtime file " + src + " does not exists"
            sys.exit(4)

        already_exists = False

        with open(src, 'r') as fp:
            src_contents = fp.read()

        # Replace all instances matching the format: ${var} with the
        # corresponding value in self.vars, using "var" as the key.
        # E.g. given: self.vars = {'key1', 'hello'}
        # then all instances of "${key1}" in the source file are replaced
        # with "hello" in the destination file.
        # Variables in the ${var} format, but without a corresponding
        # key in self.vars are replaced with an empty string.
        def lookup(match):
            return str(self._vars.get(match.group(1)))
        contents = re.sub(r'\$\{([^\}]*)\}', lookup, src_contents)

        if os.path.isfile(dst):
            with open(dst, 'r') as fp:
                dst_contents = fp.read()

            if dst_contents != contents:
                print "runtime file " + dst + " already exists"
                print "cannot install " + src
                sys.exit(5)
            else:
                already_exists = True

        if installed_files is not None:
            if os.path.basename(dst) in installed_files:
                print "runtime file " + dst + " installed multiple times"
                sys.exit(6)

            installed_files.append(os.path.basename(dst))

        if already_exists:
            if target.verbose:
                print "same file, skip: " + src + ", " + dst
        else:
            with open(dst, 'w') as fp:
                fp.write(contents)

class FilesHolder(object):
    # Sources directories
    gnatdir = "../gnat"
    gccdir = "../gcc"

    # Gnat MANIFEST file
    manifest = None

    # Display actions
    verbose = False

    link = False

    _gcc_version = None

    @staticmethod
    def gcc_version():
        if FilesHolder._gcc_version is None:
            base_ver = os.path.join(FilesHolder.gccdir, 'gcc', 'BASE-VER')
            with open(base_ver, 'r') as fp:
                for line in fp:
                    line = line.strip()
                    if len(line) > 0:
                        FilesHolder._gcc_version = line
                        break
        return FilesHolder._gcc_version

    def __init__(self):
        self.dirs = {}
        self.c_srcs = []
        self.asm_srcs = []
        self.asm_cpp_srcs = []

        # Read manifest file (if exists)
        if FilesHolder.manifest is None:
            manifest_file = os.path.join(self.gnatdir, "MANIFEST.GNAT")
            FilesHolder.manifest = []
            if os.path.isfile(manifest_file):
                f = open(manifest_file, 'r')
                for line in f:
                    line = line.strip()
                    if line and not line.startswith('--'):
                        FilesHolder.manifest.append(line)

    def add_source(self, dir, dst, src, installer=CopyFileInstaller()):
        base = os.path.basename(dst)
        # A file could have `.` in its name. (eg: pikeos4.2-cert-app.c)
        _, ext = base.rsplit('.', 1)
        if '__' in base:
            # File with variant:
            # remove the variant part from the destination file name
            base, _ = base.split('__')
            base = "%s.%s" % (base, ext)
        self.dirs[dir][base] = (src, installer)
        if dir not in self.c_srcs:
            if ext in ('c', 'h'):
                self.c_srcs.append(dir)
        if dir not in self.asm_srcs:
            if ext == 's':
                self.asm_srcs.append(dir)
        if dir not in self.asm_cpp_srcs:
            if ext in ('S', 'inc'):
                self.asm_cpp_srcs.append(dir)

    def add_sources(self, dir, sources, installer=CopyFileInstaller()):
        if dir not in self.dirs:
            self.dirs[dir] = {}
        if isinstance(sources, list):
            for src in sources:
                self.add_sources(dir, src, installer)
        elif isinstance(sources, dict):
            for k, v in sources.items():
                self.add_source(dir, k, v, installer)
        else:
            self.add_source(dir, sources, sources)

    def has_source(self, name):
        for d in self.dirs:
            if name in self.dirs[d]:
                return True
        return False

    def remove_source(self, name):
        for d in self.dirs:
            if name in self.dirs[d]:
                del(self.dirs[d][name])
                return
        print "No such source %s" % name
        sys.exit(2)

    def remove_pair(self, original):
        for d in self.dirs:
            if original in self.dirs[d]:
                self.dirs[d][original] = (original, None)
                return True
        return False

    def update_pair(self, dest, src, installer=CopyFileInstaller()):
        assert isinstance(dest, basestring), \
            "dest is not a string: %s (src is %s)" % (dest, src)
        assert src is None or isinstance(src, basestring), \
            "src is not a string: %s (dest is %s)" % (src, dest)

        for d in self.dirs:
            if dest in self.dirs[d]:
                self.dirs[d][dest] = (src, installer)
                return True
        print 'update_pair: %s not found' % dest
        sys.exit(2)
        # no such file
        return False

    def update_pairs(self, pairs, installer=CopyFileInstaller()):
        for k, v in pairs.items():
            if not self.update_pair(k, v, installer):
                print "in update_pairs: no such source: %s" % k
        return True
