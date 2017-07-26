import os
import shutil
import sys

from build_rts_support import fullpath


class FilesHolder(object):
    # Sources directories
    gnatdir = "../gnat"
    gccdir = "../gcc"

    # Display actions
    verbose = False

    link = False

    def __init__(self):
        self.dirs = {}
        self.c_srcs = []
        self.asm_srcs = []

        # Read manifest file (if exists)
        manifest_file = os.path.join(self.gnatdir, "MANIFEST.GNAT")
        self.manifest = []
        if os.path.isfile(manifest_file):
            f = open(manifest_file, 'r')
            for line in f:
                line = line.strip()
                if line and not line.startswith('--'):
                    self.manifest.append(line)

    def add_source(self, dir, dst, src):
        base = os.path.basename(dst)
        if '__' in base:
            # filename with variant: remove the variant part
            base, ext = base.split(".")
            base, variant = base.split("__")
            base = "%s.%s" % (base, ext)
        self.dirs[dir][base] = src
        if dir not in self.c_srcs:
            if dst.endswith('.c') or dst.endswith('.h'):
                self.c_srcs.append(dir)
        if dir not in self.asm_srcs:
            if dst.endswith('.s') or dst.endswith('.S') \
               or dst.endswith('.inc'):
                self.asm_srcs.append(dir)

    def add_sources(self, dir, sources):
        if dir not in self.dirs:
            self.dirs[dir] = {}
        if isinstance(sources, list):
            for src in sources:
                self.add_sources(dir, src)
        elif isinstance(sources, dict):
            for k, v in sources.items():
                self.add_source(dir, k, v)
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
                self.dirs[d][original] = original
                return True
        return False

    def update_pair(self, dest, src):
        assert isinstance(dest, basestring), \
            "dest is not a string: %s (src is %s)" % (dest, src)
        assert src is None or isinstance(src, basestring), \
            "src is not a string: %s (dest is %s)" % (src, dest)

        for d in self.dirs:
            if dest in self.dirs[d]:
                self.dirs[d][dest] = src
                return True
        print 'update_pair: %s not found' % dest
        sys.exit(2)
        # no such file
        return False

    def update_pairs(self, pairs):
        for k, v in pairs.items():
            if not self.update_pair(k, v):
                print "in update_pairs: no such source: %s" % k
        return True

    def _copy(self, src, dst, installed_files):
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
            if self.verbose:
                print "same file, skip: " + src + ", " + dst
        else:
            if self.verbose:
                print "copy " + src + " to " + dst
            if self.link:
                try:
                    os.symlink(os.path.abspath(src), dst)
                except os.error, e:
                    print "symlink error for " + src
                    print "msg: " + str(e)
                    sys.exit(2)
            else:
                shutil.copy(src, dst)

    def _copy_pair(self, dst, srcfile, destdir, installed_files=None):
        "Copy after substitution with pairs"

        dstdir = os.path.join(destdir, os.path.basename(dst))

        # Find the sourcedir
        if srcfile is None:
            print "No source file for %s" % dst
            sys.exit(2)

        if '/' not in srcfile:
            # Files without path elements are in gnat
            assert self.manifest, "Error: MANIFEST file not found"
            assert srcfile in self.manifest, \
                "Error: source file %s not in MANIFEST" % srcfile
            self._copy(os.path.join(self.gnatdir, srcfile),
                       dstdir, installed_files)
        else:
            for d in ('.', self.gccdir):
                src = os.path.join(fullpath(d), srcfile)
                if os.path.exists(src):
                    self._copy(src, dstdir, installed_files)
                    return
            print "Cannot find source dir for %s" % srcfile
            sys.exit(2)
