import os
import shutil
import sys
import re

from support import fullpath, is_string


TEMPLATE_EXT = '.tmpl'


def _apply_template_config(content, template_config):
    # Replace all instances matching the format "${key}" with the corresponding
    # value in template_config. The matching pattern is enclosed in
    # double-quotes (") to avoid invalid characters in Ada source code.
    def lookup(match):
        key = match.group(1)
        assert key in template_config, \
            "key '%s' not defined in template configuration" % key
        return str(template_config.get(key))

    return(re.sub(r'\"\$\{([^\$\"\}]*)\}\"', lookup, content))


def _copy(src, dst, template_config=None):
    "Copy (or symlink) src to dst"

    src_is_template = (src.endswith(TEMPLATE_EXT) and
                       template_config is not None)

    # Remove template extension from destination filename, if any
    if dst.endswith(TEMPLATE_EXT):
        dst = dst[:-len(TEMPLATE_EXT)]

    # The run-time sources are encoded in ASCII
    source_encoding = 'us-ascii'

    if not os.path.isfile(src):
        print("runtime file " + src + " does not exists")
        sys.exit(4)

    already_exists = False

    if os.path.isfile(dst) or src_is_template:
        with open(src, 'r', encoding=source_encoding) as fp:
            src_cnt = fp.read()

        # Apply template configuration
        if src_is_template:
            if FilesHolder.verbose:
                print("Apply template config to %s: %s" %
                      (src, str(template_config)))
            src_cnt = _apply_template_config(src_cnt, template_config)

        # Check if destination file already exists with same content
        if os.path.isfile(dst):
            with open(dst, 'r') as fp:
                dst_cnt = fp.read()

            if dst_cnt != src_cnt:
                print("runtime file " + dst + " already exists")
                print("cannot install " + src)
                sys.exit(5)
            else:
                already_exists = True

    if already_exists:
        if FilesHolder.verbose:
            print("same file, skip: " + src + ", " + dst)
    else:
        if FilesHolder.verbose:
            print("copy " + src + " to " + dst)
        if FilesHolder.link and not src_is_template:
            os.symlink(os.path.abspath(src), dst)
        elif src_is_template:
            # Write source from template file
            with open(dst, 'w',
                      newline='',  # Disable newline to os.linesep translation
                      encoding=source_encoding) as fp:
                fp.write(src_cnt)
        else:
            # Copy non templated source file
            shutil.copy(src, dst)


class FilePair(object):
    def __init__(self, dst, src, template_config=None):
        self._dst = dst
        self._template_config = template_config

        # Full path to the source file
        self._src = None

        if '/' not in src:
            # Files without path elements are in gnat
            assert FilesHolder.manifest, "Error: MANIFEST file not found"
            assert src in FilesHolder.manifest, \
                "Error: source file %s not in MANIFEST" % src
            self._src = os.path.join(FilesHolder.gnatdir, src)

        elif src.split('/')[0] in ('hie', 'libgnarl', 'libgnat'):
            # BB-specific file in gnat/hie
            self._src = os.path.join(FilesHolder.gnatdir, src)
            assert os.path.exists(self._src), \
                "Error: source file %s not found in gnat" % src

        else:
            # Look into the current repository
            self._src = fullpath(src)

            if not os.path.exists(self._src):
                # Look into gcc
                self._src = os.path.join(FilesHolder.gccdir, src)

        assert os.path.exists(self._src), \
            "Error: source file '%s' not found" % src

    def __eq__(self, other):
        if is_string(other):
            return self._dst == other
        elif isinstance(other, FilePair):
            return other._dst == self._dst and other._src == self._src
        else:
            return False

    def __str__(self):
        return self._dst

    def install(self, dir):
        _copy(self._src, os.path.join(dir, self._dst), self._template_config)


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
        self._template_config = {}

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

    def add_source_alias(self, dir, dst, src):
        """Add source.

         DIR is the target directory the file will be copied to
         DST is the install basename of the file to copy
         SRC is the full name of the file to copy"""
        if dir not in self.dirs:
            self.dirs[dir] = []
        self.dirs[dir].append(FilePair(dst, src, self._template_config))

    def add_source(self, dir, src):
        """Add source.

         DIR is the target directory the file will be copied to
         SRC is the full name of the file to copy"""
        base = os.path.basename(src)
        # A file could have `.` in its name. (eg: pikeos4.2-cert-app.c)
        _, ext = base.rsplit('.', 1)
        # Check if the basename of the source file contains two consecutive
        # underscores. This is by naming convention a file variant whose
        # variant part needs to be removed before installation.
        # Example:
        # s-textio__myboard.adb should be installed as s-textio.adb
        if '__' in base:
            base, _ = base.split('__')
            base = "%s.%s" % (base, ext)
        self.add_source_alias(dir, base, src)

    def add_sources(self, dir, sources):
        """Add a list of sources.

        DIR is the target directory the files will be copied to
        SOURCES is a list of source files"""
        assert isinstance(sources, list) or isinstance(sources, tuple), \
            "incorrect parameter %s" % str(sources)
        for src in sources:
            self.add_source(dir, src)

    def add_template_config_value(self, key, value):
        """Adds key/value that will be used to instantiate templated source"""

        assert isinstance(key, str) and isinstance(value, str), \
            "template key and value must be strings"

        if key in self._template_config:
            assert False, "config key already defined  %s" % key
        else:
            self._template_config[key] = value

    def has_source(self, name):
        for d in self.dirs:
            if name in self.dirs[d]:
                return True
        return False

    def remove_source(self, name):
        for d in self.dirs:
            if name in self.dirs[d]:
                self.dirs[d].remove(name)
                return
        assert False, "No such source %s" % name

    def update_pair(self, dest, src):
        assert is_string(dest), \
            "dest is not a string: %s (src is %s)" % (str(dest), str(src))
        assert src is None or is_string(src), \
            "src is not a string: %s (dest is %s)" % (str(src), str(dest))

        for d in self.dirs:
            if dest in self.dirs[d]:
                self.dirs[d].remove(dest)
                self.dirs[d].append(FilePair(dest, src, self._template_config))
                return True
        # no such file
        return False

    def install(self, dir):
        installed = []
        for d in self.dirs:
            full = os.path.join(dir, d)
            installed.append(d)

            if not os.path.exists(full):
                os.makedirs(full)
            for f in self.dirs[d]:
                f.install(full)

    def copy_pair(self, dst, srcfile, destdir):
        "Copy after substitution with pairs"

        dstdir = os.path.join(destdir, os.path.basename(dst))

        # Find the sourcedir
        if srcfile is None:
            print("No source file for %s" % dst)
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
            print("Cannot find source dir for %s" % srcfile)
            sys.exit(2)

        _copy(src, dstdir)
