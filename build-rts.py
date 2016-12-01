#! /usr/bin/env python
#
# Copyright (C) 2016, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

import copy
import getopt
import sys
import os
import shutil
import string
import re

# Sources directories
gnatdir = "../gnat"
gccdir = "../gcc"
crossdir = "../cross/bare_board/libbareboard"
shared_sources = "rts-sources"

# Output directory
objdir = "install"

# Display actions
verbose = False

# create a common directory
create_common = False

link = False
config = ""


def fullpath(filename):
    """Returns the full path of filename.

    If filename is a relative path, then returns the path relative to the
    script's directory.
    """
    if os.path.isabs(filename):
        return filename
    else:
        return os.path.abspath(
            os.path.join(os.path.dirname(__file__), filename))


def readfile(filename):
    """Reads the content of filename, relative to the bb-runtimes directory"""
    fp = open(fullpath(filename), 'r')
    res = fp.read()
    fp.close()
    return res


class FilesHolder(object):
    def __init__(self):
        self.dirs = {}
        self.c_srcs = []
        self.asm_srcs = []

        # Read manifest file (if exists)
        manifest_file = os.path.join(gnatdir, "MANIFEST.GNAT")
        self.manifest = []
        if os.path.isfile(manifest_file):
            f = open(manifest_file, 'r')
            for line in f:
                line = line.strip()
                if line and not line.startswith('--'):
                    self.manifest.append(line)

    def add_source(self, dir, dst, src):
        self.dirs[dir][dst] = src
        if dir not in self.c_srcs:
            if dst.endswith('.c') or dst.endswith('.h'):
                self.c_srcs.append(dir)
        if dir not in self.asm_srcs:
            if dst.endswith('.s') or dst.endswith('.S'):
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
        if not isinstance(dest, basestring):
            print "dest is not a string: %s (src is %s)" % (dest, src)
            sys.exit(2)
        if src is not None and not isinstance(src, basestring):
            print "src is not a string: %s (dest is %s)" % (src, dest)
            sys.exit(2)

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
        if os.path.isfile(dst):
            print "runtime file " + dst + " already exists"
            sys.exit(5)
        if os.path.basename(dst) in installed_files:
            print "runtime file " + dst + " installed multiple times"
            sys.exit(6)

        installed_files.append(os.path.basename(dst))

        if verbose:
            print "copy " + src + " to " + dst
        if link:
            try:
                os.symlink(os.path.abspath(src), dst)
            except os.error, e:
                print "symlink error for " + src
                print "msg: " + str(e)
                sys.exit(2)
        else:
            shutil.copy(src, dst)

    def _copy_pair(self, dst, srcfile, destdir, installed_files):
        "Copy after substitution with pairs"

        dstdir = os.path.join(destdir, os.path.basename(dst))

        # Find the sourcedir
        if srcfile is None:
            print "No source file for %s" % dst
            sys.exit(2)

        if '/' not in srcfile:
            # Files without path elements are in gnat
            if self.manifest and srcfile not in self.manifest:
                print "Error: source file %s not in MANIFEST" % srcfile
                sys.exit(2)
            self._copy(os.path.join(gnatdir, srcfile), dstdir, installed_files)
        else:
            for d in (os.path.dirname(__file__), gccdir, crossdir):
                src = os.path.join(d, srcfile)
                if os.path.exists(src):
                    self._copy(src, dstdir, installed_files)
                    return
            print "Cannot find source dir for %s" % srcfile
            sys.exit(2)


# Definitions of shared source files.
# Keep spec and body on one line.

class SourceDirs(FilesHolder):
    def __init__(self, is_bb):
        super(SourceDirs, self).__init__()
        self._is_bb = is_bb
        self._init_zfp()
        self._init_sfp()
        self._init_full()

    def update_pairs(self, dir, pairs):
        # overload the parent method: this one allows updating pairs in a
        # specific directory. This is needed in the shared sources case as
        # it is expected to have different version of the same source in
        # different sub-directories
        for k, v in pairs.items():
            if k not in self.dirs[dir]:
                print "in update_pairs: no such source: %s" % k
            else:
                self.dirs[dir][k] = v
        return True

    def install(self, dirname, destination, installed_files):
        if dirname not in self.dirs:
            print('undefined shared directory %s' % dirname)

        if create_common:
            # Change dirs to ../rts-sources/<dirname>
            rel = os.path.join('..', shared_sources, dirname)
        else:
            rel = dirname

        destdir = os.path.join(destination, rel)

        if not os.path.exists(destdir):
            os.makedirs(destdir)

            for k, v in self.dirs[dirname].items():
                self._copy_pair(dst=k, srcfile=v, destdir=destdir,
                                installed_files=installed_files)
        else:
            for k, v in self.dirs[dirname].items():
                installed_files.append(os.path.basename(k))

        return rel

    def _init_zfp(self):
        """Files common to all runtimes"""
        # files there no matter what
        self.add_sources('common', [
            'a-assert.ads', 'a-assert.adb',
            {'a-textio.ads': 'a-textio-zfp.ads'},
            'a-unccon.ads',
            'a-uncdea.ads',
            'ada.ads',
            {'g-io.ads': 'g-io-zfp.ads'},
            {'g-io.adb': 'g-io-zfp.adb'},
            {'g-io-put.adb': 'g-io-put-stextio.adb'},
            'g-souinf.ads',
            'gnat.ads',
            'i-cexten.ads',
            'interfac.ads',
            'machcode.ads',
            's-assert.ads',
            's-atacco.ads', 's-atacco.adb',
            's-imgboo.ads', 's-imgboo.adb',
            's-imgint.ads', 's-imgint.adb',
            's-imglli.ads', 's-imglli.adb',
            's-imgllu.ads', 's-imgllu.adb',
            's-imguns.ads', 's-imguns.adb',
            's-maccod.ads',
            's-macres.ads',
            {'s-secsta.ads': 's-secsta-zfp.ads'},
            {'s-secsta.adb': 's-secsta-zfp.adb'},
            's-stoele.ads', 's-stoele.adb',
            {'s-textio.ads': 's-textio-zfp.ads'},
            's-unstyp.ads',
            'text_io.ads',
            'unchconv.ads',
            'unchdeal.ads'])

        if self._is_bb:
            self.add_sources('common', [
                {'a-textio.adb': 'a-textio-zfp.adb'},
                's-bb.ads'])
        else:
            self.add_sources('zfp_io', {'a-textio.adb': 'a-textio-zfp.adb'})
            self.add_sources('sfp_io', {'a-textio.adb': 'a-textio-raven.adb'})

        # FPU support sources
        self.add_sources('fpu', [
            's-fatflt.ads',
            's-fatgen.ads', 's-fatgen.adb',
            's-fatlfl.ads',
            's-fatllf.ads',
            's-fatsfl.ads'])

        # Memory support sources
        self.add_sources('mem', [
            's-memcom.ads', 's-memcom.adb',
            {'s-memcop.ads': 's-memcop-zfp.ads',
             's-memcop.adb': 's-memcop-zfp.adb'},
            's-memmov.ads', 's-memmov.adb',
            's-memset.ads', 's-memset.adb'])

        # Libc implementation
        self.add_sources('libc', {
            's-c.ads': 's-c-zfp.ads',
            's-cerrno.ads': 's-cerrno-zfp.ads',
            's-cerrno.adb': 's-cerrno-zfp.adb',
            's-cmallo.ads': 's-cmallo-zfp.ads',
            's-cmallo.adb': 's-cmallo-zfp.adb',
            's-cstrle.ads': 's-cstrle-zfp.ads',
            's-cstrle.adb': 's-cstrle-zfp.adb'})

        # Newlib support
        self.add_sources('newlib', [
            'newlib-bb.c'])

        # Math support
        self.add_sources('math', [
            'a-ncelfu.ads',
            'a-ngcefu.ads', 'a-ngcefu.adb',
            'a-ngcoar.ads', 'a-ngcoar.adb',
            'a-ngcoty.ads', 'a-ngcoty.adb',
            'a-ngelfu.ads', 'a-ngelfu.adb',
            'a-ngrear.ads', 'a-ngrear.adb',
            'a-nlcefu.ads',
            'a-nlcoty.ads',
            'a-nlelfu.ads',
            'a-nllcef.ads',
            'a-nllcty.ads',
            'a-nllefu.ads',
            'a-nscefu.ads',
            'a-nscoty.ads',
            'a-nselfu.ads',
            'a-nucoty.ads',
            'a-nuelfu.ads',
            'a-numaux.ads',
            'a-numeri.ads',
            'a-nurear.ads',
            's-exnllf.ads', 's-exnllf.adb',
            's-gcmain.ads', 's-gcmain.adb',
            's-gearop.ads', 's-gearop.adb',
            's-libdou.ads', 's-libdou.adb',
            's-libm.ads', 's-libm.adb',
            's-libpre.ads',
            's-libsin.ads', 's-libsin.adb',
            's-lidosq.ads',
            's-lisisq.ads'])
        self.update_pairs('math', {
            'a-ngcoty.adb': 'a-ngcoty-ada.adb',
            'a-ngelfu.ads': 'a-ngelfu-ada.ads',
            'a-ngelfu.adb': 'a-ngelfu-ada.adb',
            'a-nlelfu.ads': 'a-nlelfu-ada.ads',
            'a-nuelfu.ads': 'a-nuelfu-ada.ads',
            'a-numaux.ads': 'a-numaux-ada.ads',
            's-gcmain.ads': 's-gcmain-ada.ads',
            's-gcmain.adb': 's-gcmain-ada.adb',
            's-libdou.ads': 's-libdou-ada.ads',
            's-libdou.adb': 's-libdou-ada.adb',
            's-libm.ads': 's-libm-ada.ads',
            's-libm.adb': 's-libm-ada.adb',
            's-libpre.ads': 's-libpre-ada.ads',
            's-libsin.ads': 's-libsin-ada.ads',
            's-libsin.adb': 's-libsin-ada.adb',
            's-lidosq.ads': 's-lidosq-ada.ads',
            's-lisisq.ads': 's-lisisq-ada.ads'})
        if self._is_bb:
            self.add_sources('math/softsp', {
                's-lisisq.adb': 's-lisisq-ada.adb'})
            self.add_sources('math/hardsp', {
                's-lisisq.adb': 's-lisisq-fpu.adb'})
            self.add_sources('math/softdp', {
                's-lidosq.adb': 's-lidosq-ada.adb'})
            self.add_sources('math/harddp', {
                's-lidosq.adb': 's-lidosq-fpu.adb'})
        else:
            self.add_sources('math', {
                's-lisisq.adb': 's-lisisq-fpu.adb',
                's-lidosq.adb': 's-lidosq-fpu.adb'})

        # Finally, the ZFP & SFP-specific libgnat files
        self.add_sources('zfp', {
            'a-elchha.ads': 'a-elchha-zfp.ads',
            'a-elchha.adb': 'a-elchha-zfp.adb',
            'a-except.ads': 'a-except-zfp.ads',
            'a-except.adb': 'a-except-zfp.adb',
            'a-tags.ads': 'a-tags-hie.ads',
            'a-tags.adb': 'a-tags-hie.adb',
            'i-c.ads': 'i-c-hie.ads',
            's-assert.adb': 's-assert-xi.adb',
            's-memory.ads': 's-memory-zfp.ads',
            's-sssita.ads': 's-sssita-xi.ads',
            's-sssita.adb': 's-sssita-xi.adb'})
        if self._is_bb:
            self.add_sources('zfp', {
                's-memory.adb': 's-memory-zfp.adb'})
        else:
            self.add_sources('zfp', {
                's-memory.adb': 's-memory-raven-min.adb'})

    def _init_sfp(self):
        """ravenscar-sfp files"""
        # libgnarl sources common to sfp/full
        self.add_sources('gnarl/common', [
            'a-interr.ads', 'a-interr.adb',
            'a-reatim.ads', 'a-reatim.adb',
            'a-retide.ads', 'a-retide.adb',
            'a-sytaco.ads', 'a-sytaco.adb',
            'a-taside.ads', 'a-taside.adb',
            'a-taster.ads', 'a-taster.adb',
            's-interr.ads',
            's-mufalo.ads', 's-mufalo.adb',
            's-multip.ads', 's-multip.adb',
            's-musplo.ads',
            's-parame.adb',
            's-taprob.ads', 's-taprob.adb',
            's-taprop.ads', 's-taprop.adb',
            's-tarest.ads', 's-tarest.adb',
            's-tasdeb.ads', 's-tasdeb.adb',
            's-tasinf.ads', 's-tasinf.adb',
            's-taskin.adb',
            's-taspri.ads',
            's-tasres.ads',
            's-tpobmu.ads', 's-tpobmu.adb'])
        self.update_pairs('gnarl/common', {
            'a-interr.adb': 'a-interr-raven.adb',
            'a-reatim.ads': 'a-reatim-xi.ads',
            'a-reatim.adb': 'a-reatim-xi.adb',
            'a-retide.adb': 'a-retide-raven.adb',
            'a-sytaco.ads': 'a-sytaco-xi.ads',
            'a-sytaco.adb': 'a-sytaco-xi.adb',
            'a-taside.adb': 'a-taside-raven.adb',
            'a-taster.ads': 'a-taster-raven.ads',
            'a-taster.adb': 'a-taster-raven.adb',
            's-interr.ads': 's-interr-raven.ads',
            's-parame.adb': 's-parame-xi.adb',
            's-taprob.adb': 's-taprob-raven.adb',
            's-taprob.ads': 's-taprob-raven.ads',
            's-taprop.ads': 's-taprop-xi.ads',
            's-tarest.adb': 's-tarest-raven.adb',
            's-tasdeb.adb': 's-tasdeb-raven.adb',
            's-tasdeb.ads': 's-tasdeb-xi.ads',
            's-taskin.adb': 's-taskin-raven.adb',
            's-taspri.ads': 's-taspri-xi.ads'})
        if self._is_bb:
            # BB case
            self.add_sources('gnarl/common', [
                'a-exetim.ads', 'a-exetim.adb',
                'a-extiin.ads', 'a-extiin.adb',
                'a-rttiev.ads', 'a-rttiev.adb',
                's-bbbosu.ads',
                's-bbexti.ads', 's-bbexti.adb',
                's-bbinte.ads',
                's-bbprot.ads', 's-bbprot.adb',
                's-bbthqu.ads', 's-bbthqu.adb',
                's-bbthre.ads', 's-bbthre.adb',
                's-bbtiev.ads', 's-bbtiev.adb',
                's-bbtime.ads',
                's-bcprmu.ads', 's-bcprmu.adb',
                's-interr.adb',
                's-osinte.ads'])
            self.update_pairs('gnarl/common', {
                'a-exetim.ads': 'a-exetim-bb.ads',
                'a-exetim.adb': 'a-exetim-bb.adb',
                'a-extiin.ads': 'a-extiin-bb.ads',
                'a-extiin.adb': 'a-extiin-bb.adb',
                'a-rttiev.ads': 'a-rttiev-bb.ads',
                'a-rttiev.adb': 'a-rttiev-bb.adb',
                's-interr.adb': 's-interr-xi.adb',
                's-multip.ads': 's-multip-bb.ads',
                's-multip.adb': 's-multip-bb.adb',
                's-osinte.ads': 's-osinte-bb.ads',
                's-taprop.adb': 's-taprop-xi.adb',
                's-tpobmu.adb': 's-tpobmu-bb.adb'})
        else:
            # PikeOS case
            self.update_pairs('gnarl/common', {
                's-multip.ads': 's-multip-raven-default.ads',
                's-multip.adb': 's-multip-raven-default.adb',
                's-taprop.adb': 's-taprop-pikeos.adb'})
            self.add_sources('gnarl/pikeos3', {
                's-interr.adb': 's-interr-pikeos.adb',
                's-osinte.ads': 's-osinte-pikeos.ads',
                's-osinte.adb': 's-osinte-pikeos.adb'})
            self.add_sources('gnarl/pikeos4', {
                's-interr.adb': 's-interr-pikeos4.adb',
                's-osinte.ads': 's-osinte-pikeos4.ads',
                's-osinte.adb': 's-osinte-pikeos4.adb'})

        # SFP-specific files
        self.add_sources('gnarl/sfp', {
            's-taskin.ads': 's-taskin-raven.ads',
            's-tposen.adb': 's-tposen-raven.adb',
            's-tposen.ads': 's-tposen-raven.ads'})

        # timer support
        self.add_sources('gnarl/timer32', 's-bbtime.adb')
        self.add_sources('gnarl/timer64', {'s-bbtime.adb': 's-bbtime-ppc.adb'})

        # spinlock support (leon workaround)
        if self._is_bb:
            self.add_sources('gnarl/spinlock/gcc', 's-musplo.adb')
            self.add_sources('gnarl/spinlock/leon',
                             {'s-musplo.adb': 's-musplo-leon.adb'})
        else:
            self.add_sources('gnarl/common', 's-musplo.adb')

        # memory profile
        if self._is_bb:
            self.add_sources('gnarl/mem-small',
                             {'s-parame.ads': 's-parame-xi-small.ads'})
            self.add_sources('gnarl/mem-large',
                             {'s-parame.ads': 's-parame-xi.ads'})
        else:
            # PikeOS:
            self.add_sources('gnarl/common',
                             {'s-parame.ads': 's-parame-xi.ads'})

    def _init_full(self):
        """ravenscar-full files"""

        # libgnat files for the full profile
        self.add_sources('full', [
            'a-chahan.ads', 'a-chahan.adb',
            'a-charac.ads',
            'a-chlat1.ads',
            'a-chlat9.ads',
            'a-cwila1.ads',
            'a-cwila9.ads',
            'a-decima.ads', 'a-decima.adb',
            'a-einuoc.ads', 'a-einuoc.adb',
            'a-elchha.ads', 'a-elchha.adb',
            'a-excach.adb',
            'a-except.ads', 'a-except.adb',
            'a-excpol.adb',
            'a-exctra.ads', 'a-exctra.adb',
            'a-exexda.adb',
            'a-exexpr.adb',
            'a-exextr.adb',
            'a-exstat.adb',
            'a-finali.ads', 'a-finali.adb',
            'a-ioexce.ads',
            'a-nudira.ads', 'a-nudira.adb',
            'a-nuflra.ads', 'a-nuflra.adb',
            'a-stmaco.ads',
            'a-storio.ads', 'a-storio.adb',
            'a-strbou.ads', 'a-strbou.adb',
            'a-stream.ads', 'a-stream.adb',
            'a-strfix.ads', 'a-strfix.adb',
            'a-string.ads',
            'a-strmap.ads', 'a-strmap.adb',
            'a-strsea.ads', 'a-strsea.adb',
            'a-strsup.ads', 'a-strsup.adb',
            'a-strunb.ads', 'a-strunb.adb',
            'a-stunau.ads', 'a-stunau.adb',
            'a-stwibo.ads', 'a-stwibo.adb',
            'a-stwifi.ads', 'a-stwifi.adb',
            'a-stwima.ads', 'a-stwima.adb',
            'a-stwise.ads', 'a-stwise.adb',
            'a-stwisu.ads', 'a-stwisu.adb',
            'a-stwiun.ads', 'a-stwiun.adb',
            'a-swmwco.ads',
            'a-tags.ads', 'a-tags.adb',
            'a-undesu.ads', 'a-undesu.adb',
            'g-arrspl.ads', 'g-arrspl.adb',
            'g-bubsor.ads', 'g-bubsor.adb',
            'g-busora.ads', 'g-busora.adb',
            'g-busorg.ads', 'g-busorg.adb',
            'g-bytswa.ads', 'g-bytswa.adb',
            'g-casuti.ads', 'g-casuti.adb',
            'g-comver.ads', 'g-comver.adb',
            'g-crc32.ads', 'g-crc32.adb',
            'g-debuti.ads', 'g-debuti.adb',
            'g-except.ads',
            'g-heasor.ads', 'g-heasor.adb',
            'g-hesora.ads', 'g-hesora.adb',
            'g-hesorg.ads', 'g-hesorg.adb',
            'g-htable.ads', 'g-htable.adb',
            'g-md5.ads', 'g-md5.adb',
            'g-moreex.ads', 'g-moreex.adb',
            'g-regexp.ads',
            'g-sechas.ads', 'g-sechas.adb',
            'g-sehamd.ads', 'g-sehamd.adb',
            'g-sehash.ads', 'g-sehash.adb',
            'g-sha1.ads', 'g-sha1.adb',
            'g-sha224.ads',
            'g-sha256.ads',
            'g-sha384.ads',
            'g-sha512.ads',
            'g-shsh32.ads', 'g-shsh32.adb',
            'g-shsh64.ads', 'g-shsh64.adb',
            'g-shshco.ads', 'g-shshco.adb',
            'g-string.ads',
            'g-strspl.ads',
            'g-table.ads', 'g-table.adb',
            'g-tasloc.ads',
            'g-wistsp.ads',
            'i-c.ads', 'i-c.adb',
            'i-cobol.ads', 'i-cobol.adb',
            'i-cpoint.ads', 'i-cpoint.adb',
            'i-cstrin.ads', 'i-cstrin.adb',
            'i-fortra.ads', 'i-fortra.adb',
            'i-pacdec.ads', 'i-pacdec.adb',
            'ioexcept.ads',
            'raise-gcc.c',
            'raise.h',
            's-addima.ads', 's-addima.adb',
            's-addope.ads', 's-addope.adb',
            's-arit64.ads', 's-arit64.adb',
            's-assert.adb',
            's-bitops.ads', 's-bitops.adb',
            's-boarop.ads',
            's-bytswa.ads',
            's-carsi8.ads', 's-carsi8.adb',
            's-carun8.ads', 's-carun8.adb',
            's-casi16.ads', 's-casi16.adb',
            's-casi32.ads', 's-casi32.adb',
            's-casi64.ads', 's-casi64.adb',
            's-casuti.ads', 's-casuti.adb',
            's-caun16.ads', 's-caun16.adb',
            's-caun32.ads', 's-caun32.adb',
            's-caun64.ads', 's-caun64.adb',
            's-chepoo.ads',
            's-crc32.ads', 's-crc32.adb',
            's-excdeb.ads', 's-excdeb.adb',
            's-except.ads', 's-except.adb',
            's-exctab.ads', 's-exctab.adb',
            's-exnint.ads', 's-exnint.adb',
            's-exnlli.ads', 's-exnlli.adb',
            's-expint.ads', 's-expint.adb',
            's-explli.ads', 's-explli.adb',
            's-expllu.ads', 's-expllu.adb',
            's-expmod.ads', 's-expmod.adb',
            's-expuns.ads', 's-expuns.adb',
            's-finmas.ads', 's-finmas.adb',
            's-finroo.ads', 's-finroo.adb',
            's-flocon.ads', 's-flocon.adb',
            's-fore.ads', 's-fore.adb',
            's-geveop.ads', 's-geveop.adb',
            's-htable.ads', 's-htable.adb',
            's-imenne.ads', 's-imenne.adb',
            's-imgbiu.ads', 's-imgbiu.adb',
            's-imgcha.adb', 's-imgcha.ads',
            's-imgdec.adb', 's-imgdec.ads',
            's-imgenu.ads', 's-imgenu.adb',
            's-imgllb.ads', 's-imgllb.adb',
            's-imglld.ads', 's-imglld.adb',
            's-imgllw.ads', 's-imgllw.adb',
            's-imgrea.ads', 's-imgrea.adb',
            's-imgwch.ads', 's-imgwch.adb',
            's-imgwiu.ads', 's-imgwiu.adb',
            's-init.ads', 's-init.adb',
            's-io.ads', 's-io.adb',
            's-mantis.ads', 's-mantis.adb',
            's-mastop.ads', 's-mastop.adb',
            's-memory.ads', 's-memory.adb',
            's-pack03.ads', 's-pack03.adb',
            's-pack05.ads', 's-pack05.adb',
            's-pack06.ads', 's-pack06.adb',
            's-pack07.ads', 's-pack07.adb',
            's-pack09.ads', 's-pack09.adb',
            's-pack10.ads', 's-pack10.adb',
            's-pack11.ads', 's-pack11.adb',
            's-pack12.ads', 's-pack12.adb',
            's-pack13.ads', 's-pack13.adb',
            's-pack14.ads', 's-pack14.adb',
            's-pack15.ads', 's-pack15.adb',
            's-pack17.ads', 's-pack17.adb',
            's-pack18.ads', 's-pack18.adb',
            's-pack19.ads', 's-pack19.adb',
            's-pack20.ads', 's-pack20.adb',
            's-pack21.ads', 's-pack21.adb',
            's-pack22.ads', 's-pack22.adb',
            's-pack23.ads', 's-pack23.adb',
            's-pack24.ads', 's-pack24.adb',
            's-pack25.ads', 's-pack25.adb',
            's-pack26.ads', 's-pack26.adb',
            's-pack27.ads', 's-pack27.adb',
            's-pack28.ads', 's-pack28.adb',
            's-pack29.ads', 's-pack29.adb',
            's-pack30.ads', 's-pack30.adb',
            's-pack31.ads', 's-pack31.adb',
            's-pack33.ads', 's-pack33.adb',
            's-pack34.ads', 's-pack34.adb',
            's-pack35.ads', 's-pack35.adb',
            's-pack36.ads', 's-pack36.adb',
            's-pack37.ads', 's-pack37.adb',
            's-pack38.ads', 's-pack38.adb',
            's-pack39.ads', 's-pack39.adb',
            's-pack40.ads', 's-pack40.adb',
            's-pack41.ads', 's-pack41.adb',
            's-pack42.ads', 's-pack42.adb',
            's-pack43.ads', 's-pack43.adb',
            's-pack44.ads', 's-pack44.adb',
            's-pack45.ads', 's-pack45.adb',
            's-pack46.ads', 's-pack46.adb',
            's-pack47.ads', 's-pack47.adb',
            's-pack48.ads', 's-pack48.adb',
            's-pack49.ads', 's-pack49.adb',
            's-pack50.ads', 's-pack50.adb',
            's-pack51.ads', 's-pack51.adb',
            's-pack52.ads', 's-pack52.adb',
            's-pack53.ads', 's-pack53.adb',
            's-pack54.ads', 's-pack54.adb',
            's-pack55.ads', 's-pack55.adb',
            's-pack56.ads', 's-pack56.adb',
            's-pack57.ads', 's-pack57.adb',
            's-pack58.ads', 's-pack58.adb',
            's-pack59.ads', 's-pack59.adb',
            's-pack60.ads', 's-pack60.adb',
            's-pack61.ads', 's-pack61.adb',
            's-pack62.ads', 's-pack62.adb',
            's-pack63.ads', 's-pack63.adb',
            's-pooglo.ads', 's-pooglo.adb',
            's-pooloc.ads', 's-pooloc.adb',
            's-poosiz.ads', 's-poosiz.adb',
            's-powtab.ads',
            's-rannum.ads', 's-rannum.adb',
            's-ransee.ads', 's-ransee.adb',
            's-regexp.ads', 's-regexp.adb',
            's-restri.ads', 's-restri.adb',
            's-rident.ads',
            's-scaval.ads', 's-scaval.adb',
            's-soflin.ads', 's-soflin.adb',
            's-sopco3.ads', 's-sopco3.adb',
            's-sopco4.ads', 's-sopco4.adb',
            's-sopco5.ads', 's-sopco5.adb',
            's-spsufi.ads', 's-spsufi.adb',
            's-stalib.ads', 's-stalib.adb',
            's-stopoo.ads', 's-stopoo.adb',
            's-stposu.ads', 's-stposu.adb',
            's-stratt.ads', 's-stratt.adb',
            's-strhas.ads', 's-strhas.adb',
            's-string.ads', 's-string.adb',
            's-tasloc.ads', 's-tasloc.adb',
            's-traceb.ads',
            's-traent.ads', 's-traent.adb',
            's-trasym.ads', 's-trasym.adb',
            's-valboo.ads', 's-valboo.adb',
            's-valcha.ads', 's-valcha.adb',
            's-valdec.ads', 's-valdec.adb',
            's-valenu.ads', 's-valenu.adb',
            's-valint.ads', 's-valint.adb',
            's-vallld.ads', 's-vallld.adb',
            's-vallli.ads', 's-vallli.adb',
            's-valllu.ads', 's-valllu.adb',
            's-valrea.ads', 's-valrea.adb',
            's-valuns.ads', 's-valuns.adb',
            's-valuti.ads', 's-valuti.adb',
            's-valwch.ads', 's-valwch.adb',
            's-veboop.ads', 's-veboop.adb',
            's-vector.ads', 's-vercon.adb',
            's-vercon.ads',
            's-wchcnv.ads', 's-wchcnv.adb',
            's-wchcon.ads', 's-wchcon.adb',
            's-wchjis.ads', 's-wchjis.adb',
            's-wchstw.ads', 's-wchstw.adb',
            's-wchwts.ads', 's-wchwts.adb',
            's-widboo.ads', 's-widboo.adb',
            's-widcha.ads', 's-widcha.adb',
            's-widenu.ads', 's-widenu.adb',
            's-widlli.ads', 's-widlli.adb',
            's-widllu.ads', 's-widllu.adb',
            's-widwch.ads', 's-widwch.adb',
            's-wwdcha.ads', 's-wwdcha.adb',
            's-wwdenu.ads', 's-wwdenu.adb',
            's-wwdwch.ads', 's-wwdwch.adb',
            'src/tconfig.h',
            'src/tsystem.h',
            'libgcc/unwind-pe.h'])
        self.update_pairs('full', {
            'a-elchha.adb': 'a-elchha-xi.adb',
            'a-excach.adb': 'a-excach-cert.adb',
            'a-except.adb': 'a-except-2005.adb',
            'a-except.ads': 'a-except-2005.ads',
            'a-exexpr.adb': 'a-exexpr-gcc.adb',
            's-flocon.adb': 's-flocon-none.adb',
            's-io.adb': 's-io-xi.adb',
            's-ransee.adb': 's-ransee-xi.adb',
            's-soflin.adb': 's-soflin-xi.adb',
            's-soflin.ads': 's-soflin-xi.ads',
            's-traceb.ads': 's-traceb-cert.ads'})

        if self._is_bb:
            self.add_sources('full', 'adaint-xi.c')
            self.update_pairs('full', {
                's-memory.adb': 's-memory-xi.adb'})
        else:
            # PikeOS
            self.update_pairs('full', {
                's-memory.ads': 's-memory-pikeos.ads',
                's-memory.adb': 's-memory-pikeos.adb',
                's-init.adb': 's-init-pikeos-ravenscar.adb'})

        # Zero-cost-exception support
        self.add_sources('full/zcx-arm', {
            's-excmac.ads': 's-excmac-arm.ads',
            's-traceb.adb': 's-traceb-xi-armeabi.adb'})
        self.add_sources('full/zcx-dw2', [
            {'s-excmac.ads': 's-excmac-gcc.ads'},
            'libgcc/unwind-dw2-fde.h'])
        if self._is_bb:
            self.add_sources('full/zcx-dw2', 'src/unwind-dw2-fde-bb.c')

        self.add_sources('full/zcx-ppc', {
            's-traceb.adb': 's-traceb-xi-ppc.adb'})
        self.add_sources('full/zcx-leon', {
            's-traceb.adb': 's-traceb-xi-sparc.adb'})
        self.add_sources('full/zcx-x86', {
            's-traceb.adb': 's-traceb-vx653-sim.adb'})

        # Containers
        self.add_sources('containers', [
            'a-btgbso.adb', 'a-btgbso.ads',
            'a-cbdlli.adb', 'a-cbdlli.ads',
            'a-cbhama.adb', 'a-cbhama.ads',
            'a-cbhase.adb', 'a-cbhase.ads',
            'a-cbmutr.adb', 'a-cbmutr.ads',
            'a-cborma.adb', 'a-cborma.ads',
            'a-cborse.adb', 'a-cborse.ads',
            'a-cdlili.adb', 'a-cdlili.ads',
            'a-cfdlli.adb', 'a-cfdlli.ads',
            'a-cfhama.adb', 'a-cfhama.ads',
            'a-cfhase.adb', 'a-cfhase.ads',
            'a-cfinve.adb', 'a-cfinve.ads',
            'a-cforma.adb', 'a-cforma.ads',
            'a-cforse.adb', 'a-cforse.ads',
            'a-cgaaso.adb', 'a-cgaaso.ads',
            'a-cgarso.adb', 'a-cgarso.ads',
            'a-cgcaso.adb', 'a-cgcaso.ads',
            'a-chtgbk.adb', 'a-chtgbk.ads',
            'a-chtgbo.adb', 'a-chtgbo.ads',
            'a-chtgke.adb', 'a-chtgke.ads',
            'a-chtgop.adb', 'a-chtgop.ads',
            'a-cidlli.adb', 'a-cidlli.ads',
            'a-cihama.adb', 'a-cihama.ads',
            'a-cihase.adb', 'a-cihase.ads',
            'a-cimutr.adb', 'a-cimutr.ads',
            'a-ciorma.adb', 'a-ciorma.ads',
            'a-ciormu.adb', 'a-ciormu.ads',
            'a-ciorse.adb', 'a-ciorse.ads',
            'a-coboho.adb', 'a-coboho.ads',
            'a-cobove.adb', 'a-cobove.ads',
            'a-cofove.adb', 'a-cofove.ads',
            'a-cogeso.adb', 'a-cogeso.ads',
            'a-cohama.adb', 'a-cohama.ads',
            'a-cohase.adb', 'a-cohase.ads',
            'a-cohata.ads',
            'a-coinho.adb', 'a-coinho.ads',
            'a-coinve.adb', 'a-coinve.ads',
            'a-comutr.adb', 'a-comutr.ads',
            'a-conhel.adb', 'a-conhel.ads',
            'a-contai.ads',
            'a-convec.adb', 'a-convec.ads',
            'a-coorma.adb', 'a-coorma.ads',
            'a-coormu.adb', 'a-coormu.ads',
            'a-coorse.adb', 'a-coorse.ads',
            'a-coprnu.adb', 'a-coprnu.ads',
            'a-crbltr.ads',
            'a-crbtgk.adb', 'a-crbtgk.ads',
            'a-crbtgo.adb', 'a-crbtgo.ads',
            'a-crdlli.adb', 'a-crdlli.ads',
            'a-csquin.ads',
            'a-rbtgbk.adb', 'a-rbtgbk.ads',
            'a-rbtgbo.adb', 'a-rbtgbo.ads',
            'a-rbtgso.adb', 'a-rbtgso.ads',
            'a-iteint.ads',
            's-atocou.adb', 's-atocou.ads'])
        self.update_pairs('containers', {
            'a-coinho.adb': 'a-coinho-shared.adb',
            'a-coinho.ads': 'a-coinho-shared.ads',
            's-atocou.adb': 's-atocou-builtin.adb'})

        # GNARL files for the full runtime
        self.add_sources('gnarl/full', {
            's-taskin.ads': 's-taskin-xi-full.ads',
            's-tposen.adb': 's-tposen-xi-full.adb',
            's-tposen.ads': 's-tposen-xi-full.ads'})

        if self._is_bb:
            self.add_sources('gnarl/full', [
                's-btstch.ads', 's-btstch.adb'])

        # Ravenscar extended: relative delays
        self.add_sources('gnarl/full/extended', {
            's-reldel.ads': 's-reldel-xi.ads',
            's-reldel.adb': 's-reldel-xi.adb'})

        # Tasking extensions: multiple entries
        self.add_sources('gnarl/full/extended', [
            'a-synbar.adb', 'a-synbar.ads',
            'g-boubuf.adb', 'g-boubuf.ads',
            'g-boumai.ads',
            'g-semaph.adb', 'g-semaph.ads'])
        self.add_sources('gnarl/full/extended', {
            's-tpoben.ads': 's-tpoben-raven-full.ads',
            's-tpoben.adb': 's-tpoben-raven-full.adb',
            's-tasque.ads': 's-tasque-raven-full.ads',
            's-tasque.adb': 's-tasque-raven-full.adb',
            's-tpobop.ads': 's-tpobop-raven-full.ads',
            's-tpobop.adb': 's-tpobop-raven-full.adb'})

    def zfp_dirs(self, config, mem_routines, math_lib):
        """Returns the list of directories contained in a base ZFP runtime"""
        dirs = []
        dirs.append('common')
        dirs.append('zfp')

        if config.has_fpu:
            dirs.append('fpu')

        if mem_routines:
            dirs.append('mem')

        if config.is_pikeos:
            dirs.append('zfp_io')

        if math_lib:
            dirs.append('math')
            if self._is_bb:
                if config.has_single_precision_fpu:
                    dirs.append('math/hardsp')
                else:
                    dirs.append('math/softsp')
                if config.has_double_precision_fpu:
                    dirs.append('math/harddp')
                else:
                    dirs.append('math/softdp')
        return dirs

    def sfp_dirs(self, config, mem_routines, math_lib, small_mem):
        """Returns the list of directories contained in a base SFP runtime"""
        dirs = self.zfp_dirs(config, mem_routines, math_lib)

        if config.is_pikeos:
            dirs.remove('zfp_io')
            dirs.append('sfp_io')

        dirs.append('gnarl/common')
        dirs.append('gnarl/sfp')

        if not config.is_pikeos:
            if config.has_timer_64:
                dirs.append('gnarl/timer64')
            else:
                dirs.append('gnarl/timer32')
            if config.target.startswith('leon'):
                dirs.append('gnarl/spinlock/leon')
            else:
                dirs.append('gnarl/spinlock/gcc')
            if small_mem:
                dirs.append('gnarl/mem-small')
            else:
                dirs.append('gnarl/mem-large')

        return dirs

    def full_dirs(self, config, mem_routines, math_lib, small_mem):
        """Returns the list of directories contained in a base full runtime"""
        dirs = self.sfp_dirs(config, mem_routines, math_lib, small_mem)
        # remove zfp and sfp-specific files
        dirs.remove('zfp')
        dirs.remove('gnarl/sfp')
        dirs.append('full')

        cpu = config.target.split('-')[0]

        if cpu in ('arm', 'aarch64'):
            dirs.append('full/zcx-arm')
        elif cpu in ('powerpc',):
            dirs.append('full/zcx-dw2')
            dirs.append('full/zcx-ppc')
        elif cpu in ('leon', 'leon3'):
            dirs.append('full/zcx-dw2')
            dirs.append('full/zcx-leon')
        elif cpu in ('x86'):
            dirs.append('full/zcx-dw2')
            dirs.append('full/zcx-x86')
        else:
            print "Unexpected cpu %s" % cpu
            print "  not in 'arm', 'aarch64', 'powerpc', 'leon', 'leon3'"
            sys.exit(2)

        if not config.is_pikeos:
            if config.has_newlib:
                dirs.append('newlib')
            else:
                dirs.append('libc')

        dirs.append('containers')
        dirs.append('gnarl/full')
        dirs.append('gnarl/full/extended')

        return dirs


class TargetConfiguration(FilesHolder):
    """Gives information on the target to allow proper configuration of the
    runtime"""

    @property
    def target(self):
        raise Exception("not implemented")

    @property
    def is_pikeos(self):
        return 'pikeos' in self.target

    @property
    def has_fpu(self):
        return self.is_pikeos or \
            self.has_single_precision_fpu or self.has_double_precision_fpu

    @property
    def has_single_precision_fpu(self):
        return self.has_double_precision_fpu

    @property
    def has_double_precision_fpu(self):
        raise Exception("not implemented")

    @property
    def has_timer_64(self):
        raise Exception("not implemented")

    @property
    def has_newlib(self):
        raise Exception("not implemented")


class Target(TargetConfiguration):
    """Handles the creation of runtimes for a particular target"""

    def __init__(self, mem_routines, small_mem):
        """Initialize the target
        :param mem_routines: True for adding memory functions (memcpy..)
        :param small_mem: True when targetting a board with minimal memory

        The build_flags dictionnary is used to set attributes of
        runtime_build.gpr"""
        super(Target, self).__init__()
        self._srcs = SourceDirs(not self.is_pikeos)
        self._mem_routines = mem_routines
        self._small_mem = small_mem
        self.config_files = {}
        self.shared = None
        self.build_flags = {'source_dirs': None,
                            'common_flags': ['-fcallgraph-info=su,da',
                                             '-ffunction-sections',
                                             '-fdata-sections'],
                            'asm_flags': [],
                            'c_flags': ['-DIN_RTS', '-Dinhibit_libc']}

    def amend_zfp(self):
        pass

    def amend_ravenscar_sfp(self):
        self.amend_zfp()

    def amend_ravenscar_full(self):
        self.amend_ravenscar_sfp()

    def _init_zfp_config_files(self):
        self.config_files.update(
            {'target_options.gpr': readfile('src/target_options.gpr')})
        self.add_sources('arch', {
            'system.ads': None,
            's-macres.adb': None})
        self.add_sources('bsp', {
            # Implementation of s-textio is board-specific
            's-textio.adb': None})

    def _init_sfp_config_files(self):
        self._init_zfp_config_files()
        self.config_files.update(
            {'ravenscar_build.gpr': readfile('src/ravenscar_build.gpr')})
        self.add_sources('gnarl/bsp', 'a-intnam.ads')

        if not self.is_pikeos:
            self.add_sources('gnarl/bsp', {'s-bbpara.ads': None})
            self.add_sources('gnarl/arch', [
                's-bbcppr.ads', 's-bbcppr.adb',
                's-bbbosu.adb',
                's-bbinte.adb'])
            self.update_pairs({
                's-bbcppr.adb': None,
                's-bbbosu.adb': None})

    def init_as_zfp(self):
        self.shared = self._srcs.zfp_dirs(
            self, self._mem_routines, math_lib=False)
        self._init_zfp_config_files()

        self.amend_zfp()

    def init_as_sfp(self):
        self.shared = self._srcs.sfp_dirs(
            self, self._mem_routines, False, self._small_mem)
        self._init_sfp_config_files()

        self.amend_ravenscar_sfp()

    def init_as_full(self):
        self.shared = self._srcs.full_dirs(
            self, self._mem_routines, True, self._small_mem)
        self._init_sfp_config_files()

        self.amend_ravenscar_full()

    def install(self, destination):
        assert self.shared is not None, "Uninitialized Target object"

        # Build target directories
        destination = fullpath(destination)
        os.mkdir(destination)

        installed_files = []

        src_dirs = []
        gnarl_dirs = []
        gnarl_langs = ['Ada']
        gnat_dirs = []
        gnat_langs = ['Ada']

        # Install the shared rts files
        for d in sorted(self.shared):
            dest = self._srcs.install(d, destination, installed_files)
            src_dirs.append(dest)
            if 'gnarl' in d:
                gnarl_dirs.append(dest)
                if 'C' not in gnarl_langs and d in self._srcs.c_srcs:
                    gnarl_langs.append('C')
                if 'Asm_Cpp' not in gnarl_langs and d in self._srcs.asm_srcs:
                    gnarl_langs.append('Asm_Cpp')
            else:
                gnat_dirs.append(dest)
                if 'C' not in gnat_langs and d in self._srcs.c_srcs:
                    gnat_langs.append('C')
                if 'Asm_Cpp' not in gnat_langs and d in self._srcs.asm_srcs:
                    gnat_langs.append('Asm_Cpp')

        # Now install the rts-specific sources
        for dirname, l in self.dirs.items():
            subdir = os.path.join(destination, dirname)

            if dirname not in src_dirs:
                src_dirs.append(dirname)
                if 'gnarl' in dirname:
                    gnarl_dirs.append(dirname)
                    if 'C' not in gnarl_langs and \
                       dirname in self.c_srcs:
                        gnarl_langs.append('C')
                    if 'Asm_Cpp' not in gnarl_langs and \
                       dirname in self.asm_srcs:
                        gnarl_langs.append('Asm_Cpp')
                else:
                    gnat_dirs.append(dirname)
                    if 'C' not in gnat_langs and \
                       dirname in self.c_srcs:
                        gnat_langs.append('C')
                    if 'Asm_Cpp' not in gnat_langs and \
                       dirname in self.asm_srcs:
                        gnat_langs.append('Asm_Cpp')

                os.makedirs(subdir)

            if l and len(l) > 0:
                for srcname, pair in l.items():
                    self._copy_pair(srcname, pair, subdir, installed_files)

        for d in ['obj', 'adalib']:
            os.mkdir(os.path.join(destination, d))

        # Generate ada_source_path
        with open(os.path.join(destination, 'ada_source_path'), 'w') as fp:
            for d in sorted(src_dirs):
                fp.write(d + '\n')

        # Generate ada_object_path
        with open(os.path.join(destination, 'ada_object_path'), 'w') as fp:
            fp.write('adalib\n')

        # Write config files
        for name, content in self.config_files.iteritems():
            fp = open(os.path.join(destination, name), 'w')
            fp.write(content)
            fp.close()

        # Add the project files
        cnt = readfile(fullpath('src/runtime_build.gpr'))
        # Set source_dirs and languages
        self.build_flags['source_dirs'] = '", "'.join(sorted(gnat_dirs))
        self.build_flags['langs'] = '", "'.join(gnat_langs)
        # Flags array are converted to a string
        for f in ['common_flags', 'asm_flags', 'c_flags']:
            self.build_flags[f] = '",\n        "'.join(self.build_flags[f])
        self.build_flags['target'] = self.target
        # Format
        cnt = cnt.format(**self.build_flags)
        # Write
        fp = open(os.path.join(destination, 'runtime_build.gpr'), 'w')
        fp.write(cnt)
        fp.close()

        if len(gnarl_dirs) > 0:
            cnt = readfile(fullpath('src/ravenscar_build.gpr'))
            # Set source_dirs and languages
            self.build_flags['source_dirs'] = '", "'.join(sorted(gnarl_dirs))
            self.build_flags['langs'] = '", "'.join(gnarl_langs)
            # Format
            cnt = cnt.format(**self.build_flags)
            # Write
            fp = open(os.path.join(destination, 'ravenscar_build.gpr'), 'w')
            fp.write(cnt)
            fp.close()


class PikeOS(Target):
    def __init__(self):
        super(PikeOS, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self):
        super(PikeOS, self).amend_zfp()
        self.add_sources('arch', [
            'arm/pikeos/memory.ld',
            'pikeos-cert-app.c'])
        self.update_pairs({
            's-textio.adb': 's-textio-pikeos.adb',
            's-macres.adb': 's-macres-native.adb'})
        # Children should add a pair for system.ads and read runtime.xml

    def amend_ravenscar_sfp(self):
        super(PikeOS, self).amend_ravenscar_sfp()
        self.add_sources('arch', 'adaint-pikeos.c')
        self.update_pairs({
            'a-intnam.ads': 'a-intnam-dummy.ads'})

    def amend_ravenscar_full(self):
        super(PikeOS, self).amend_ravenscar_full()
        # Register ZCX frames (for pikeos-cert-app.c)
        self.build_flags['c_flags'] += ['-DUSE_ZCX']


class PikeOS3(PikeOS):
    def amend_zfp(self):
        super(PikeOS3, self).amend_zfp()
        # Don't use function/data sections, not supported by linker script
        self.build_flags['common_flags'] = \
            filter(lambda x: x not in ['-ffunction-sections',
                                       '-fdata-sections'],
                   self.build_flags['common_flags'])

    def amend_ravenscar_sfp(self):
        super(PikeOS3, self).amend_ravenscar_sfp()
        self.shared.append('gnarl/pikeos3')


class PikeOS4(PikeOS):
    def amend_ravenscar_sfp(self):
        super(PikeOS4, self).amend_ravenscar_sfp()
        self.shared.append('gnarl/pikeos4')


class ArmPikeOS(PikeOS4):
    @property
    def target(self):
        return 'arm-pikeos'

    def amend_zfp(self):
        super(ArmPikeOS, self).amend_zfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-arm.ads'})
        self.config_files.update(
            {'runtime.xml': readfile('arm/pikeos/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(ArmPikeOS, self).amend_ravenscar_sfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-arm-ravenscar-sfp.ads'})

    def amend_ravenscar_full(self):
        super(ArmPikeOS, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-pikeos-arm-ravenscar-full.ads'})


class PpcPikeOS(PikeOS3):
    @property
    def target(self):
        return 'powerpc-pikeos'

    def amend_zfp(self):
        super(PpcPikeOS, self).amend_zfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-ppc.ads'})
        self.config_files.update(
            {'runtime.xml': readfile('powerpc/pikeos3/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(PpcPikeOS, self).amend_ravenscar_sfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-ppc-ravenscar-sfp.ads'})

    def amend_ravenscar_full(self):
        super(PpcPikeOS, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-pikeos-ppc-ravenscar-full.ads'})


class X86PikeOS(PikeOS3):
    @property
    def target(self):
        return 'x86-pikeos'

    def amend_zfp(self):
        super(X86PikeOS, self).amend_zfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-x86.ads'})
        # FIXME: use an i586 specific runtime.xml ?
        self.config_files.update(
            {'runtime.xml': readfile('arm/pikeos/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(X86PikeOS, self).amend_ravenscar_sfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-x86-ravenscar-sfp.ads'})

    def amend_ravenscar_full(self):
        super(X86PikeOS, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-pikeos-x86-ravenscar-full.ads'})


class CortexMTarget(Target):
    @property
    def target(self):
        return "arm-eabi"

    @property
    def has_timer_64(self):
        return False

    @property
    def has_newlib(self):
        return True

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return False

    def __init__(self):
        super(CortexMTarget, self).__init__(
            mem_routines=True,
            small_mem=True)

    def amend_zfp(self):
        super(CortexMTarget, self).amend_zfp()
        self.update_pairs({
            'system.ads': 'system-xi-arm.ads',
            's-macres.adb': 's-macres-cortexm3.adb'})

    def amend_ravenscar_sfp(self):
        super(CortexMTarget, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/arch', 's-bbsumu.adb')
        self.update_pairs({
            'system.ads': 'system-xi-cortexm4-sfp.ads',
            's-bbcppr.adb': 's-bbcppr-armv7m.adb',
            's-bbbosu.adb': 's-bbbosu-armv7m.adb'})

    def amend_ravenscar_full(self):
        super(CortexMTarget, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-xi-cortexm4-full.ads'})
        self.config_files['runtime.xml'] = \
            self.config_files['runtime.xml'].replace(
                '"-nolibc", ', '"-lc", "-lgnat", ')


class LM3S(CortexMTarget):
    @property
    def has_single_precision_fpu(self):
        return False

    @property
    def has_fpu(self):
        # Still add floating point attributes
        return True

    def amend_zfp(self):
        super(LM3S, self).amend_zfp()
        self.add_sources('arch', [
            'arm/lm3s/lm3s-rom.ld',
            'arm/lm3s/lm3s-ram.ld',
            'arm/lm3s/start-rom.S',
            'arm/lm3s/start-ram.S',
            'arm/lm3s/setup_pll.adb',
            'arm/lm3s/setup_pll.ads'])
        self.update_pairs({
            's-textio.adb': 's-textio-lm3s.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('arm/lm3s/runtime.xml')})


class Stm32(CortexMTarget):
    _to_mcu = {
        'stm32f4': 'stm32f40x',
        'stm32f429disco': 'stm32f429x',
        'stm32f469disco': 'stm32f469x',
        'stm32f7disco': 'stm32f7x',
        'stm32f769disco': 'stm32f7x9'}

    def __init__(self, board):
        super(Stm32, self).__init__()

        assert board in Stm32._to_mcu, 'Unknown STM32 board: %s' % board
        self.mcu = Stm32._to_mcu[board]

    @property
    def has_double_precision_fpu(self):
        return self.mcu == 'stm32f7x9'

    def amend_zfp(self):
        super(Stm32, self).amend_zfp()

        if self.has_source('s-bbpara.ads'):
            self.remove_source('s-bbpara.ads')
        # s-bbpara.ads is always needed: put in the bsp
        self.add_sources('bsp', {'s-bbpara.ads': 's-bbpara-stm32f4.ads'})

        self.add_sources('bsp', [
            's-stm32.ads',
            's-stm32.adb',
            'arm/stm32/common-RAM.ld',
            'arm/stm32/common-ROM.ld',
            'arm/stm32/start-rom.S',
            'arm/stm32/start-ram.S',
            'arm/stm32/start-common.S',
            'arm/stm32/setup_pll.adb',
            'arm/stm32/setup_pll.ads',
            'arm/stm32/%s/memory-map.ld' % self.mcu,
            'arm/stm32/%s/s-bbmcpa.ads' % self.mcu,
            'arm/stm32/%s/s-bbmcpa.adb' % self.mcu,
            'arm/stm32/%s/s-bbbopa.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-flash.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-gpio.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-pwr.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-rcc.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-syscfg.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-usart.ads' % self.mcu])

        if self.mcu == 'stm32f40x':
            self.update_pairs({
                's-stm32.adb': 's-stm32-f40x.adb',
                's-textio.adb': 's-textio-stm32f4.adb'})
        elif self.mcu == 'stm32f429x':
            self.update_pairs({
                's-stm32.adb': 's-stm32-f4x9x.adb',
                's-textio.adb': 's-textio-stm32f4.adb'})
        elif self.mcu == 'stm32f469x':
            self.update_pairs({
                's-stm32.adb': 's-stm32-f4x9x.adb',
                's-textio.adb': 's-textio-stm32f469.adb'})
        elif self.mcu in ('stm32f7x', 'stm32f7x9'):
            self.update_pairs({
                's-stm32.adb': 's-stm32-f7x.adb',
                's-textio.adb': 's-textio-stm32f7.adb'})

        runtime_xml = readfile('arm/stm32/runtime.xml')
        if self.mcu in ('stm32f7x', 'stm32f7x9'):
            runtime_xml = runtime_xml.replace('cortex-m4', 'cortex-m7')

        if self.mcu == 'stm32f7x9':
            # double precision cortex-m7 fpu
            runtime_xml = runtime_xml.replace('fpv4-sp-d16', 'fpv5-d16')
        elif self.mcu == 'stm32f7x':
            # single precision cortex-m7 fpu
            runtime_xml = runtime_xml.replace('fpv4-sp-d16', 'fpv5-sp-d16')

        self.config_files.update({'runtime.xml': runtime_xml})

    def amend_ravenscar_sfp(self):
        super(Stm32, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/bsp', 'arm/stm32/%s/svd/handler.S' % self.mcu)
        self.update_pairs({
            'a-intnam.ads': 'arm/stm32/%s/svd/a-intnam.ads' % self.mcu})


class SmartFusion2(CortexMTarget):
    @property
    def has_single_precision_fpu(self):
        return False

    @property
    def has_fpu(self):
        # Still add floating point attributes
        return True

    def __init__(self):
        super(SmartFusion2, self).__init__()

    def amend_zfp(self):
        super(SmartFusion2, self).amend_zfp()

        self.add_sources('bsp', [
            'arm/smartfusion2/common-ROM.ld',
            'arm/smartfusion2/start-rom.S',
            'arm/smartfusion2/setup_pll.adb',
            'arm/smartfusion2/setup_pll.ads',
            'arm/smartfusion2/memory-map.ld',
            'arm/smartfusion2/s-sf2.ads',
            'arm/smartfusion2/s-sf2.adb',
            'arm/smartfusion2/s-sf2gpi.ads',
            'arm/smartfusion2/s-sf2gpi.adb',
            'arm/smartfusion2/s-sf2uar.ads',
            'arm/smartfusion2/s-sf2uar.adb',
            'arm/smartfusion2/svd/i-sf2.ads',
            'arm/smartfusion2/svd/i-sf2-system_registers.ads',
            'arm/smartfusion2/svd/i-sf2-mmuart.ads',
            'arm/smartfusion2/svd/i-sf2-gpio.ads'])

        self.update_pairs({
            's-textio.adb': 'arm/smartfusion2/s-textio.adb'})

        runtime_xml = readfile('arm/smartfusion2/runtime.xml')
        self.config_files.update({'runtime.xml': runtime_xml})

    def amend_ravenscar_sfp(self):
        super(SmartFusion2, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/bsp', 'arm/smartfusion2/svd/handler.S')
        self.update_pairs({
            's-bbpara.ads': 's-bbpara-smartfusion2.ads',
            'a-intnam.ads': 'arm/smartfusion2/svd/a-intnam.ads'})


class Sam(CortexMTarget):
    def __init__(self, board):
        super(Sam, self).__init__()

        assert board in ('sam4s', 'samg55'), 'Unknown SAM board: %s' % board
        self.board = board

    @property
    def has_single_precision_fpu(self):
        return self.board != 'sam4s'

    def amend_zfp(self):
        super(Sam, self).amend_zfp()
        self.add_sources('bsp', [
            's-sam4s.ads',
            'arm/sam/common-SAMBA.ld',
            'arm/sam/common-ROM.ld',
            'arm/sam/start-rom.S',
            'arm/sam/start-ram.S',
            'arm/sam/setup_pll.ads',
            'arm/sam/%s/board_config.ads' % self.board,
            'arm/sam/%s/setup_pll.adb' % self.board,
            'arm/sam/%s/memory-map.ld' % self.board,
            'arm/sam/%s/svd/i-sam.ads' % self.board,
            'arm/sam/%s/svd/i-sam-efc.ads' % self.board,
            'arm/sam/%s/svd/i-sam-pmc.ads' % self.board,
            'arm/sam/%s/svd/i-sam-sysc.ads' % self.board])

        self.update_pairs({
            's-textio.adb': 's-textio-sam4s.adb'})

        if self.has_fpu:
            fp = 'hardfloat'
        else:
            fp = 'softfloat'
        self.config_files.update(
            {'runtime.xml': readfile('arm/sam/%s/runtime.xml' % fp)})

    def amend_ravenscar_sfp(self):
        super(Sam, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/bsp', [
            'arm/sam/%s/svd/handler.S' % self.board,
            'arm/sam/%s/s-bbbopa.ads' % self.board,
            'arm/sam/%s/s-bbmcpa.ads' % self.board])

        self.update_pairs({
            'a-intnam.ads': 'arm/sam/%s/svd/a-intnam.ads' % self.board,
            's-bbpara.ads': 's-bbpara-sam4s.ads'})


class DFBBTarget(Target):
    "BB target with single and double FPU"

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True

    @property
    def has_timer_64(self):
        return False

    @property
    def has_newlib(self):
        return True


class TMS570(DFBBTarget):
    @property
    def target(self):
        return 'arm-eabi'

    def __init__(self):
        super(TMS570, self).__init__(
            mem_routines=True,
            small_mem=True)

    def amend_zfp(self):
        super(TMS570, self).amend_zfp()
        self.add_sources('bsp', [
            'arm/tms570/sys_startup.S',
            'arm/tms570/crt0.S',
            'arm/tms570/start-ram.S',
            'arm/tms570/start-rom.S'])
        self.add_sources('arch', [
            'arm/tms570/tms570.ld',
            'arm/tms570/flash.ld',
            'arm/tms570/monitor.ld',
            'arm/tms570/hiram.ld',
            'arm/tms570/loram.ld',
            'arm/tms570/common.ld'])
        self.update_pairs(
            {'system.ads': 'system-xi-arm.ads',
             's-textio.adb': 's-textio-tms570.adb',
             's-macres.adb': 's-macres-tms570.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('arm/tms570/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(TMS570, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/arch', [
            's-bbcpsp.ads',
            's-bbsumu.adb'])
        self.update_pairs({
            'system.ads': 'system-xi-arm-sfp.ads',
            's-bbcppr.adb': 's-bbcppr-arm.adb',
            's-bbcpsp.ads': 's-bbcpsp-arm.ads',
            'a-intnam.ads': 'a-intnam-xi-tms570.ads',
            's-bbbosu.adb': 's-bbbosu-tms570.adb',
            's-bbpara.ads': 's-bbpara-tms570.ads'})

    def amend_ravenscar_full(self):
        super(TMS570, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-xi-arm-full.ads'})
        self.config_files['runtime.xml'] = \
            self.config_files['runtime.xml'].replace(
                '"-nolibc", ', '"-lc", "-lgnat", ')


class Zynq7000(DFBBTarget):
    @property
    def target(self):
        return 'arm-eabi'

    @property
    def has_timer_64(self):
        return True

    @property
    def has_newlib(self):
        return True

    def __init__(self):
        super(Zynq7000, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self):
        super(Zynq7000, self).amend_zfp()
        self.add_sources('bsp', [
            'arm/zynq/ram.ld',
            'arm/zynq/start-ram.S',
            'arm/zynq/memmap.inc',
            'i-arm_v7ar.ads',
            'i-arm_v7ar.adb'])
        self.add_sources('arch', [
            'i-cache.ads',
            'i-cache.adb'])
        self.update_pairs(
            {'system.ads': 'system-xi-arm.ads',
             's-textio.adb': 's-textio-zynq.adb',
             's-macres.adb': 's-macres-zynq.adb',
             'i-cache.ads': 'i-cache.ads',
             'i-cache.adb': 'i-cache-armv7.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('arm/zynq/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(Zynq7000, self).amend_ravenscar_sfp()

        self.add_sources('gnarl/arch', 's-bbcpsp.ads')
        self.update_pairs({
            'system.ads': 'system-xi-cortexa-sfp.ads',
            's-bbcppr.adb': 's-bbcppr-arm.adb',
            's-bbcpsp.ads': 's-bbcpsp-arm.ads',
            'a-intnam.ads': 'a-intnam-xi-zynq.ads',
            's-bbbosu.adb': 's-bbbosu-cortexa9.adb',
            's-bbpara.ads': 's-bbpara-cortexa9.ads'})

    def amend_ravenscar_full(self):
        super(Zynq7000, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-xi-cortexa-full.ads'})
        self.config_files['runtime.xml'] = \
            self.config_files['runtime.xml'].replace(
                '"-nolibc", ', '"-lc", "-lgnat", ')


class RPI2(DFBBTarget):
    @property
    def target(self):
        return 'arm-eabi'

    @property
    def has_timer_64(self):
        return True

    @property
    def has_newlib(self):
        return True

    def __init__(self):
        super(RPI2, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self):
        super(RPI2, self).amend_zfp()
        self.add_sources('bsp', [
            'arm/rpi2/ram.ld',
            'arm/rpi2/start-ram.S',
            'arm/rpi2/memmap.s',
            'i-raspberry_pi.ads',
            'i-arm_v7ar.ads',
            'i-arm_v7ar.adb'])
        self.add_sources('arch', [
            'i-cache.ads',
            'i-cache.adb'])
        self.update_pairs(
            {'system.ads': 'system-xi-arm.ads',
             's-textio.adb': 's-textio-rpi2.adb',
             's-macres.adb': 's-macres-rpi2.adb',
             'i-cache.ads': 'i-cache.ads',
             'i-cache.adb': 'i-cache-armv7.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('arm/rpi2/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(RPI2, self).amend_ravenscar_sfp()

        self.add_sources('gnarl/arch', 's-bbcpsp.ads')
        self.update_pairs({
            'system.ads': 'system-xi-arm-sfp.ads',
            's-bbcppr.adb': 's-bbcppr-arm.adb',
            's-bbcpsp.ads': 's-bbcpsp-arm.ads',
            'a-intnam.ads': 'a-intnam-dummy.ads',
            's-bbbosu.adb': 's-bbbosu-rpi2.adb',
            's-bbpara.ads': 's-bbpara-rpi2.ads'})

    def amend_ravenscar_full(self):
        super(RPI2, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-xi-arm-full.ads'})


class RPI3(DFBBTarget):
    @property
    def target(self):
        return 'aarch64-eabi'

    @property
    def has_timer_64(self):
        return True

    @property
    def has_newlib(self):
        return True

    def __init__(self):
        super(RPI3, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self):
        super(RPI3, self).amend_zfp()
        self.add_sources('arch', [
            'i-cache.ads',
            'i-cache.adb'])
        self.add_sources('bsp', [
            'aarch64/rpi3/ram.ld',
            'aarch64/rpi3/start-ram.S',
            'aarch64/rpi3/memmap.s',
            'aarch64/context_switch.S',
            'aarch64/rpi3/trap_dump.ads',
            'aarch64/rpi3/trap_dump.adb',
            'i-raspberry_pi.ads'])
        self.update_pairs(
            {'system.ads': 'system-xi-aarch64.ads',
             's-textio.adb': 's-textio-rpi2.adb',
             's-macres.adb': 's-macres-rpi2.adb',
             'i-cache.ads': 'i-cache.ads',
             'i-cache.adb': 'i-cache-aarch64.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('aarch64/rpi3/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(RPI3, self).amend_ravenscar_sfp()

        self.add_sources('gnarl/arch', 's-bbcpsp.ads')
        self.update_pairs({
            'system.ads': 'system-xi-arm-sfp.ads',
            's-bbcppr.ads': 's-bbcppr-ppc.ads',
            's-bbcppr.adb': 's-bbcppr-aarch64.adb',
            's-bbcpsp.ads': 's-bbcpsp-aarch64.ads',
            'a-intnam.ads': 'a-intnam-dummy.ads',
            's-bbbosu.adb': 's-bbbosu-rpi3.adb',
            's-bbpara.ads': 's-bbpara-rpi2.ads'})

    def amend_ravenscar_full(self):
        super(RPI3, self).amend_ravenscar_full()


class SparcBBTarget(DFBBTarget):
    def __init__(self):
        super(SparcBBTarget, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self):
        super(SparcBBTarget, self).amend_zfp()
        self.add_sources('arch', 'sparc.h')
        # Add s-bbbopa (needed by zfp for uart address)
        self.add_sources('bsp', 's-bbbopa.ads')
        self.update_pairs({
            'system.ads': 'system-xi-sparc.ads',
            's-macres.adb': 's-macres-leon.adb',
            'sparc.h': 'sparc-bb.h'})

        # Was not present on erc32:
        self.build_flags['c_flags'] += ['-DLEON']

    def amend_ravenscar_sfp(self):
        super(SparcBBTarget, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/bsp', [
            'context_switch.S',
            'trap_handler.S',
            'interrupt_masking.S',
            'floating_point.S',
            's-bcpith.adb',
            # Were not present in erc32:
            's-bbcaco.ads', 's-bbcaco.adb'])
        self.update_pairs({
            'system.ads': 'system-xi-sparc-ravenscar.ads',
            's-bbcppr.adb': 's-bbcppr-sparc.adb',
            's-bcpith.adb': 's-bcpith-bb-sparc.adb',
            'context_switch.S': 'context_switch-bb-sparc.S',
            'trap_handler.S': 'trap_handler-bb-sparc.S',
            'interrupt_masking.S': 'interrupt_masking-bb-sparc.S',
            'floating_point.S': 'floating_point-bb-sparc.S',
            's-bbcaco.adb': 's-bbcaco-leon.adb'})

    def amend_ravenscar_full(self):
        super(SparcBBTarget, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-xi-sparc-full.ads'})
        # Use leon-zcx.specs to link with -lc.
        self.config_files.update(
            {'link-zcx.spec':
             readfile(os.path.join(crossdir, 'leon-elf/leon-zcx.specs'))})
        self.config_files['runtime.xml'] = \
            self.config_files['runtime.xml'].replace(
                '"-nostartfiles",',
                '"--specs=${RUNTIME_DIR(ada)}/link-zcx.spec",')


class Leon2(SparcBBTarget):
    @property
    def target(self):
        return 'leon-elf'

    def __init__(self):
        super(Leon2, self).__init__()

    def amend_zfp(self):
        super(Leon2, self).amend_zfp()
        self.add_sources('arch', [
            'leon-elf/leon.ld',
            'leon-elf/crt0.S'])
        self.add_sources('bsp', 'sparc/leon/hw_init.S')
        self.update_pairs({
            's-textio.adb': 's-textio-leon.adb',
            's-bbbopa.ads': 's-bbbopa-leon.ads'})
        self.config_files.update(
            {'runtime.xml': readfile('sparc/leon/runtime.xml')})
        self.build_flags['c_flags'] += ['-DLEON2']

    def amend_ravenscar_sfp(self):
        super(Leon2, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/arch', [
            's-bbsule.ads',
            's-bbsumu.adb'])
        self.update_pairs({
            's-bbbosu.adb': 's-bbbosu-leon.adb',
            's-bbpara.ads': 's-bbpara-leon.ads',
            'a-intnam.ads': 'a-intnam-xi-leon.ads'})


class Leon3(SparcBBTarget):
    @property
    def target(self):
        return 'leon3-elf'

    def __init__(self):
        super(Leon3, self).__init__()

    def amend_zfp(self):
        super(Leon3, self).amend_zfp()
        self.add_sources('arch', [
            'leon3-elf/leon.ld',
            'leon-elf/crt0.S'])
        self.add_sources('bsp', 'sparc/leon/hw_init.S')
        self.update_pairs({
            's-textio.adb': 's-textio-leon3.adb',
            's-bbbopa.ads': 's-bbbopa-leon3.ads'})
        self.config_files.update(
            {'runtime.xml': readfile('sparc/leon3/runtime.xml')})
        self.build_flags['c_flags'] += ['-DLEON3']

    def amend_ravenscar_sfp(self):
        super(Leon3, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/arch', 's-bbsle3.ads')
        self.update_pairs({
            's-bbbosu.adb': 's-bbbosu-leon3.adb',
            's-bbpara.ads': 's-bbpara-leon.ads',
            'a-intnam.ads': 'a-intnam-xi-leon3.ads'})


class PPC6XXBBTarget(DFBBTarget):
    @property
    def target(self):
        return 'powerpc-elf'

    @property
    def has_timer_64(self):
        return True

    @property
    def has_newlib(self):
        return False

    @property
    def has_fpu(self):
        # Add fpu support
        return True

    @property
    def has_single_precision_fpu(self):
        # But use soft float in the math lib
        return False

    @property
    def has_double_precision_fpu(self):
        # But use soft float in the math lib
        return False

    def __init__(self):
        super(PPC6XXBBTarget, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self):
        super(PPC6XXBBTarget, self).amend_zfp()
        self.update_pairs({
            'system.ads': 'system-xi-ppc.ads'})

    def amend_ravenscar_sfp(self):
        super(PPC6XXBBTarget, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/bsp', [
            'powerpc/6xx/context_switch.S',
            'powerpc/6xx/handler.S'])
        self.add_sources('gnarl/arch', [
            's-bbcpsp.ads', 's-bbcpsp.adb'])
        self.update_pairs({
            'system.ads': 'system-xi-ppc-sfp.ads',
            's-bbcppr.adb': 's-bbcppr-ppc.adb',
            's-bbcppr.ads': 's-bbcppr-ppc.ads',
            's-bbinte.adb': 's-bbinte-ppc.adb',
            's-bbcpsp.ads': 's-bbcpsp-6xx.ads',
            's-bbcpsp.adb': 's-bbcpsp-6xx.adb'})

    def amend_ravenscar_full(self):
        super(PPC6XXBBTarget, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-xi-ppc-full.ads'})


class MPC8641(PPC6XXBBTarget):
    def amend_zfp(self):
        super(MPC8641, self).amend_zfp()
        self.add_sources('arch', [
            'powerpc/8641d/qemu-rom.ld',
            'powerpc/8641d/ram.ld'])
        self.add_sources('bsp', [
            'powerpc/8641d/start-rom.S',
            'powerpc/8641d/setup.S',
            's-bbbopa.ads'])  # Add s-bbbopa (needed by zfp for uart address)
        self.update_pairs({
            's-macres.adb': 's-macres-p2020.adb',
            's-bbbopa.ads': 's-bbbopa-8641d.ads',
            's-textio.adb': 's-textio-p2020.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('powerpc/8641d/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(MPC8641, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/arch', ['s-bbsuti.adb', 's-bbsumu.adb'])
        self.update_pairs({
            's-bbbosu.adb': 's-bbbosu-ppc-openpic.adb',
            's-bbsuti.adb': 's-bbsuti-ppc.adb',
            's-bbsumu.adb': 's-bbsumu-8641d.adb',
            's-bbpara.ads': 's-bbpara-8641d.ads',
            'a-intnam.ads': 'a-intnam-xi-ppc-openpic.ads'})

    def amend_ravenscar_full(self):
        super(MPC8641, self).amend_ravenscar_full()
        self.config_files.update(
            {'link-zcx.spec':
             readfile('powerpc/prep/link-zcx.spec')})
        self.config_files['runtime.xml'] = \
            self.config_files['runtime.xml'].replace(
                '"-nolibc"',
                '"-nolibc",\n' +
                '         "-lgnat", "-lgcc", "-lgnat",\n' +
                '         "--specs=${RUNTIME_DIR(ada)}/link-zcx.spec"')


class MPC8349e(PPC6XXBBTarget):
    def amend_zfp(self):
        super(MPC8349e, self).amend_zfp()
        self.add_sources('arch', 'powerpc/8349e/ram.ld')
        self.add_sources('bsp', [
            'powerpc/8349e/start-ram.S',
            'powerpc/8349e/setup.S',
            's-bbbopa.ads'])  # Add s-bbbopa (needed by zfp for uart address)
        self.update_pairs({
            's-macres.adb': 's-macres-8349e.adb',
            's-bbbopa.ads': 's-bbbopa-8349e.ads',
            's-textio.adb': 's-textio-p2020.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('powerpc/8349e/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(MPC8349e, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/arch', 's-bbsuti.adb')
        self.update_pairs({
            's-bbbosu.adb': 's-bbbosu-8349e.adb',
            's-bbsuti.adb': 's-bbsuti-ppc.adb',
            's-bbpara.ads': 's-bbpara-ppc.ads',
            'a-intnam.ads': 'a-intnam-xi-8349e.ads'})


class PPCSPEBBTarget(DFBBTarget):
    @property
    def target(self):
        return 'powerpc-eabispe'

    @property
    def has_timer_64(self):
        return True

    @property
    def has_newlib(self):
        return False

    @property
    def has_fpu(self):
        # Add fpu support
        return True

    @property
    def has_single_precision_fpu(self):
        # But use soft float in the math lib
        return False

    @property
    def has_double_precision_fpu(self):
        # But use soft float in the math lib
        return False

    def __init__(self):
        super(PPCSPEBBTarget, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self):
        super(PPCSPEBBTarget, self).amend_zfp()
        self.update_pairs({
            'system.ads': 'system-xi-e500v2.ads'})

    def amend_ravenscar_sfp(self):
        super(PPCSPEBBTarget, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/bsp', [
            'powerpc/spe/handler.S',
            'powerpc/spe/context_switch.S'])
        self.add_sources('gnarl/arch', [
            's-bbcpsp.ads', 's-bbcpsp.adb'])
        self.update_pairs({
            'system.ads': 'system-xi-e500v2-sfp.ads',
            's-bbcppr.adb': 's-bbcppr-ppc.adb',
            's-bbcppr.ads': 's-bbcppr-ppc.ads',
            's-bbinte.adb': 's-bbinte-ppc.adb',
            's-bbcpsp.ads': 's-bbcpsp-spe.ads',
            's-bbcpsp.adb': 's-bbcpsp-spe.adb'})

    def amend_ravenscar_full(self):
        super(PPCSPEBBTarget, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-xi-e500v2-full.ads'})


class P2020(PPCSPEBBTarget):
    def amend_zfp(self):
        super(P2020, self).amend_zfp()
        self.add_sources('arch', 'powerpc/p2020/p2020.ld')
        self.add_sources('bsp', [
            'powerpc/p2020/start-ram.S',
            'powerpc/p2020/setup.S',
            's-bbbopa.ads'])  # Add s-bbbopa (needed by zfp for uart address)
        self.update_pairs({
            's-macres.adb': 's-macres-p2020.adb',
            's-bbbopa.ads': 's-bbbopa-p2020.ads',
            's-textio.adb': 's-textio-p2020.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('powerpc/p2020/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(P2020, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/arch', [
            's-bbsuti.adb',
            's-bbsumu.adb'])
        self.update_pairs({
            's-bbbosu.adb': 's-bbbosu-ppc-openpic.adb',
            's-bbsuti.adb': 's-bbsuti-ppc.adb',
            's-bbpara.ads': 's-bbpara-ppc.ads',
            'a-intnam.ads': 'a-intnam-xi-ppc-openpic.ads'})

    def amend_ravenscar_full(self):
        super(P2020, self).amend_ravenscar_full()
        self.config_files.update(
            {'link-zcx.spec':
             readfile('powerpc/prep/link-zcx.spec')})
        self.config_files['runtime.xml'] = \
            self.config_files['runtime.xml'].replace(
                '"-nolibc"',
                '"-nolibc",\n' +
                '         "-lgnat", "-lgcc", "-lgnat",\n' +
                '         "--specs=${RUNTIME_DIR(ada)}/link-zcx.spec"')


class P5566(PPCSPEBBTarget):
    def amend_zfp(self):
        super(P5566, self).amend_zfp()
        self.add_sources('arch', [
            'powerpc/p5566/bam.ld',
            'powerpc/p5566/flash.ld',
            'powerpc/p5566/ram.ld'])
        self.add_sources('bsp', [
            'powerpc/p5566/start-bam.S',
            'powerpc/p5566/start-ram.S',
            'powerpc/p5566/start-flash.S',
            'powerpc/p5566/setup.S',
            'powerpc/p5566/setup-pll.S'])
        self.update_pairs({
            's-macres.adb': 's-macres-p55.adb',
            's-textio.adb': 's-textio-p55.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('powerpc/p5566/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(P5566, self).amend_ravenscar_sfp()
        self.add_sources('gnarl/bsp', 's-bbbopa.ads')
        self.add_sources('gnarl/arch', [
            's-bbsuti.adb',
            's-bbsumu.adb'])
        self.update_pairs({
            's-bbbopa.ads': 's-bbbopa-p55.ads',
            's-bbbosu.adb': 's-bbbosu-p55.adb',
            's-bbsuti.adb': 's-bbsuti-ppc.adb',
            's-bbpara.ads': 's-bbpara-p55.ads',
            'a-intnam.ads': 'a-intnam-xi-p55.ads'})

    def amend_ravenscar_full(self):
        super(P5566, self).amend_ravenscar_full()
        self.config_files.update(
            {'link-zcx.spec':
             readfile('powerpc/prep/link-zcx.spec')})
        self.config_files['runtime.xml'] = \
            self.config_files['runtime.xml'].replace(
                ' "-nostartfiles"',
                '\n' +
                '         "-lgnat", "-lgcc", "-lgnat",\n' +
                '         "--specs=${RUNTIME_DIR(ada)}/link-zcx.spec"')


class P5634(PPCSPEBBTarget):
    def amend_zfp(self):
        super(P5634, self).amend_zfp()
        self.add_sources('arch', 'powerpc/mpc5634/5634.ld')
        self.add_sources('bsp', 'powerpc/mpc5634/start.S')
        self.update_pairs({
            's-macres.adb': 's-macres-p55.adb',
            's-textio.adb': 's-textio-p55.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('powerpc/mpc5634/runtime.xml')})


class Visium(DFBBTarget):
    @property
    def target(self):
        return 'visium-elf'

    @property
    def has_timer_64(self):
        return False

    @property
    def has_newlib(self):
        return True

    def __init__(self):
        super(Visium, self).__init__(
            mem_routines=False,
            small_mem=True)

    def amend_zfp(self):
        super(Visium, self).amend_zfp()
        self.update_pairs(
            {'system.ads': 'system-xi-visium.ads',
             's-textio.adb': 's-textio-stdio.adb',
             's-macres.adb': 's-macres-native.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('visium/mcm/runtime.xml'),
             'target_options.gpr': readfile('visium/mcm/target_options.gpr')})


class X86Native(DFBBTarget):
    def __init__(self):
        super(X86Native, self).__init__(
            mem_routines=False,
            small_mem=False)

    def amend_zfp(self):
        super(X86Native, self).amend_zfp()
        self.update_pairs(
            {'system.ads': 'system-xi-x86.ads',
             's-textio.adb': 's-textio-stdio.adb',
             's-macres.adb': 's-macres-native.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('native/runtime.xml')})


class X86Linux(X86Native):
    @property
    def target(self):
        return 'x86-linux'

    def __init__(self):
        super(X86Linux, self).__init__()


class X86Windows(X86Native):
    @property
    def target(self):
        return 'x86-windows'

    def __init__(self):
        super(X86Windows, self).__init__()


def build_configs(target, runtime):
    if target == 'arm-pikeos':
        t = ArmPikeOS()
    elif target == 'ppc-pikeos':
        t = PpcPikeOS()
    elif target == 'x86-pikeos':
        t = X86PikeOS()
    elif target == 'zynq7000':
        t = Zynq7000()
    elif target == 'rpi2':
        t = RPI2()
    elif target == 'rpi3':
        t = RPI3()
    elif target.startswith('sam'):
        t = Sam(target)
    elif target.startswith('smartfusion2'):
        t = SmartFusion2()
    elif target.startswith('stm32'):
        t = Stm32(target)
    elif target == 'tms570':
        t = TMS570()
    elif target == 'lm3s':
        t = LM3S()
    elif target == 'leon2' or target == 'leon':
        t = Leon2()
    elif target == 'leon3':
        t = Leon3()
    elif target == '8641d':
        t = MPC8641()
    elif target == '8349e':
        t = MPC8349e()
    elif target == 'p2020':
        t = P2020()
    elif target == 'p5566':
        t = P5566()
    elif target == 'mpc5634':
        t = P5634()
    elif target == 'mcm':
        t = Visium()
    elif target == 'x86-linux':
        t = X86Linux()
    elif target == 'x86-windows':
        t = X86Windows()
    else:
        print 'Error: undefined target %s' % target
        sys.exit(2)

    if runtime == 'zfp':
        t.init_as_zfp()
    elif runtime == 'ravenscar-sfp':
        t.init_as_sfp()
    elif runtime == 'ravenscar-full':
        t.init_as_full()
    else:
        print 'Error: unknown runtime %s' % runtime
        sys.exit(2)

    return t


def usage():
    print "usage: build-rts.py OPTIONS runtime/arch"
    print "Options are:"
    print " -v --verbose     be verbose"
    print " --output=DIR     output directory"
    print " --gcc-dir=DIR    gcc source directory"
    print " --gnat-dir=DIR   gnat source directory"
    print " --cross-dir=DIR  cross source directory"
    print " --link           create symbolic links"
    print " --create-common  create a common runtime directory"


def main():
    global link, gccdir, gnatdir, crossdir, verbose, create_common

    install = objdir

    try:
        opts, args = getopt.getopt(
            sys.argv[1:], "hvl",
            ["help", "verbose",
             "output=",
             "gcc-dir=", "gnat-dir=", "cross-dir=",
             "create-common",
             "link"])
    except getopt.GetoptError, e:
        print "error: " + str(e)
        print "Try --help"
        sys.exit(2)
    for opt, arg in opts:
        if opt in ("-v", "--verbose"):
            verbose = True
        elif opt in ("-h", "--help"):
            usage()
            sys.exit()
        elif opt in ("-l", "--link"):
            link = True
        elif opt == "--output":
            install = arg
        elif opt == "--gcc-dir":
            gccdir = arg
        elif opt == "--gnat-dir":
            gnatdir = arg
        elif opt == "--cross-dir":
            crossdir = arg
        elif opt == "--create-common":
            create_common = True
        else:
            sys.abort()

    if len(args) != 1:
        print "error: missing configuration"
        print "Try --help"
        sys.exit(2)

    (rts, arch) = args[0].split('/')

    target = build_configs(arch, rts)
    target.install(install)


if __name__ == '__main__':
    main()
