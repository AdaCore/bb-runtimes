#! /usr/bin/env python
#
# Copyright (C) 2016, AdaCore
#
# Python script to generate MMU tables.

import getopt
import sys
import xml.etree.ElementTree as ET

filename = "memmap.xml"

pagesize = 0
pageshift = 0


class Arch(object):
    """Describe the architecture to build the MMU tables"""
    def pageshift(self):
        return 12

    def insert(self, name, virt, phys, size, cache, access):
        pass

    def generate(self):
        pass


class arm_mmu(Arch):
    def __init__(self):
        # Translation table (initially empty)
        self.tt = [None for x in range(4096)]

    def pageshift(self):
        # Handle only large pages
        return 20

    def insert(self, name, virt, phys, size, cache, access):
        global pagesize

        # Convert cache
        if cache == 'wb':
            tex = 7
            c = 1
            b = 1
        elif cache == 'nc':
            # Must be nGnR nE
            tex = 0
            c = 0
            b = 1
        else:
            print "unhandled cache attribute '%s' for region %s" % \
              (cache, name)
            exit(1)

        # Convert access
        if access == "rwx---":
            ap = 3  # To be checked
            nx = 0
        elif access == "rw-rw-":
            ap = 3
            nx = 1
        else:
            print "unhandled access '%s' for region %s" % (access, name)
            exit(1)
        ns = 0      # Not secure
        nG = 0      # Not global
        S = 1       # Shareable (ignored for device)
        domain = 0

        # Fill tt
        p = phys
        for v in range(virt, virt + size, pagesize):
            vn = v / pagesize
            if self.tt[vn]:
                print "overlap at %s in region %s" % (hex(v), name)
                exit(1)
            val = (p + (ns << 19) + (nG << 17) + (S << 16) +
                   (((ap >> 2) & 1) << 15) + (tex << 12) + ((ap & 3) << 10) +
                   (domain << 5) + (nx << 4) + (c << 3) + (b << 2) + 2)
            self.tt[vn] = {'name': name,
                           'format': 'section', 'virt': v, 'phys': p, 'ns': ns,
                           'nG': nG, 'S': S, 'AP': ap, 'TEX': tex,
                           'domain': domain, 'XN': nx, 'C': c, 'B': b,
                           'val': val}
            p += pagesize

    def generate(self):
        addr = 0
        print "\t.p2align 14"
        print "__mmu_l0:"
        for e in self.tt:
            if e:
                v = e['val']
                n = e['name']
            else:
                v = 0
                n = "*none*"

            print "\t.long 0x%08x  @ for 0x%08x, %s" % (v, addr, n)
            addr += pagesize


class aarch64_pge(object):
    def __init__(self, name, va, pa,
                 uxn, pxn, cont, nG, AF, SH, AP, NS, attridx, log2_sz):
        self.name = name
        self.va = va
        self.pa = pa
        self.uxn = uxn
        self.pxn = pxn
        self.cont = cont
        self.nG = nG
        self.AF = AF
        self.SH = SH
        self.AP = AP
        self.NS = NS
        self.attridx = attridx
        self.log2_sz = log2_sz
        if log2_sz == pageshift:
            # A level-3 descriptor
            bt = 0x3
        else:
            # A block descriptor
            bt = 0x1
        self.val = ((uxn << 54) + (pxn << 53) + (cont << 52) +
                    (nG << 11) + (AF << 10) + (SH << 8) + (AP << 6) +
                    (NS << 5) + (attridx << 2) +
                    (pa & 0x0000fffffffff000) + bt)

    def generate_table(self, level):
        pass

    def generate_entry(self, level):
        print "\t.dword 0x%016x  // for 0x%08x, %s" % \
          (self.val, self.va, self.name)


class aarch64_pgd(object):
    def __init__(self, log2_granule, log2_entries, va, va_shift):
        self.tt = [None for x in range(1 << log2_entries)]
        self.va = va
        self.va_shift = va_shift
        self.log2_entries = log2_entries
        self.log2_granule = log2_granule

    def generate_entry(self, level):
        # NSTable: 1
        # APTable: 00 (no effect)
        # XNTable: 0
        # PXNTable: 0
        v = 0x3 + (1 << 63)
        print "\t.dword __mmu_l%d_%09x + 0x%x" % \
            (level, self.va >> pageshift, v)

    def generate_table(self, level):
        # First the next level
        for t1 in self.tt:
            if t1:
                t1.generate_table(level + 1)
        # then the pgd
        print "\t.p2align %d" % pageshift
        print "__mmu_l%d_%09x:" % (level, self.va >> pageshift)
        for t1 in self.tt:
            if t1:
                t1.generate_entry(level + 1)
            else:
                print "\t.dword 0"


class aarch64_mmu(Arch):
    def __init__(self):
        # Translation table (initially empty)
        self.log2_granule = 12  # Page size
        self.log2_entries = self.log2_granule - 3   # log2 nbr entries per page
        self.tt = aarch64_pgd(self.log2_granule,
                              self.log2_entries, 0, 48 - self.log2_entries)

    def pageshift(self):
        # Handle only large pages
        return self.log2_granule

    def insert(self, name, virt, phys, size, cache, access):
        # Convert cache
        if cache == 'wb':
            attridx = 0
        elif cache == 'nc':
            attridx = 1
        else:
            print "unhandled cache attribute '%s' for region %s" % \
              (cache, name)
            exit(1)

        # Convert access
        if access == "rwx---":
            AP = 0
            uxn = 0
            pxn = 0
        elif access == "rw-rw-":
            AP = 1
            uxn = 1
            pxn = 1
        else:
            print "unhandled access '%s' for region %s" % (access, name)
            exit(1)
        aptable = 0
        cont = 0    # Not contiguous (by default)
        nG = 0      # Not global
        AF = 1		# Access flag (don't care)
        SH = 2		# Shareability
        NS = 1      # Not secure

        # Fill tt
        pa = phys
        va = virt
        block2_log2_size = 2 * self.log2_entries + self.log2_granule
        block2_size = 1 << block2_log2_size
        block1_log2_size = self.log2_entries + self.log2_granule
        block1_size = 1 << block1_log2_size
        while va < virt + size:
            if va % block2_size == 0 and size % block2_size == 0:
                sz = block2_log2_size
            elif va % block1_size == 0 and size % block1_size == 0:
                sz = block1_log2_size
            else:
                sz = self.log2_granule
            e = aarch64_pge(name=name, va=va, pa=pa,
                            uxn=uxn, pxn=pxn, cont=cont,
                            nG=nG, AF=AF, SH=SH, AP=AP, NS=NS,
                            attridx=attridx, log2_sz=sz)

            self.insert_entry(self.tt, e)
            pa += 1 << sz
            va += 1 << sz

    def insert_entry(self, t, e):
        ia = (e.va >> t.va_shift) & ((1 << self.log2_entries) - 1)
        if t.va_shift == e.log2_sz:
            # E must be added in that table
            if t.tt[ia]:
                print "overlap at %s in region %s" % (hex(e.va), e.name)
                exit(1)
            t.tt[ia] = e
        elif isinstance(t.tt[ia], aarch64_pge):
            # There is already a superpage
            print "overlap at %s in region %s" % (hex(e.va), e.name)
            exit(1)
        else:
            if not t.tt[ia]:
                # Create table
                nsh = t.va_shift - self.log2_entries
                va = (e.va >> nsh) << nsh
                t.tt[ia] = aarch64_pgd(self.log2_granule, self.log2_entries,
                                       va, nsh)
            self.insert_entry(t.tt[ia], e)

    def generate(self):
        #  Look for the max size.
        level = 1
        va_max = 48
        sz = 1 << self.log2_entries
        t = self.tt
        while True:
            # Not empty
            if [True for e in t.tt[sz >> 1:sz] if e]:
                break
            if sz == 2:
                if level == 2:
                    break
                print "// Skip level %d" % level
                level += 1
                t = t.tt[0]
                sz = 1 << self.log2_entries
            else:
                sz = sz >> 1
            va_max -= 1
        print "// Maz VA size: 2**%d, (sz = %d)" % (va_max, sz)
        t.tt = t.tt[0:sz]
        t.generate_table(level)


def parse_addr(str):
    """Helper to translate a string to a number, handling memory suffixes"""
    ustr = str.upper()
    if ustr.startswith("0X"):
        return int(ustr, 16)
    elif ustr.endswith("GB"):
        return int(ustr[:-2]) << 30
    elif ustr.endswith("MB"):
        return int(ustr[:-2]) << 20
    elif ustr.endswith("KB"):
        return int(ustr[:-2]) << 10
    else:
        return int(ustr)


# Supported architectures
arches = {'arm': arm_mmu,
          'aarch64': aarch64_mmu}


def usage():
    print "usage: memmap.py OPTIONS [INPUT]"
    print "Options are:"
    print " --arch=ARCH      set architecture"
    print "    architectures are: %s" % ", ".join(arches.keys())


def main():
    global filename
    global pageshift
    global pagesize

    arch = None

    try:
        opts, args = getopt.getopt(
            sys.argv[1:], "h", ["help", "arch="])
    except getopt.GetoptError, e:
        print "error: " + str(e)
        print "Try --help"
        sys.exit(2)
    for opt, arg in opts:
        if opt == "--arch":
            arch = arg
        elif opt in ("-h", "--help"):
            usage()
            sys.exit()
        else:
            sys.abort()

    if len(args) == 0:
        filename = "memmap.xml"
    elif len(args) == 1:
        filename = args[0]
    else:
        print "error: too many arguments"
        print "Try --help"
        sys.exit(2)

    tree = ET.parse(filename)
    root = tree.getroot()

    if not arch:
        if 'arch' in root.attrib:
            arch = root.attrib['arch']
        else:
            print "error: unknown architecture"
            print "Use --arch or set arch attribute"
            sys.exit(3)

    if arch not in arches:
        print "error: unknown architecture '%s'" % arch
        sys.exit(1)

    mmu = arches[arch]()
    pageshift = mmu.pageshift()
    pagesize = 1 << pageshift

    # Create entries for each regions
    for child in root:
        name = child.attrib['name']
        virt = parse_addr(child.attrib['virt'])
        if 'phys' in child.attrib:
            phys = parse_addr(child.attrib['phys'])
        else:
            phys = virt
        size = parse_addr(child.attrib['size'])
        if (virt % pagesize) != 0:
            print "%s.virt is not aligned" % name
            exit(1)
        if (phys % pagesize) != 0:
            print "%s.phys is not aligned" % name
            exit(1)
        if (size % pagesize) != 0:
            print "size of %s is not aligned" % name
            exit(1)

        cache = child.attrib['cache']
        access = child.attrib['access']

        mmu.insert(name, virt, phys, size, cache, access)

    mmu.generate()

if __name__ == '__main__':
    main()
