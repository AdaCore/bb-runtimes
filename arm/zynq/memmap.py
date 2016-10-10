#! /usr/bin/env python
#
# Copyright (C) 2016, AdaCore
#
# Python script to generate MMU tables.

import getopt
import sys
import xml.etree.ElementTree as ET

filename = "memmap.xml"


class Arch(object):
    """Describe the architecture to build the MMU tables"""
    def pagesize(self):
        return 4096

    def insert(self, name, virt, phys, size, cache, access):
        pass

    def generate(self):
        pass


class arm_mmu(Arch):
    def __init__(self):
        # Translation table (initially empty)
        self.tt = [None for x in range(4096)]
        self.pagealign = 1 << 20

    def pagesize(self):
        # Handle only large pages
        return self.pagealign

    def insert(self, name, virt, phys, size, cache, access):
        # Convert cache
        if cache == 'cb':
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
        if access == "rwxrwx":
            ap = 3
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
        for v in range(virt, virt + size):
            if self.tt[v]:
                print "overlap at %s in region %s" % \
                  (hex(v * self.pagealign), name)
                exit(1)
            val = ((p << 20) + (ns << 19) + (nG << 17) + (S << 16) +
                   (((ap >> 2) & 1) << 15) + (tex << 12) + ((ap & 3) << 10) +
                   (domain << 5) + (nx << 4) + (c << 3) + (b << 2) + 2)
            self.tt[v] = {'name': name,
                          'format': 'section', 'virt': v, 'phys': p, 'ns': ns,
                          'nG': nG, 'S': S, 'AP': ap, 'TEX': tex,
                          'domain': domain, 'XN': nx, 'C': c, 'B': b,
                          'val': val}
            p += 1

    def generate(self):
        addr = 0
        for e in self.tt:
            if e:
                v = e['val']
                n = e['name']
            else:
                v = 0
                n = "*none*"

            print "\t.long 0x%08x  @ for 0x%08x, %s" % (v, addr, n)
            addr += self.pagealign


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
arches = {'arm': arm_mmu}


def usage():
    print "usage: memmap.py OPTIONS [INPUT]"
    print "Options are:"
    print " --arch=ARCH      set architecture"


def main():
    global filename

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

    tree = ET.parse('memmap.xml')
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
    pagealign = mmu.pagesize()

    # Create entries for each regions
    for child in root:
        name = child.attrib['name']
        virt = parse_addr(child.attrib['virt'])
        if 'phys' in child.attrib:
            phys = parse_addr(child.attrib['phys'])
        else:
            phys = virt
        size = parse_addr(child.attrib['size'])
        if (virt % pagealign) != 0:
            print "%s.virt is not aligned" % name
            exit(1)
        if (phys % pagealign) != 0:
            print "%s.phys is not aligned" % name
            exit(1)
        virt = virt / pagealign
        phys = phys / pagealign
        if (size % pagealign) != 0:
            print "size of %s is not aligned" % name
            exit(1)
        size = size / pagealign

        cache = child.attrib['cache']
        access = child.attrib['access']

        mmu.insert(name, virt, phys, size, cache, access)

    mmu.generate()

if __name__ == '__main__':
    main()
