#! /usr/bin/env python
#
# Copyright (C) 2016, AdaCore
#
# Python script to generate MMU tables.

import getopt
import sys
import xml.etree.ElementTree as ET

sys.path.append('../../arm')
import memmap


class vcpu(object):
    def __init__(self, name):
        self.name = name
        self.regions = []
        self.files = []


class file(object):
    def __init__(self, name, filename):
        self.name = name
        self.filename = filename
        self.idx = 0


def usage():
    print "usage: multilevel.py [--gen-regions | --gen-mmu] OPTIONS [INPUT]"
    print "Options are:"
    print " --arch=ARCH      set architecture"
    print "    architectures are: %s" % ", ".join(memmap.arches.keys())


def main():
    arch = None
    gen_regions = False
    gen_mmu = False
    gen_dir = False

    try:
        opts, args = getopt.getopt(
            sys.argv[1:], "h", ["help", "arch=",
                                "gen-regions", "gen-mmu", "gen-dir"])
    except getopt.GetoptError, e:
        sys.stderr.write("error: " + str(e) + '\n')
        sys.stderr.write("Try --help\n")
        sys.exit(2)
    for opt, arg in opts:
        if opt == "--arch":
            arch = arg
        elif opt in ("-h", "--help"):
            usage()
            sys.exit()
        elif opt == "--gen-regions":
            gen_regions = True
        elif opt == "--gen-mmu":
            gen_mmu = True
        elif opt == "--gen-dir":
            gen_dir = True
        else:
            sys.abort()

    if len(args) == 0:
        filename = "memmap.xml"
    elif len(args) == 1:
        filename = args[0]
    else:
        sys.stderr.write("error: too many arguments\n")
        sys.stderr.write("Try --help\n")
        sys.exit(2)

    tree = ET.parse(filename)
    root = tree.getroot()

    if root.tag != "multi-image":
        sys.stderr.write("document tag must be 'multi-image'")
        sys.exit(2)

    # Load and parse board memmap
    mmu = memmap.create_mmu_from_xml(root, arch, "el2")

    memmap_el = root.find('include')
    memmap_tree = ET.parse(memmap_el.get('file'))
    regions = memmap.parse_memmap(mmu, memmap_tree.getroot())

    core_el = root.find('core')
    parts_el = root.find('partitions')

    offsets = {}
    for r in regions:
        offsets[r.name] = r.phys

    # Parse core
    for a in core_el:
        assert a.tag == 'area'
        reg = a.attrib['region']
        size = memmap.parse_addr(a.attrib['size'])
        offsets[reg] += size

    parts = {}
    files = {}
    for child in parts_el:
        name = child.attrib['name']
        v = vcpu(name)
        parts[name] = v
        for a in child:
            if a.tag == 'area':
                size = memmap.parse_addr(a.attrib['size'])
                vaddr = memmap.parse_addr(a.attrib['vaddr'])
                reg = a.attrib['region']
                name = a.attrib['name']
                v.regions.append(
                    memmap.mmu_region(name, vaddr, offsets[reg], size,
                                      'cache', 'acc'))
                offsets[reg] += size
            elif a.tag == 'file':
                filename = a.attrib['filename']
                name = a.attrib['name']
                f = files.get(name)
                if not f:
                    f = file(name, filename)
                    files[name] = f
                v.files.append(f)
            else:
                sys.stderr.write("unhandled partition tag {}".format(a.tag))
                sys.exit(2)

#    if base > main_region.size:
#        sys.stderr.write("image size is too large")
#        sys.exit(2)

    if gen_regions:
        # Per partition linker script (memory definition)
        for k, v in parts.iteritems():
            filename = '{}.ld'.format(k)
            print "Writing {}".format(filename)
            with open(filename, 'w') as f:
                f.write("MEMORY\n")
                f.write("{\n")
                for a in v.regions:
                    f.write(
                        "  {} (rwx): ORIGIN = {:#x}, LENGTH = {:#x}\n".format(
                            a.name, a.virt, a.size))
                f.write("}\n")

    if gen_mmu:
        # Core mmu definition
        for r in regions:
            mmu.insert(r.name, r.virt, r.phys, r.size, r.cache, r.access)
        mmu.generate("__mmu")

    if gen_dir:
        print '\t.section .rodata,"a"'

        idx = 1
        for f in files.itervalues():
            f.idx = idx
            idx += 1

            sym = "__files_{}_name".format(f.idx)
            print "\t.type {}, @object".format(sym)
            print "{}:".format(sym)
            print "\t.asciz \"{}\"".format(f.name)
            print "\t.size {}, . - {}".format(sym, sym)

            sym = "__files_{}_content".format(f.idx)
            print "\t.type {}, @object".format(sym)
            print "{}:".format(sym)
            print "\t.incbin \"{}\"".format(f.filename)
            print "{}_end:".format(sym)
            print "\t.size {}, . - {}".format(sym, sym)
        print

        for k, v in parts.iteritems():
            mmu = memmap.create_mmu_from_xml(root, arch, "stage2")
            for a in v.regions:
                mmu.insert(a.name, a.virt, a.phys, a.size, "wb", "rwx---")
            v.mmu_sym = mmu.generate("__mmu_{}".format(k))

        for k, v in parts.iteritems():
            print "\t.type __memmap_{}, @object".format(k)
            print "__memmap_{}:".format(k)
            for a in v.regions:
                print "\t.dword {:#x}".format(a.virt)
                print "\t.dword {:#x}".format(a.phys)
                print "\t.dword {:#x}".format(a.size)
            print "\t.size __memmap_{}, . - __memmap_{}".format(k, k)

            sym = "__files_{}".format(k)
            print "\t.type {}, @object".format(sym)
            print "{}:".format(sym)
            for f in v.files:
                print "\t.dword __files_{}_name".format(f.idx)
                fsym = "__files_{}_content".format(f.idx)
                print "\t.dword {}".format(fsym)
                print "\t.dword {}_end - {}".format(fsym, fsym)
            print "\t.size {}, . - {}".format(sym, sym)
            print

            print "\t.globl __dir_{}".format(k)
            print "\t.type __dir_{}, @object".format(k)
            print "__dir_{}:".format(k)
            print "\t.dword {}".format(v.mmu_sym)
            print "\t.dword __memmap_{}".format(k)
            print "\t.word {}".format(len(v.regions))
            print "\t.word 0"
            print "\t.dword __files_{}".format(k)
            print "\t.word {}".format(len(v.files))
            print "\t.word 0"
            print "\t.size __dir_{}, . - __dir_{}".format(k, k)
            print

if __name__ == '__main__':
    main()
