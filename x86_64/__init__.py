# BSP support for x86_64
from support.bsp_sources.archsupport import ArchSupport
from support.bsp_sources.target import DFBBTarget


class X8664Arch(ArchSupport):
    @property
    def name(self):
        return "x86_64"

    def __init__(self):
        super(X8664Arch, self).__init__()
        self.add_gnat_sources(
            'x86_64/src/i-x86_64.adb',
            'x86_64/src/i-x86_64.ads',
            'x86_64/src/ns16550.adb',
            'x86_64/src/ns16550.ads',
            'x86_64/src/s-macres.adb',
            'x86_64/src/start.S')


class X8664Target(DFBBTarget):
    @property
    def parent(self):
        return X8664Arch

    @property
    def has_huge_memory(self):
        return True

    @property
    def system_ads(self):
        return {
            'zfp': 'system-xi-x86_64.ads',
        }


class X8664Generic(X8664Target):
    @property
    def target(self):
        return 'x86_64-elf'

    @property
    def name(self):
        return "x86_64"

    @property
    def parent(self):
        return X8664Arch

    @property
    def readme_file(self):
        return 'x86_64/generic/README'

    @property
    def loaders(self):
        return ('RAM', )

    def __init__(self):
        super(X8664Generic, self).__init__()

        self.add_linker_script('x86_64/generic/memory-map.ld')
        self.add_linker_script('x86_64/generic/common-RAM.ld', loader='RAM')

        self.add_gnat_sources(
            'src/s-textio__bios.adb')
