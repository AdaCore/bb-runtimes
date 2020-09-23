# BSP support for m68k
from support.bsp_sources.archsupport import ArchSupport
from support.bsp_sources.target import DFBBTarget


class M68KArch(ArchSupport):
    @property
    def name(self):
        return "m68k"

    def __init__(self):
        super(M68KArch, self).__init__()
        # self.add_linker_switch('-Wl,-u_start', loader=None)
        # self.add_sources('arch', [
        #    'sparc/leon/crt0.S',
        #    'sparc/leon/hw_init.S',
        #    'sparc/src/sparc.h',
        #    'src/s-macres__leon.adb'])


class M68KTarget(DFBBTarget):
    @property
    def target(self):
        return 'm68k-elf'

    @property
    def parent(self):
        return M68KArch

    @property
    def system_ads(self):
        return {
            'zfp': 'system-xi-m68k.ads',
        }


class MC68020(M68KTarget):
    @property
    def name(self):
        return "mc68020"

    @property
    def parent(self):
        return M68KArch

    @property
    def readme_file(self):
        return 'm68k/mc68020/README'

    @property
    def loaders(self):
        return ('RAM', )

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mc68020', )

    def __init__(self):
        super(MC68020, self).__init__()

        self.add_linker_script('m68k/m68020/memory-map.ld')
        self.add_linker_script('m68k/m68020/common-RAM.ld', loader='RAM')
        self.add_gnat_sources(
            'm68k/m68020/mc68901.ads',
            'm68k/m68020/start-ram.S',
            'm68k/m68020/s-macres.adb',
            'm68k/m68020/s-textio.adb')
