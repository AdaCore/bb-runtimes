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
            'x86_64/src/i-x86_64-ns16550.adb',
            'x86_64/src/i-x86_64-ns16550.ads',
            'x86_64/src/s-macres.adb',
            'x86_64/src/start.S')
        # s-bbcppr.ads is listed under the Generic target as it will need
        # modifying if we want to support microarchitectures older than
        # Sandy Bridge since the current runtime uses XOPTSAVE in its
        # context switching routine.
        self.add_gnarl_sources(
            'x86_64/src/vector_table.S')


class X8664Target(DFBBTarget):
    @property
    def parent(self):
        return X8664Arch

    @property
    def has_huge_memory(self):
        return True

    @property
    def has_timer_64(self):
        return True

    @property
    def is_64bit(self):
        return True

    @property
    def system_ads(self):
        return {
            'zfp': 'system-xi-x86_64.ads',
            'ravenscar-sfp': 'system-xi-x86_64-sfp.ads',
            'ravenscar-full': 'system-xi-x86_64-full.ads'
        }

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(X8664Target, self).dump_runtime_xml(rts_name, rts)
        if rts_name == 'ravenscar-full':
            cnt = cnt.replace(
                '"-nostartfiles"',
                ('"-u", "_Unwind_Find_FDE", "-Wl,--eh-frame-hdr",\n'
                 '        "-nostartfiles"'))
        return cnt


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
        self.add_gnarl_sources(
            'src/a-intnam__x86_64.ads',
            'src/s-bbcppr__new.ads',
            'src/s-bbbosu__x86_64.adb',
            'src/s-bbcppr__x86_64.adb',
            'src/s-bbcpsp__x86_64.adb',
            'src/s-bbcpsp__x86_64.ads',
            'src/s-bbpara__x86_64.ads',
            'src/s-bbsumu__generic.adb',
            'x86_64/src/i-x86_64-exception_handler.adb',
            'x86_64/src/i-x86_64-exception_handler.ads')
