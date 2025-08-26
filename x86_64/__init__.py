# BSP support for x86_64
from support.bsp_sources.archsupport import ArchSupport
from support.bsp_sources.target import DFBBTarget
from support import readfile


class X8664Arch(ArchSupport):
    @property
    def name(self):
        return "x86_64"

    def __init__(self):
        super(X8664Arch, self).__init__()
        self.add_gnat_sources(
            "x86_64/src/i-x86_64.adb",
            "x86_64/src/i-x86_64.ads",
            "x86_64/src/i-x86_64-ns16550.adb",
            "x86_64/src/i-x86_64-ns16550.ads",
            "x86_64/src/s-macres.adb",
            "x86_64/src/start.S",
        )
        # s-bbcppr.ads is listed under the Generic target as it will need
        # modifying if we want to support microarchitectures older than
        # Sandy Bridge since the current runtime uses XOPTSAVE in its
        # context switching routine.
        self.add_gnarl_sources("x86_64/src/vector_handlers.S")


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
            "light": "system-xi-x86_64.ads",
            "light-tasking": "system-xi-x86_64-sfp.ads",
            "embedded": "system-xi-x86_64-full.ads",
        }


class X8664Generic(X8664Target):
    @property
    def target(self):
        return "x86_64-elf"

    @property
    def name(self):
        return "x86_64"

    @property
    def parent(self):
        return X8664Arch

    @property
    def readme_file(self):
        return "x86_64/generic/README"

    @property
    def loaders(self):
        return ("QEMU", "LSA", "MULTIBOOT", "MULTIBOOT2")

    def __init__(self):
        super(X8664Generic, self).__init__()

        self.add_linker_script("x86_64/generic/memory-map.ld")
        self.add_linker_script("x86_64/generic/common-LSA.ld", loader="LSA")
        self.add_linker_script("x86_64/generic/common-MULTIBOOT.ld", loader="MULTIBOOT")
        self.add_linker_script(
            "x86_64/generic/common-MULTIBOOT2.ld", loader="MULTIBOOT2"
        )
        self.add_linker_script("x86_64/generic/common-QEMU.ld", loader="QEMU")
        self.add_linker_switch("-Wl,-z,max-page-size=0x1000")

        self.add_gnat_sources(
            "src/s-textio__com1.adb",
            "x86_64/src/lynx.S",
            "x86_64/src/multiboot.S",
            "x86_64/src/multiboot2.S",
        )
        self.add_gnarl_sources(
            "src/a-intnam__x86_64.ads",
            "src/s-bbcppr__new.ads",
            "src/s-bbbosu__x86_64.adb",
            "src/s-bbcppr__x86_64.adb",
            "src/s-bbcpsp__x86_64.adb",
            "src/s-bbcpsp__x86_64.ads",
            "src/s-bbpara__x86_64.ads",
            "src/s-bbsumu__generic.adb",
            "x86_64/src/i-x86_64-exception_handler.adb",
            "x86_64/src/i-x86_64-exception_handler.ads",
            "x86_64/src/i-x86_64-io_apic.adb",
            "x86_64/src/i-x86_64-io_apic.ads",
        )

    def amend_rts(self, rts_profile, conf):
        super(X8664Generic, self).amend_rts(rts_profile, conf)
        # crti.o and crtn.o do not exist for the x86_64 bare-metal compiler
        if "embedded" in rts_profile:
            conf.config_files.update(
                {"link-zcx.spec": readfile("x86_64/generic/link-zcx.spec")}
            )
