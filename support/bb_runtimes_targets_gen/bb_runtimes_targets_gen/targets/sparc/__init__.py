#
# Copyright (C) 2025-2026, AdaCore
#

# BSP support for Sparc/Leon
from pathlib import Path

from rts_prebuilder.engine import resolve_and_read_file
from bb_runtimes_targets_gen.concrete_infrastructure import ArchSupport, DFBBTarget


class LeonArch(ArchSupport):
    @property
    def name(self):
        return "leon"

    def __init__(self):
        super(LeonArch, self).__init__()
        self.add_linker_switch("-Wl,-u_start", loader=None)
        self.add_gnat_sources(
            "sparc/leon/crt0.S",
            "sparc/leon/hw_init.S",
            "sparc/src/sparc.h",
            "shared/s-macres__leon.adb",
        )
        self.add_gnarl_sources(
            "shared/s-bbcppr__old.ads",
            "shared/s-bbcppr__sparc.adb",
            "shared/s-bcpith__sparc.adb",
            "shared/s-bbcpsp__leon.ads",
            "sparc/src/context_switch.S",
            "sparc/src/trap_handler.S",
            "sparc/src/interrupt_masking.S",
            "shared/s-bbcaco.ads",
            "shared/s-bbcaco__leon.adb",
        )


class LeonTarget(DFBBTarget):
    @property
    def parent(self):
        return LeonArch()

    @property
    def system_ads(self):
        return {
            "light": "system-xi-sparc.ads",
            "light-tasking": "system-xi-sparc-ravenscar.ads",
            "embedded": "system-xi-sparc-full.ads",
        }

    def amend_rts(self, rts_profile, conf):
        super(LeonTarget, self).amend_rts(rts_profile, conf)
        if rts_profile in ["light", "light-tasking"]:
            # Constructors and destructors are executed from the .ctors and
            # .dtors sections, so we need the crti.o and crtn.o objects.
            conf.config_files.update(
                {
                    "link-noexceptions.spec": resolve_and_read_file(
                        Path("sparc/leon/link-noexceptions.spec")
                    )
                }
            )


class Leon2(LeonTarget):
    @property
    def name(self):
        return "leon"

    @property
    def target(self):
        return "leon-elf"

    @property
    def c_switches(self):
        # The required compiler switches
        return ("-DLEON", "-DLEON2")

    def __init__(self):
        super(Leon2, self).__init__()

        self.add_linker_script("sparc/leon/leon.ld")
        self.add_gnat_sources("shared/s-textio__leon.adb", "shared/s-bbbopa__leon.ads")
        self.add_gnarl_sources(
            "shared/s-bbsumu__generic.adb",
            "shared/s-bbsule__leon.ads",
            "shared/s-bbbosu__leon.adb",
            "shared/s-bbpara__leon.ads",
            "shared/a-intnam__leon.ads",
        )


class Leon3or4(LeonTarget):
    """
    Common class for leon3 and leon4 targets
    """

    def __init__(self, smp):
        super().__init__()
        self.smp = smp
        self.add_linker_script("sparc/leon3/leon.ld")
        self.add_gnat_sources(
            "shared/s-textio__leon3.adb",
            "shared/i-leon3.ads",
            "shared/i-leon3-uart.ads",
            "shared/i-leon3-cache.ads",
        )
        self.add_gnarl_sources(
            "shared/i-leon3-timers.ads",
            "shared/i-leon3-irqmp.ads",
            "shared/s-bbbosu__leon3.adb",
            "shared/s-bbpara__leon.ads",
            "shared/a-intnam__leon3.ads",
        )

    @property
    def system_ads(self):
        ret = super().system_ads
        if self.smp:
            # The Light runtime makes no sense in the context of SMP variant
            del ret["light"]
        return ret

    @property
    def need_fix_ut699(self):
        return True

    @property
    def c_switches(self):
        # The required compiler switches
        res = ("-DLEON", "-DLEON3")
        if self.need_fix_ut699:
            res += ("-DFIX_UT699",)
        return res

    @property
    def compiler_switches(self):
        ret = ()
        if not self.smp:
            # see R409-022: -mcpu=leon3 makes gcc generate CASA instruction
            # when expanding compare_and_swap_4 intrinsic, which is invalid
            # SPARCv8 insn on most leon3.
            ret += ("-mcpu=leon",)
        else:
            ret += ("-mcpu=leon3",)

        if self.need_fix_ut699:
            ret += ("-mfix-ut699",)
        return ret

    @property
    def has_single_precision_fpu(self) -> bool:
        # Single precision sqrt is buggy on UT699
        return not self.need_fix_ut699

    @property
    def has_compare_and_swap(self) -> bool:
        if not self.smp:
            # see R409-022
            return False
        else:
            return True

    @property
    def readme_file(self):
        return Path("sparc/leon3/README")

    @property
    def target(self):
        return "leon3-elf"


class Leon3(Leon3or4):
    @property
    def name(self):
        if self.smp:
            return "leon3-smp"
        else:
            return "leon3"

    def __init__(self, smp):
        self.smp = smp
        super().__init__(smp)
        self.add_gnat_source(f"shared/s-bbbopa__leon3-{'smp' if smp else 'up'}.ads")


class Leon4(Leon3or4):
    @property
    def name(self):
        if self.smp:
            return "leon4-smp"
        else:
            return "leon4"

    @property
    def need_fix_ut699(self):
        return False

    def __init__(self, smp):
        self.smp = smp
        super().__init__(smp)

        self.add_gnat_source(f"shared/s-bbbopa__leon4-{'smp' if smp else 'up'}.ads")


TARGETS = [
    Leon2(),
    Leon3(smp=False),
    Leon3(smp=True),
    Leon4(smp=False),
    Leon4(smp=True),
]
