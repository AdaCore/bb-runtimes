from support import readfile
from support.bsp_sources.target import DFBBTarget


class RiscV64(DFBBTarget):
    @property
    def name(self):
        return "riscv64"

    @property
    def target(self):
        return "riscv64-elf"

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
        return {"light": "system-xi-riscv.ads"}

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(RiscV64, self).dump_runtime_xml(rts_name, rts)
        if rts_name == "embedded":
            cnt = cnt.replace(
                '"-nostartfiles"', '"--specs=${RUNTIME_DIR(ada)}/link-zcx.spec"'
            )
        return cnt

    def amend_rts(self, rts_profile, conf):
        super(DFBBTarget, self).amend_rts(rts_profile, conf)
        if rts_profile == "embedded":
            conf.config_files.update({"link-zcx.spec": readfile("riscv/link-zcx.spec")})


class Spike(RiscV64):
    @property
    def name(self):
        return "spike"

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mcmodel=medany",)

    @property
    def loaders(self):
        return ("RAM",)

    def __init__(self):
        super(Spike, self).__init__()
        self.add_linker_script("riscv/spike/memory-map.ld")
        self.add_linker_script("riscv/spike/common-RAM.ld", loader="RAM")
        self.add_gnat_sources(
            "riscv/start-ram.S",
            "riscv/src/riscv_host_target_interface.ads",
            "riscv/src/riscv_host_target_interface.adb",
            "src/s-macres__riscv-htif.adb",
            "src/s-textio__riscv-htif.adb",
        )


class PolarFireSOC(RiscV64):
    @property
    def name(self):
        return "polarfiresoc"

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-march=rv64imafdc", "-mabi=lp64d")

    @property
    def system_ads(self):
        return {
            "light": "system-xi-riscv.ads",
            "light-tasking": "system-xi-riscv-sifive-sfp.ads",
            "embedded": "system-xi-riscv-sifive-full.ads",
        }

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True

    @property
    def loaders(self):
        return ("RAM",)

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(PolarFireSOC, self).dump_runtime_xml(rts_name, rts)
        return cnt.replace(
            '"common-RAM.ld"',
            '"common-RAM.ld",\n'
            "               --  This symbol is used by GDB to know the\n"
            "               --  hardware id of the first CPU used by\n"
            "               --  the run-time. The -u option is used to make\n"
            "               --  sure the symbol is not discared in the final\n"
            "               --  binary.\n"
            '               "-u", "__gnat_gdb_cpu_first_id"',
        )

    def __init__(self):
        super(PolarFireSOC, self).__init__()

        self.add_linker_script("riscv/microchip/polarfiresoc/memory-map.ld")
        self.add_linker_script(
            "riscv/microchip/polarfiresoc/common-RAM.ld", loader="RAM"
        )
        self.add_gnat_sources(
            "riscv/microchip/polarfiresoc/start-ram.S",
            "riscv/sifive/fe310/svd/i-fe310.ads",
            "riscv/sifive/fe310/svd/i-fe310-plic.ads",
            "riscv/sifive/fe310/s-macres.adb",
            "src/s-textio__ns16550.adb",
            "src/s-bbbopa__polarfiresoc.ads",
            "riscv/src/riscv_def.h",
        )
        self.add_gnarl_sources(
            "riscv/microchip/polarfiresoc/a-intnam.ads",
            "src/s-bbpara__polarfiresoc.ads",
            "src/s-bbbosu__riscv.adb",
            "src/s-bbsuti__riscv_clint.adb",
            "src/s-bbsumu__generic.adb",
            "src/s-bbcppr__new.ads",
            "src/s-bbcppr__riscv.adb",
            "src/s-bbcpsp__riscv.ads",
            "src/s-bbcpsp__riscv.adb",
            "riscv/src/context_switch.S",
            "riscv/src/trap_handler.S",
            "riscv/src/s-bbripl.ads",
            "riscv/microchip/polarfiresoc/s-bbripl.adb",
        )


class RiscV32(DFBBTarget):
    @property
    def name(self):
        return "riscv32"

    @property
    def target(self):
        return "riscv32-elf"

    @property
    def has_timer_64(self):
        return True


class HiFive1(RiscV32):
    @property
    def name(self):
        return "hifive1"

    @property
    def has_single_precision_fpu(self):
        return False

    @property
    def has_double_precision_fpu(self):
        return False

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-march=rv32imac", "-mabi=ilp32")

    @property
    def has_small_memory(self):
        return True

    @property
    def loaders(self):
        return ("ROM",)

    @property
    def system_ads(self):
        # Only the Light runtime for this target, the tasking and embedded
        # run-times are not stable for the moment.
        return {"light": "system-xi-riscv.ads"}

    def __init__(self):
        super(HiFive1, self).__init__()
        self.add_linker_script("riscv/sifive/hifive1/memory-map.ld")
        self.add_linker_script("riscv/sifive/hifive1/common-ROM.ld", loader="ROM")
        self.add_gnat_sources(
            "riscv/sifive/fe310/start-rom.S",
            "riscv/sifive/fe310/svd/i-fe310.ads",
            "riscv/sifive/fe310/svd/i-fe310-uart.ads",
            "riscv/sifive/fe310/svd/i-fe310-gpio.ads",
            "riscv/sifive/fe310/svd/i-fe310-plic.ads",
            "riscv/sifive/fe310/s-macres.adb",
            "riscv/sifive/hifive1/s-textio.adb",
            "riscv/src/riscv_def.h",
        )
        self.add_gnarl_sources(
            "riscv/sifive/fe310/svd/a-intnam.ads",
            "src/s-bbpara__riscv.ads",
            "src/s-bbbopa__hifive1.ads",
            "src/s-bbbosu__riscv.adb",
            "src/s-bbsuti__riscv_clint.adb",
            "src/s-bbsumu__generic.adb",
            "src/s-bbcppr__new.ads",
            "src/s-bbcppr__riscv.adb",
            "src/s-bbcpsp__riscv.ads",
            "src/s-bbcpsp__riscv.adb",
            "riscv/src/context_switch.S",
            "riscv/src/trap_handler.S",
            "riscv/src/s-bbripl.ads",
            "riscv/sifive/fe310/s-bbripl.adb",
        )


class RV32BASE(RiscV32):
    """
    Generic Light run-time meant to be used with the startup generator (crt0 and
    ld script).
    """

    @property
    def has_small_memory(self):
        return True

    @property
    def has_single_precision_fpu(self):
        return False

    @property
    def has_double_precision_fpu(self):
        return False

    @property
    def system_ads(self):
        # Only the Light runtime for for this generic target
        return {"light": "system-xi-riscv.ads"}

    def __init__(self):
        super(RV32BASE, self).__init__()

        self.add_gnat_sources(
            "riscv/sifive/fe310/s-macres.adb", "src/s-textio__stdio.adb"
        )


class RV32I(RV32BASE):
    @property
    def name(self):
        return "rv32i"

    @property
    def compiler_switches(self):
        return ("-march=rv32i", "-mabi=ilp32")


class RV32IM(RV32BASE):
    @property
    def name(self):
        return "rv32im"

    @property
    def compiler_switches(self):
        return ("-march=rv32im", "-mabi=ilp32")


class RV32IAC(RV32BASE):
    @property
    def name(self):
        return "rv32iac"

    @property
    def compiler_switches(self):
        return ("-march=rv32iac", "-mabi=ilp32")


class RV32IMAC(RV32BASE):
    @property
    def name(self):
        return "rv32imac"

    @property
    def compiler_switches(self):
        return ("-march=rv32imac", "-mabi=ilp32")


class RV32IMAFC(RV32BASE):
    @property
    def name(self):
        return "rv32imafc"

    @property
    def compiler_switches(self):
        return ("-march=rv32imafc", "-mabi=ilp32f")

    @property
    def has_single_precision_fpu(self):
        return True


class RV32IMAFDC(RV32BASE):
    @property
    def name(self):
        return "rv32imafdc"

    @property
    def compiler_switches(self):
        return ("-march=rv32imafdc", "-mabi=ilp32d")

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True


class RV64BASE(RiscV64):
    """
    Generic Light run-time meant to be used with the startup generator (crt0 and
    ld script).
    """

    @property
    def has_single_precision_fpu(self):
        return False

    @property
    def has_double_precision_fpu(self):
        return False

    @property
    def system_ads(self):
        # Only the Light runtime for for this generic target
        return {"light": "system-xi-riscv.ads"}

    def __init__(self):
        super(RV64BASE, self).__init__()

        self.add_gnat_sources(
            "riscv/sifive/fe310/s-macres.adb", "src/s-textio__stdio.adb"
        )


class RV64IM(RV64BASE):
    @property
    def name(self):
        return "rv64im"

    @property
    def compiler_switches(self):
        return ("-march=rv64im", "-mabi=lp64")


class RV64IMC(RV64BASE):
    @property
    def name(self):
        return "rv64imc"

    @property
    def compiler_switches(self):
        return ("-march=rv64imc", "-mabi=lp64")


class RV64IMAC(RV64BASE):
    @property
    def name(self):
        return "rv64imac"

    @property
    def compiler_switches(self):
        return ("-march=rv64imac", "-mabi=lp64")


class RV64IMAFC(RV64BASE):
    @property
    def name(self):
        return "rv64imafc"

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def compiler_switches(self):
        return ("-march=rv64imafc", "-mabi=lp64f")


class RV64IMFC(RV64BASE):
    @property
    def name(self):
        return "rv64imfc"

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def compiler_switches(self):
        return ("-march=rv64imfc", "-mabi=lp64f")


class RV64IMAFDC(RV64BASE):
    @property
    def name(self):
        return "rv64imafdc"

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True

    @property
    def compiler_switches(self):
        return ("-march=rv64imafdc", "-mabi=lp64d")
