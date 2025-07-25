#! /usr/bin/env python3
#
# Copyright (C) 2016-2020, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

from support import Compiler, set_target_compiler
from support.files_holder import FilesHolder
from support.bsp_sources.installer import Installer
from support.docgen import docgen

# PikeOS
from pikeos import AArch64PikeOS5, ArmPikeOS, ArmPikeOS42, ArmPikeOS5, PPCPikeOS5

# Cortex-M runtimes
from arm.cortexm import (
    Stm32,
    Stm32l,
    Sam,
    SmartFusion2,
    LM3S,
    Microbit,
    NRF52840,
    NRF52832,
    NRF52833,
    MicrosemiM1,
    Stm32F0,
    RP2040Target,
    CortexM0,
    CortexM0P,
    CortexM1,
    CortexM3,
    CortexM4,
    CortexM4F,
    CortexM7F,
    CortexM7DF,
    CortexM23,
    CortexM33F,
    CortexM33DF,
)

# Cortex-A/R runtimes
from arm.cortexar import AM64xR5, TMS570, Rpi2, Rpi2Mc, Zynq7000, ZynqmpR5

# Aarch64
from aarch64 import Morello, Rpi3, Rpi3Mc, ZynqMP

# Deos
from deos import ArmDeos

# leon
from sparc import Leon2, Leon3, Leon4

# powerpc
from powerpc import MPC5200, MPC8641, MPC8349e, Virtex5, P2020, P5566, P5634

# riscv
from riscv import (
    HiFive1,
    MIV_RV32IMAF,
    PolarFireSOC,
    Spike,
    RV32I,
    RV32IM,
    RV32IAC,
    RV32IMAC,
    RV32IMAFC,
    RV32IMAFDC,
    RV64IM,
    RV64IMC,
    RV64IMAC,
    RV64IMAFC,
    RV64IMFC,
    RV64IMAFDC,
)

# visium
from visium import Visium

# x86_64
from x86_64 import X8664Generic

# native
from linux import Aarch64Linux, X86Linux, X8664Linux
from windows import X86Windows, X8664Windows

# vx7r2cert
from vx7r2cert import (
    AArch64Vx7r2Cert,
    MorelloVx7r2Cert,
    ArmVx7r2Cert,
    PPCVx7r2Cert,
    PPC64Vx7r2Cert,
    X86Vx7r2Cert,
    X86_64Vx7r2Cert,
    AArch64Vx7r2Cert_RTP,
    MorelloVx7r2Cert_RTP,
    ArmVx7r2Cert_RTP,
    PPCVx7r2Cert_RTP,
    PPC64Vx7r2Cert_RTP,
    X86Vx7r2Cert_RTP,
    X86_64Vx7r2Cert_RTP,
)

from qnx import Aarch64QNX, ARMQNX

from freertos import ArmV7AFP_FreeRTOS

from lynx import PPCLynx, Aarch64Lynx

import argparse
import importlib
import os
import subprocess
import sys


def build_configs(target):
    # PikeOS
    if target == "aarch64-pikeos5":
        t = AArch64PikeOS5()
    elif target == "arm-pikeos":
        t = ArmPikeOS()
    elif target == "arm-pikeos4.2":
        t = ArmPikeOS42()
    elif target == "arm-pikeos5":
        t = ArmPikeOS5()
    elif target == "ppc-pikeos5":
        t = PPCPikeOS5()
    # FreeRTOS
    elif target == "arm-freertos-v7a-fp":
        t = ArmV7AFP_FreeRTOS()
    # AArch64 elf
    elif target == "morello":
        t = Morello(uart_io=True)
    elif target == "morello-semihosting":
        t = Morello(uart_io=False)
    elif target == "rpi3":
        t = Rpi3()
    elif target == "rpi3mc":
        t = Rpi3Mc()
    elif target == "zynqmp":
        t = ZynqMP()
    # ARM elf
    elif target == "am64xr5":
        t = AM64xR5()
    elif target == "zynq7000":
        t = Zynq7000()
    elif target == "zynqmpr5":
        t = ZynqmpR5()
    elif target == "rpi2":
        t = Rpi2()
    elif target == "rpi2mc":
        t = Rpi2Mc()
    elif target in RP2040Target.supported_targets:
        t = RP2040Target(target)
    elif target.startswith("sam"):
        t = Sam(target)
    elif target.startswith("smartfusion2"):
        t = SmartFusion2()
    elif target.startswith("stm32f0"):
        t = Stm32F0(target)
    elif target.startswith("stm32l"):
        t = Stm32l(target)
    elif target.startswith("stm32"):
        t = Stm32(target)
    elif target == "feather_stm32f405":
        t = Stm32(target)
    elif target == "nucleo_f401re":
        t = Stm32(target)
    elif target == "openmv2":
        t = Stm32(target)
    elif target == "tms570":
        # by default, the TMS570LS3137 HDK board
        t = TMS570("tms570ls31")
    elif target == "tms570_sci":
        # by default, the TMS570LS3137 HDK board
        t = TMS570("tms570ls31", uart_io=True)
    elif target == "tms570lc":
        # alias for the TMS570LC43x HDK board
        t = TMS570("tms570lc43", uart_io=True)
    elif target == "tms570lc_dcc":
        t = TMS570("tms570lc43", uart_io=False)
    elif target == "lm3s":
        t = LM3S()
    elif target == "microbit":
        t = Microbit()
    elif target == "nrf52833":
        t = NRF52833()
    elif target == "nrf52840":
        t = NRF52840()
    elif target == "nrf52832":
        t = NRF52832()
    elif target == "microsemi-m1":
        t = MicrosemiM1()
    elif target == "cortex-m0":
        t = CortexM0()
    elif target == "cortex-m0p":
        t = CortexM0P()
    elif target == "cortex-m1":
        t = CortexM1()
    elif target == "cortex-m3":
        t = CortexM3()
    elif target == "cortex-m4":
        t = CortexM4()
    elif target == "cortex-m4f":
        t = CortexM4F()
    elif target == "cortex-m7f":
        t = CortexM7F()
    elif target == "cortex-m7df":
        t = CortexM7DF()
    elif target == "cortex-m23":
        t = CortexM23()
    elif target == "cortex-m33f":
        t = CortexM33F()
    elif target == "cortex-m33df":
        t = CortexM33DF()
    elif target == "arm-deos":
        t = ArmDeos()
    # SPARC/Leon elf
    elif target == "leon2" or target == "leon":
        t = Leon2()
    elif target == "leon3":
        t = Leon3(smp=False)
    elif target == "leon3-smp":
        t = Leon3(smp=True)
    elif target == "leon4":
        t = Leon4(smp=False)
    elif target == "leon4-smp":
        t = Leon4(smp=True)
    # PPC elf
    elif target == "mpc5200":
        t = MPC5200()
    elif target == "mpc8641":
        t = MPC8641()
    elif target == "8349e":
        t = MPC8349e()
    elif target == "p2020":
        t = P2020()
    elif target == "p5566":
        t = P5566()
    elif target == "mpc5634":
        t = P5634()
    elif target == "virtex5":
        t = Virtex5()
    # Visium elf
    elif target == "mcm":
        t = Visium()
    # Risc-V
    elif target == "spike":
        t = Spike()
    elif target == "hifive1":
        t = HiFive1()
    elif target == "miv_rv32imaf":
        t = MIV_RV32IMAF()
    elif target == "polarfiresoc":
        t = PolarFireSOC()
    elif target == "rv32i":
        t = RV32I()
    elif target == "rv32im":
        t = RV32IM()
    elif target == "rv32iac":
        t = RV32IAC()
    elif target == "rv32imac":
        t = RV32IMAC()
    elif target == "rv32imafc":
        t = RV32IMAFC()
    elif target == "rv32imafdc":
        t = RV32IMAFDC()
    elif target == "rv64im":
        t = RV64IM()
    elif target == "rv64imc":
        t = RV64IMC()
    elif target == "rv64imac":
        t = RV64IMAC()
    elif target == "rv64imafc":
        t = RV64IMAFC()
    elif target == "rv64imfc":
        t = RV64IMFC()
    elif target == "rv64imafdc":
        t = RV64IMAFDC()
    # x86_64
    elif target == "x86_64":
        t = X8664Generic()
    # native platforms
    elif target == "aarch64-linux":
        t = Aarch64Linux()
    elif target == "x86-linux":
        t = X86Linux()
    elif target == "x86_64-linux":
        t = X8664Linux()
    elif target == "x86-windows":
        t = X86Windows()
    elif target in ("x86_64-windows", "x86_64-windows64"):
        t = X8664Windows()
    # vx7r2cert
    elif target == "aarch64-vx7r2cert":
        t = AArch64Vx7r2Cert()
    elif target == "morello-vx7r2cert":
        t = MorelloVx7r2Cert()
    elif target == "arm-vx7r2cert":
        t = ArmVx7r2Cert()
    elif target == "ppc-vx7r2cert":
        t = PPCVx7r2Cert()
    elif target == "ppc64-vx7r2cert":
        t = PPC64Vx7r2Cert()
    elif target == "x86-vx7r2cert":
        t = X86Vx7r2Cert()
    elif target == "x86_64-vx7r2cert":
        t = X86_64Vx7r2Cert()
    elif target == "aarch64-vx7r2cert-rtp":
        t = AArch64Vx7r2Cert_RTP()
    elif target == "morello-vx7r2cert-rtp":
        t = MorelloVx7r2Cert_RTP()
    elif target == "arm-vx7r2cert-rtp":
        t = ArmVx7r2Cert_RTP()
    elif target == "ppc-vx7r2cert-rtp":
        t = PPCVx7r2Cert_RTP()
    elif target == "ppc64-vx7r2cert-rtp":
        t = PPC64Vx7r2Cert_RTP()
    elif target == "x86-vx7r2cert-rtp":
        t = X86Vx7r2Cert_RTP()
    elif target == "x86_64-vx7r2cert-rtp":
        t = X86_64Vx7r2Cert_RTP()
    elif target == "aarch64-qnx":
        t = Aarch64QNX()
    elif target == "arm-qnx":
        t = ARMQNX()
    # LynxOS
    elif target == "ppc-lynx178":
        t = PPCLynx()
    elif target == "aarch64-lynx178":
        t = Aarch64Lynx()
    else:
        print("Error: undefined target %s" % target)
        sys.exit(2)

    return t


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose output")
    parser.add_argument(
        "-f",
        "--force",
        action="store_true",
        help=("Forces the installation by overwriting " "any pre-existing runtime."),
    )
    parser.add_argument(
        "--rts-src-descriptor",
        help="The runtime source descriptor file (rts-sources.json)",
    )
    parser.add_argument(
        "--gen-doc", action="store_true", help="Generate the documentation"
    )
    parser.add_argument(
        "--compiler",
        default="gnat",
        help="The compiler to generate flags for (gnat or gnat_llvm, defaults to gnat)",
    )
    parser.add_argument(
        "-o",
        "--output",
        default="install",
        help="Where built runtimes will be installed",
    )
    parser.add_argument(
        "-l",
        "--link",
        action="store_true",
        help="Use symlinks instead of copies when installing",
    )
    parser.add_argument("-b", "--build", action="store_true", help="Build the runtimes")
    parser.add_argument("--build-flags", help="Flags passed to gprbuild")
    parser.add_argument(
        "--shared",
        action="store_true",
        help="Additionally build shared runtime "
        "(only available on platforms that support shared libraries)",
    )
    parser.add_argument(
        "target", nargs="+", help="List of target boards to generate runtimes for"
    )
    parser.add_argument(
        "--profiles",
        type=str,
        help="Comma seperated list of profiles to generate runtimes for",
    )
    args = parser.parse_args()

    if args.verbose:
        FilesHolder.verbose = True
    if args.link:
        FilesHolder.link = True
    if args.force:
        Installer.overwrite = True

    set_target_compiler(Compiler[args.compiler])

    boards = []

    if len(args.target) == 1 and args.target[0].endswith("vx7r2cert"):
        args.target.append(args.target[0] + "-rtp")

    for arg in args.target:
        board = build_configs(arg)
        boards.append(board)

    dest = os.path.abspath(args.output)
    if not os.path.exists(dest):
        os.makedirs(dest)

    # README file generation
    if args.gen_doc:
        # figure out the target
        target = boards[0].target
        for board in boards:
            assert (
                target == board.target
            ), "cannot generate rts doc for different compiler targets"

        doc_dir = os.path.join(dest, "doc")
        docgen(boards, target, doc_dir)
        # and do nothing else
        return

    if not os.path.exists(dest):
        os.makedirs(dest)

    # Install the runtimes sources
    runtimes = []
    for board in boards:
        print("install runtime sources for %s" % board.name)
        sys.stdout.flush()
        installer = Installer(board)
        runtimes += installer.install(
            dest,
            rts_descriptor=args.rts_src_descriptor,
            profiles=args.profiles.split(",") if args.profiles is not None else None,
        )

    # and build them
    if args.build:
        for rt in runtimes:
            # Objects needed before building the runtime
            obj_dir = os.path.join(rt, "obj")
            if not os.path.isdir(obj_dir):
                if os.path.exists(obj_dir):
                    raise RuntimeError("obj should be a directory")
                os.makedirs(obj_dir)
            board.pre_build_step(obj_dir)

            # Import and call runtime-specific build script

            # Add the runtime path to the Python path. To ensure the correct module is
            # loaded the runtime location is inserted as the first element of the path.
            sys.path.insert(0, rt)
            import build as rts_build

            # Reload the imported module. This is important as by default Python caches modules
            # by name. If the module is not reloaded explicitly Python will reuse the same module
            # that has been imported first over and over again, even if it has been removed with
            # del.
            importlib.reload(rts_build)

            # Call the build script
            rts_build.main(args.shared, args.build_flags)

            # Delete the module
            del rts_build

            # Remove the runtime from the Python path
            sys.path.remove(rt)

    print("runtimes successfully installed in %s" % dest)


if __name__ == "__main__":
    main()
