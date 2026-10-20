#
# Copyright (C) 2026, AdaCore
#

"""Derives the component set that makes up a runtime, per base profile.

One factory per base profile. Each is a plain function, not a component. Given
a target, it returns the flat set of components for that runtime. That is the
choice the legacy ``component_selection`` module used to make.

The matching profile factory calls it to fill ``Profile.components``. The
specializer then folds those into the target's always-on tree. Custom runtimes
may hand-pick their own set instead.
"""

from __future__ import annotations

from typing import Literal, TYPE_CHECKING

from rts_prebuilder.abstract_infrastructure import SourcesAndFlagsComponent
from rts_prebuilder.base_types import ValidBaseProfileType
from rts_prebuilder.end_user_data.compiler_selector import using_llvm_compiler

from ..components import (
    AdaLibc,
    AllocC,
    AllocNoCas,
    AllocNoTasking,
    AllocNoTaskingCheri,
    AllocTasking,
    AllocTaskingCheri,
    AllocTaskingNoc,
    Arith64,
    Arith128,
    Atomics,
    Atomics32,
    Atomics64,
    Cert,
    CertArm,
    CommandLine,
    Common,
    Common32,
    Common64,
    CommonCheri,
    Containers,
    CtorsDtors,
    CtorsDtorsC,
    CtorsDtorsDefault,
    ExponentFloat,
    ExponentInt,
    ExponentIntLL,
    ExponentIntLLL,
    ExponentMod,
    Fpu,
    Full,
    Full32,
    Full64,
    FullGcc,
    FullZcxAarch64,
    FullZcxArm,
    FullZcxDw2,
    FullZcxDw2Gcc,
    FullZcxLeon,
    FullZcxPpc,
    FullZcxRiscv,
    FullZcxX86,
    FullZcxX8664,
    GccMath,
    GnarlBareboard,
    GnarlCommon,
    GnarlCommonCheri,
    GnarlCommonNoCheri,
    GnarlAarch64,
    GnarlArm,
    GnarlFull,
    GnarlFullExtended,
    GnarlLinux,
    GnarlLynx,
    GnarlPikeos,
    GnarlPpc,
    GnarlQnx,
    GnarlSfp,
    GnarlSpinlockGcc,
    GnarlSpinlockLeon,
    GnarlTimer32,
    GnarlTimer64,
    GnarlVx7r2Cert,
    GnarlX86,
    GnarlX8664,
    ImageChar,
    ImageDecimal,
    ImageDecimalLL,
    ImageDecimalLLL,
    ImageEnum,
    ImageFixed,
    ImageFixedLL,
    ImageFixedLLL,
    ImageFloat,
    ImageInt,
    ImageIntLL,
    ImageIntLLL,
    ImageUtil,
    ImageWideChar,
    Ioexce,
    Libgcc,
    LibgccArm,
    LibgccOtherArchs,
    Light,
    LightAarch64,
    LightArm,
    LightExceptions,
    LightNonTasking,
    LightPpc,
    LightPpc64,
    LightTasking,
    LightX86,
    LightX8664,
    Math,
    MathComplex,
    MathFull,
    MathHardDP,
    MathHardSP,
    MathLongLong64bit,
    MathLongLong80bit,
    MathSoftDP,
    MathSoftSP,
    Mem,
    MemCheri,
    MemNoCheri,
    Newlib,
    Pack,
    Pack64,
    ParametersHuge,
    ParametersLarge,
    ParametersLightHuge,
    ParametersLightLarge,
    ParametersLightSmall,
    ParametersSmall,
    SecondaryStackSoftlinks,
    SecondaryStackSymbol,
    SecondaryStackSymbolCheri,
    Semihosting,
    Streams,
    StreamsCheri,
    StreamsNoCheri,
    SystemIO,
    UtilsCaseUtil,
    UtilsCaseUtilNss,
    UtilsFloat,
    UtilsFloatFma,
    ValueBoolean,
    ValueChar,
    ValueDecimal,
    ValueDecimalLL,
    ValueDecimalLLL,
    ValueEnum,
    ValueFixed,
    ValueFixedLL,
    ValueFixedLLL,
    ValueFloat,
    ValueInt,
    ValueIntLL,
    ValueIntLLL,
    ValueUtils,
    ValueWideChar,
)

type CpuFamilyType = Literal[
    "arm",
    "aarch64",
    "leon",
    "powerpc",
    "powerpc64",
    "x86",
    "x86_64",
    "riscv32",
    "riscv64",
]


if TYPE_CHECKING:
    from .target import Target


def _cpu_family(target: Target) -> CpuFamilyType:
    """The CPU family of the target, deduced from its gprbuild triplet."""
    cpu = target.target.split("-")[0]

    if cpu in ("aarch64", "morello"):
        return "aarch64"
    elif cpu in ("arm",):
        return "arm"
    elif cpu.startswith("leon"):
        return "leon"
    elif cpu in ("powerpc", "ppc"):
        return "powerpc"
    elif cpu in ("powerpc64", "ppc64"):
        return "powerpc64"
    elif cpu in ("x86",):
        return "x86"
    elif cpu in ("x86_64",):
        return "x86_64"
    elif cpu in ("riscv32", "r7"):
        return "riscv32"
    elif cpu in ("riscv64",):
        return "riscv64"

    raise ValueError(
        f"Unknown CPU '{cpu}' from {target} target triplet '{target.target}'"
    )


def _has_fma(target: Target) -> bool:
    """Whether the target has a fused multiply-add instruction."""
    match _cpu_family(target):
        case "aarch64" | "powerpc" | "powerpc64":
            return target.has_fpu
        case "riscv32" | "riscv64":
            return target.has_double_precision_fpu
        case _:
            return False


def no_profile_components(target: Target) -> list[SourcesAndFlagsComponent]:
    """Minimal component set for the "none" base profile, meant as a base
    for custom profiles that hand-pick the rest.
    """
    ret: list[SourcesAndFlagsComponent] = [
        Common(),
        Common64() if target.is_64bit else Common32(),
        Fpu(),
        Atomics(),
    ]
    if target.has_cheri:
        ret.append(CommonCheri())

    # Using certifiable packages means libgcc is not linked with. This may
    # be useful for custom profiles which do not want to link with gcc.
    if target.use_certifiable_packages:
        ret.append(Libgcc())
        ret.append(LibgccArm() if _cpu_family(target) == "arm" else LibgccOtherArchs())

    return ret


def _light_subset_components(
    target: Target, base_profile: ValidBaseProfileType = "light"
) -> list[SourcesAndFlagsComponent]:
    """The components used at least by the "light" runtime. Reused by the more
    complete profiles.

    :param base_profile: If the subset is used for another profile, it is
                         mandatory to set this arg.
    """
    cpu = _cpu_family(target)
    is64 = target.is_64bit

    ret: list[SourcesAndFlagsComponent] = []

    ret.append(Common())
    ret.append(Common64() if is64 else Common32())
    if target.has_cheri:
        ret.append(CommonCheri())
    ret.append(Fpu())

    ret.append(Atomics())
    if cpu in ("aarch64", "powerpc64", "x86", "x86_64", "riscv64"):
        ret.append(Atomics64())
    else:
        ret.append(Atomics32())

    ret.append(Arith64())
    if is64:
        ret.append(Arith128())

    ret += [ExponentFloat(), ExponentInt(), ExponentIntLL(), ExponentMod()]
    if is64:
        ret.append(ExponentIntLLL())

    ret += [
        ImageEnum(),
        ImageInt(),
        ImageIntLL(),
        ImageDecimal(),
        ImageDecimalLL(),
        ImageFixed(),
        ImageFixedLL(),
        ImageFloat(),
        ImageChar(),
        ImageUtil(),
    ]
    if is64:
        ret += [ImageIntLLL(), ImageDecimalLLL(), ImageFixedLLL()]

    ret += [
        ValueBoolean(),
        ValueEnum(),
        ValueDecimal(),
        ValueDecimalLL(),
        ValueFixed(),
        ValueFixedLL(),
        ValueFloat(),
        ValueInt(),
        ValueIntLL(),
        ValueChar(),
        ValueUtils(),
    ]
    if is64:
        ret += [ValueIntLLL(), ValueDecimalLLL(), ValueFixedLLL()]

    ret += [UtilsCaseUtil(), UtilsCaseUtilNss()]
    ret.append(UtilsFloatFma() if _has_fma(target) else UtilsFloat())

    ret.append(Math())
    if cpu in ("x86", "x86_64"):
        ret.append(MathLongLong80bit())
    else:
        ret.append(MathLongLong64bit())
    ret.append(MathHardSP() if target.has_single_precision_fpu else MathSoftSP())
    ret.append(MathHardDP() if target.has_double_precision_fpu else MathSoftDP())

    if target.has_command_line_arguments:
        ret.append(CommandLine())

    ret.append(Semihosting() if target.use_semihosting_io else SystemIO())

    if not target.has_libc(base_profile):
        ret.append(Mem())
        ret.append(MemCheri() if target.has_cheri else MemNoCheri())

    if base_profile == "cert":
        ret.append(SecondaryStackSoftlinks())
    else:
        ret.append(
            SecondaryStackSymbolCheri() if target.has_cheri else SecondaryStackSymbol()
        )

    if target.use_certifiable_packages and base_profile != "embedded":
        ret.append(Libgcc())
        ret.append(LibgccArm() if cpu == "arm" else LibgccOtherArchs())

    # C++ constructors/destructors package:
    # not for OSs (it has libc) or morello or LLVM (they do not support C++)
    if base_profile != "embedded" and not (
        target.is_os_target or target.has_cheri or using_llvm_compiler()
    ):
        ret.append(CtorsDtors())
        ret.append(CtorsDtorsC() if cpu in ("powerpc", "leon") else CtorsDtorsDefault())

    if base_profile != "embedded" and base_profile != "cert":
        ret.append(LightExceptions())

    if base_profile in ("light", "light-tasking", "cert"):
        ret.append(Light())
    if base_profile in ("light", "cert"):
        ret.append(LightNonTasking())

    if base_profile in ("light", "light-tasking"):
        match cpu:
            case "arm":
                ret.append(LightArm())
            case "aarch64":
                ret.append(LightAarch64())
            case "powerpc":
                ret.append(LightPpc())
            case "powerpc64":
                ret.append(LightPpc64())
            case "x86":
                ret.append(LightX86())
            case "x86_64":
                ret.append(LightX8664())
            case _:
                pass

        # Of the families listed here only aarch64 is ever 64-bit, so aarch64
        # is what this condition selects today. The wider list is kept so a
        # future 64-bit arm or x86 target picks GccMath up on its own.
        if is64 and cpu in ("arm", "aarch64", "x86"):
            ret.append(GccMath())

    if base_profile != "cert":
        ret.append(Pack())
        if is64:
            ret.append(Pack64())

    # System.Parameters, sized by the memory profile
    if target.has_small_memory:
        ret.append(
            ParametersLightSmall() if base_profile == "light" else ParametersSmall()
        )
    elif target.has_huge_memory:
        ret.append(
            ParametersLightHuge() if base_profile == "light" else ParametersHuge()
        )
    else:
        ret.append(
            ParametersLightLarge() if base_profile == "light" else ParametersLarge()
        )

    return ret


def _gnarl_subset_components(target: Target) -> list[SourcesAndFlagsComponent]:
    """The GNARL components the tasking profiles (light-tasking, embedded)
    share.
    """
    cpu = _cpu_family(target)

    ret: list[SourcesAndFlagsComponent] = [GnarlCommon()]
    ret.append(GnarlCommonCheri() if target.has_cheri else GnarlCommonNoCheri())

    ret.append(GnarlTimer64() if target.has_timer_64 else GnarlTimer32())

    ret.append(GnarlSpinlockLeon() if cpu == "leon" else GnarlSpinlockGcc())

    match cpu:
        case "aarch64":
            ret.append(GnarlAarch64())
        case "arm":
            ret.append(GnarlArm())
        case "powerpc" | "powerpc64":
            ret.append(GnarlPpc())
        case "x86":
            ret.append(GnarlX86())
        case "x86_64":
            ret.append(GnarlX8664())
        case _:
            pass

    match target.platform:
        case "bb":
            ret.append(GnarlBareboard())
        case "linux":
            ret.append(GnarlLinux())
        case "lynx":
            ret.append(GnarlLynx())
        case "qnx":
            ret.append(GnarlQnx())
        case "pikeos":
            ret.append(GnarlPikeos())
        case "vx7r2cert":
            ret.append(GnarlVx7r2Cert())
        case _:
            pass

    return ret


def light_components(target: Target) -> list[SourcesAndFlagsComponent]:
    """The components for the light profile."""
    ret = _light_subset_components(target)

    if not target.has_libc("light"):
        ret.append(AllocNoTaskingCheri() if target.has_cheri else AllocNoTasking())
    else:
        ret.append(AllocC())

    return ret


def light_tasking_components(target: Target) -> list[SourcesAndFlagsComponent]:
    """The components for the light-tasking profile."""
    ret = _light_subset_components(target, "light-tasking")

    ret.append(LightTasking())

    if not target.has_libc("light-tasking"):
        if not target.has_compare_and_swap:
            ret.append(AllocNoCas())
        elif target.use_certifiable_packages:
            # The certifiable tasking allocator has no CHERI variant.
            if not target.has_cheri:
                ret.append(AllocTaskingNoc())
        else:
            ret.append(AllocTaskingCheri() if target.has_cheri else AllocTasking())
    else:
        ret.append(AllocC())

    ret += _gnarl_subset_components(target)
    ret.append(GnarlSfp())

    return ret


def embedded_components(target: Target) -> list[SourcesAndFlagsComponent]:
    """The components for the embedded profile."""
    cpu = _cpu_family(target)

    # We don't support certifiable components with Embedded since our libgcc
    # replacement does not provide exception support. Ctors/dtors support comes
    # from the libc. Both are excluded by _light_subset_components here.
    ret = _light_subset_components(target, "embedded")

    ret.append(Full())
    ret.append(Full64() if target.is_64bit else Full32())
    if not using_llvm_compiler():
        ret.append(FullGcc())
    ret.append(Containers())

    # Zero-cost exception propagation support
    if cpu == "arm":
        ret.append(FullZcxArm())
    else:
        ret.append(FullZcxDw2())
        if not using_llvm_compiler():
            ret.append(FullZcxDw2Gcc())
        match cpu:
            case "aarch64":
                ret.append(FullZcxAarch64())
            case "powerpc":
                ret.append(FullZcxPpc())
            case "riscv32" | "riscv64":
                ret.append(FullZcxRiscv())
            case "leon":
                ret.append(FullZcxLeon())
            case "x86":
                ret.append(FullZcxX86())
            case "x86_64":
                ret.append(FullZcxX8664())
            case _:
                pass

    ret += [MathComplex(), MathFull()]
    ret += [ImageWideChar(), ValueWideChar()]

    ret.append(Ioexce())
    ret.append(Streams())
    ret.append(StreamsCheri() if target.has_cheri else StreamsNoCheri())

    if target.has_libc("embedded"):
        match target.libc_implementation("embedded"):
            case "newlib":
                ret.append(Newlib())
            case "adalib":
                ret.append(AdaLibc())

    ret += _gnarl_subset_components(target)
    ret.append(GnarlFull())
    ret.append(GnarlFullExtended())

    return ret


def cert_components(target: Target) -> list[SourcesAndFlagsComponent]:
    """The components for the cert profile."""
    ret = _light_subset_components(target, "cert")

    ret.append(Ioexce())

    ret.append(Cert())
    if _cpu_family(target) == "arm":
        ret.append(CertArm())

    if target.has_libc("cert"):
        ret.append(AllocC())

    return ret
