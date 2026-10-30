#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import Literal, override

from bb_runtimes_targets_gen.components.atomics import AtomicSupport

from bb_runtimes_targets_gen.concrete_infrastructure import (
    Target,
    light_profile,
    light_tasking_profile,
)
from rts_prebuilder.abstract_infrastructure import Loader
from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.abstract_infrastructure import Profile
from rts_prebuilder.base_types import PlatformIdType
from rts_prebuilder.base_types import ValidBaseProfileType

Vx7r2CertVariant = Literal["kernel", "rtp"]
"""Whether the target builds for the VxWorks kernel or for an RTP."""


class Vx7r2Cert(Target):
    """VxWorks 7 r2 Certified target base class."""

    _platform_variant: Vx7r2CertVariant

    RTP_SYS_LIBS: tuple[str, ...] = ("-l:certRtp.o",)
    """System libraries the FULL_LINK loader links (the certified-VSB library by
    default)."""

    RTP_EXTRA_LIBS: tuple[str, ...] = ()
    """Extra libraries FULL_LINK appends after the ldscript (e.g. PowerPC's
    ``-lgnu``)."""

    @property
    def _rtp_suffix(self) -> str:
        """The ``-rtp`` the RTP target names and runtime names carry, empty on a
        kernel target."""
        return "-rtp" if self._platform_variant == "rtp" else ""

    @property
    @override
    def platform_variant(self) -> Vx7r2CertVariant:
        return self._platform_variant

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "vx7r2cert"

    @property
    @override
    def has_command_line_arguments(self) -> bool:
        return True

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def has_huge_memory(self) -> bool:
        return True

    @property
    @override
    def is_os_target(self) -> bool:
        return True

    @override
    def has_libc(self, base_profile: ValidBaseProfileType) -> bool:
        return True

    @override
    def runtime_name_generator(self, profile: Profile) -> str:
        return "rts-" + profile.base_profile + self._rtp_suffix

    def __init__(self, variant: Vx7r2CertVariant) -> None:
        self._platform_variant = variant
        super().__init__()

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if self._platform_variant == "rtp":
            self.add_app_switches(common_switches=("-mrtp",))

        self.add_gnat_sources("shared/s-macres__vx7r2cert.adb")

        if self._platform_variant == "rtp":
            self.add_runtime_switches(
                common_switches=("-mrtp", "-DCERT_RTP", "-D_WRS_VXCERT_RTP"),
            )
            # VSB location comes from the environment at gpr-parse time. The
            # FULL_LINK switches below reference it.
            self.add_gpr_external("VSB_Dir", "VSB_DIR")

            # An RTP program is linked either fully, from the switches here, or
            # partially by the GNATbench makefiles, hence the bare
            # PARTIAL_LINK. The whole FULL_LINK line is written here so its
            # order is fixed and visible.
            self.add_loaders(
                Loader(
                    "FULL_LINK",
                    link_switches=(
                        "-nodefaultlibs",
                        "-nostartfiles",
                        *self.RTP_SYS_LIBS,
                        '-L" & VSB_Dir & "/usr/lib/common/objcert',
                        '-T" & VSB_Dir & "/usr/ldscripts/rtp.ld',
                        *self.RTP_EXTRA_LIBS,
                    ),
                ),
                Loader("PARTIAL_LINK"),
            )
        else:
            self.add_app_switches(link_switches=("-nostdlib",))


class Vx7r2Cert64(Vx7r2Cert):
    def __init__(self, variant: Vx7r2CertVariant) -> None:
        super().__init__(variant)

    @property
    @override
    def is_64bit(self) -> bool:
        return True


class AArch64Vx7r2Cert(Vx7r2Cert64):
    @property
    @override
    def target(self) -> str:
        return "aarch64-vx7r2"

    @property
    @override
    def name(self) -> str:
        return f"aarch64-vx7r2cert{self._rtp_suffix}"

    @property
    @override
    def profiles(self) -> list[Profile]:
        if self._platform_variant == "rtp":
            return [
                light_tasking_profile(
                    self,
                    Path("system-vxworks7-arm-ravenscar-sfp-rtp.ads"),
                )
            ]
        return [
            light_profile(self, Path("system-vxworks7-arm-zfp.ads")),
            light_tasking_profile(
                self,
                Path("system-vxworks7-arm-ravenscar-sfp.ads"),
            ),
        ]

    def __init__(self, variant: Vx7r2CertVariant = "kernel") -> None:
        super().__init__(variant)

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_runtime_switches(
            common_switches=(
                "-mno-outline-atomics",
                # The traceback implementation in our restricted runtimes for
                # this platform relies on all frames having a frame pointer, so
                # make sure it is always there. See T709-039 for more info.
                "-fno-omit-frame-pointer",
            ),
        )


class MorelloVx7r2Cert(AArch64Vx7r2Cert):
    @property
    @override
    def target(self) -> str:
        return "morello-vx7r2"

    @property
    @override
    def name(self) -> str:
        return f"morello-vx7r2cert{self._rtp_suffix}"

    @property
    @override
    def has_cheri(self) -> bool:
        return True

    @property
    @override
    def profiles(self) -> list[Profile]:
        if self._platform_variant == "rtp":
            return [
                light_profile(self, Path("system-vxworks7-arm-light-rtp.ads")),
                light_tasking_profile(
                    self,
                    Path("system-vxworks7-arm-ravenscar-sfp-rtp.ads"),
                ),
            ]
        return [
            light_profile(self, Path("system-vxworks7-arm-zfp.ads")),
            light_tasking_profile(
                self,
                Path("system-vxworks7-arm-ravenscar-sfp.ads"),
            ),
        ]

    def __init__(self, variant: Vx7r2CertVariant = "kernel") -> None:
        super().__init__(variant)

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        # RTP: purecap Morello, replacing the base's -mrtp. Kernel stays a plain
        # aarch64 build until the kernel supports purecap (avoids a morello
        # exception in built_rts.py to exclude the kernel runtimes).
        if self._platform_variant == "rtp":
            self.add_app_switches(
                common_switches=("-march=morello+c64", "-mabi=purecap"),
                replace_channel=True,
            )


class ArmVx7r2Cert(Vx7r2Cert):
    @property
    @override
    def target(self) -> str:
        return "arm-vx7r2"

    @property
    @override
    def name(self) -> str:
        return f"arm-vx7r2cert{self._rtp_suffix}"

    @property
    @override
    def profiles(self) -> list[Profile]:
        if self._platform_variant == "rtp":
            return [
                light_tasking_profile(
                    self,
                    Path("system-vxworks7-arm-ravenscar-sfp-rtp.ads"),
                )
            ]
        return [
            light_profile(self, Path("system-vxworks7-arm-zfp.ads")),
            light_tasking_profile(
                self,
                Path("system-vxworks7-arm-ravenscar-sfp.ads"),
            ),
        ]

    def __init__(self, variant: Vx7r2CertVariant = "kernel") -> None:
        super().__init__(variant)

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        # The traceback implementation in our restricted runtimes for this
        # platform relies on all frames having a frame pointer, so make sure
        # it is always there. See V217-008 for more info.
        self.add_runtime_switches(
            common_switches=("-fno-omit-frame-pointer",),
        )


class PPCVx7r2Cert(Vx7r2Cert):
    @property
    @override
    def target(self) -> str:
        return "ppc-vx7r2"

    @property
    @override
    def name(self) -> str:
        return f"ppc-vx7r2cert{self._rtp_suffix}"

    @property
    @override
    def profiles(self) -> list[Profile]:
        if self._platform_variant == "rtp":
            return [
                light_tasking_profile(
                    self,
                    Path("system-vxworks7-ppc-ravenscar-sfp-rtp.ads"),
                )
            ]
        return [
            light_profile(self, Path("system-vxworks7-ppc-zfp.ads")),
            light_tasking_profile(
                self,
                Path("system-vxworks7-ppc-ravenscar-sfp.ads"),
            ),
        ]

    def __init__(self, variant: Vx7r2CertVariant = "kernel") -> None:
        super().__init__(variant)


class PPC64Vx7r2Cert(Vx7r2Cert64):
    @property
    @override
    def target(self) -> str:
        return "ppc64-vx7r2"

    @property
    @override
    def name(self) -> str:
        return f"ppc64-vx7r2cert{self._rtp_suffix}"

    @property
    @override
    def profiles(self) -> list[Profile]:
        if self._platform_variant == "rtp":
            return [
                light_tasking_profile(
                    self,
                    Path("system-vxworks7-ppc64-ravenscar-sfp-rtp.ads"),
                )
            ]
        return [
            light_profile(self, Path("system-vxworks7-ppc-zfp.ads")),
            light_tasking_profile(
                self,
                Path("system-vxworks7-ppc64-ravenscar-sfp.ads"),
            ),
        ]

    def __init__(self, variant: Vx7r2CertVariant = "kernel") -> None:
        super().__init__(variant)


class X86Vx7r2Cert(Vx7r2Cert):
    @property
    @override
    def atomic_support(self) -> AtomicSupport:
        return AtomicSupport.UP_TO_64_BITS

    @property
    @override
    def target(self) -> str:
        return "x86-vx7r2"

    @property
    @override
    def name(self) -> str:
        return f"x86-vx7r2cert{self._rtp_suffix}"

    @property
    @override
    def profiles(self) -> list[Profile]:
        if self._platform_variant == "rtp":
            return [
                light_tasking_profile(
                    self,
                    Path("system-vxworks7-x86-ravenscar-sfp-rtp.ads"),
                )
            ]
        return [
            light_profile(self, Path("system-vxworks7-x86-zfp.ads")),
            light_tasking_profile(
                self,
                Path("system-vxworks7-x86-ravenscar-sfp.ads"),
            ),
        ]

    def __init__(self, variant: Vx7r2CertVariant = "kernel") -> None:
        super().__init__(variant)


class X86_64Vx7r2Cert(Vx7r2Cert64):
    @property
    @override
    def target(self) -> str:
        return "x86_64-vx7r2"

    @property
    @override
    def name(self) -> str:
        return f"x86_64-vx7r2cert{self._rtp_suffix}"

    @property
    @override
    def profiles(self) -> list[Profile]:
        if self._platform_variant == "rtp":
            return [
                light_tasking_profile(
                    self,
                    Path("system-vxworks7-x86-ravenscar-sfp-rtp.ads"),
                )
            ]
        return [
            light_profile(self, Path("system-vxworks7-x86-zfp.ads")),
            light_tasking_profile(
                self,
                Path("system-vxworks7-x86-ravenscar-sfp.ads"),
            ),
        ]

    def __init__(self, variant: Vx7r2CertVariant = "kernel") -> None:
        super().__init__(variant)

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        # The traceback implementation in our restricted runtimes for this
        # platform relies on all frames having a frame pointer, so make sure
        # it is always there. See V217-008 for more info.
        self.add_runtime_switches(
            common_switches=("-fno-omit-frame-pointer",),
        )


# RTP note: the -Wl,--defsym=__wrs_rtp_base= linker switch is defined in the
# gprconfig_kb database.


class AArch64Vx7r2Cert_RTP(AArch64Vx7r2Cert):
    def __init__(self) -> None:
        super().__init__(variant="rtp")


class MorelloVx7r2Cert_RTP(MorelloVx7r2Cert):
    # Morello can't build a GOS_SAFETY VSB, so it links the regular RTP
    # libraries instead of the certified one. gprconfig_kb forces -nostdlib,
    # hence the explicit system libraries.
    RTP_SYS_LIBS = ("-l:crt0.o", "-lc", "-lc_internal_s", "-lgcc")

    def __init__(self) -> None:
        super().__init__(variant="rtp")

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            link_switches=("-Wl,--defsym=__wrs_rtp_base=0x80000000",),
        )


class ArmVx7r2Cert_RTP(ArmVx7r2Cert):
    def __init__(self) -> None:
        super().__init__(variant="rtp")


class PPCVx7r2Cert_RTP(PPCVx7r2Cert):
    RTP_EXTRA_LIBS = ("-lgnu",)  # PowerPC additionally needs -lgnu

    def __init__(self) -> None:
        super().__init__(variant="rtp")


class PPC64Vx7r2Cert_RTP(PPC64Vx7r2Cert):
    RTP_EXTRA_LIBS = ("-lgnu",)  # PowerPC additionally needs -lgnu

    def __init__(self) -> None:
        super().__init__(variant="rtp")


class X86Vx7r2Cert_RTP(X86Vx7r2Cert):
    def __init__(self) -> None:
        super().__init__(variant="rtp")


class X86_64Vx7r2Cert_RTP(X86_64Vx7r2Cert):
    def __init__(self) -> None:
        super().__init__(variant="rtp")


TARGETS = [
    AArch64Vx7r2Cert(variant="kernel"),
    AArch64Vx7r2Cert_RTP(),
    MorelloVx7r2Cert(variant="kernel"),
    MorelloVx7r2Cert_RTP(),
    ArmVx7r2Cert(variant="kernel"),
    ArmVx7r2Cert_RTP(),
    PPCVx7r2Cert(variant="kernel"),
    PPCVx7r2Cert_RTP(),
    PPC64Vx7r2Cert(variant="kernel"),
    PPC64Vx7r2Cert_RTP(),
    X86Vx7r2Cert(variant="kernel"),
    X86Vx7r2Cert_RTP(),
    X86_64Vx7r2Cert(variant="kernel"),
    X86_64Vx7r2Cert_RTP(),
]
