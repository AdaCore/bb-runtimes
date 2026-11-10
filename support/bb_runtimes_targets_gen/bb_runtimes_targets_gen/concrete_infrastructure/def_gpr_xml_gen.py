#
# Copyright (C) 2025-2026, AdaCore
#

from dataclasses import dataclass, field
from pathlib import Path

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    GprSwitches,
    Loader,
    Profile,
    dest_name,
)
from rts_prebuilder.end_user_data.compiler_selector import (
    using_gcc_compiler,
    using_llvm_compiler,
)
from rts_prebuilder.engine import resolve_and_read_file
from rts_prebuilder.engine.profile_specializer import runtime_loaders


@dataclass
class RuntimeXmlSource:
    """A target's runtime.xml sourced from a hand-maintained data file, with
    optional ``old -> new`` text substitutions (e.g. filling ``@version@``).

    A target declares one as data on ``runtime_xml_source``. It carries no
    generation logic. A target that declares none has its runtime.xml generated
    from its attributes and component tree instead."""

    path: Path
    substitutions: dict[str, str] = field(default_factory=dict)

    def render(self) -> str:
        """Return the runtime.xml content."""
        content = resolve_and_read_file(self.path)
        for old, new in self.substitutions.items():
            content = content.replace(old, new)
        return content


def generate_runtime_xml(
    target: AbstractTarget,
    profile: Profile,
) -> str:
    """Produce a target's runtime.xml: render its declared
    ``runtime_xml_source`` if it has one, else generate it from the target's
    attributes and component tree. The engine reaches this one entry point
    through the infrastructure interface. Targets never generate.

    The engine passes an ``AbstractTarget``. Only this infrastructure's
    ``Target`` declares ``runtime_xml_source``, so a target from elsewhere is
    always generated.
    """
    # Local: target imports this module for RuntimeXmlSource.
    from .target import Target

    source = target.runtime_xml_source if isinstance(target, Target) else None
    if source is not None:
        _check_nothing_registered_for_the_app(target, profile, source)
        return source.render()
    return _generate_from_components(target, profile)


def _check_nothing_registered_for_the_app(
    target: AbstractTarget,
    profile: Profile,
    source: RuntimeXmlSource,
) -> None:
    """Refuse app-side registrations on a target whose runtime.xml is
    hand-maintained: that file is rendered as it stands, so a switch registered
    for the app has nowhere to land and would be lost without a word.

    :param target: the target the runtime is being built for.
    :param profile: the profile the runtime is being built for.
    :param source: the hand-maintained runtime.xml the target declared.
    :raises ValueError: if any component registered app-compile switches
        (``add_app_switches`` or ``add_app_and_runtime_switches``).
    """
    switches = GprSwitches()
    for root in [target, *profile.components]:
        switches = switches.merge(root.get_app_switches(target, profile))
    registered = [*switches.common, *switches.c, *switches.asm, *switches.link]
    if registered:
        raise ValueError(
            f"Target {target.name}, profile '{profile.name}': "
            f"{', '.join(registered)} are registered for the app but the "
            f"runtime.xml they belong in is hand-maintained ({source.path}); "
            f"put them in that file, or drop runtime_xml_source so runtime.xml "
            f"is generated"
        )


def _generate_from_components(
    target: AbstractTarget,
    profile: Profile,
) -> str:
    """Generates the runtime.xml file that gives the configuration to gprbuild

    :param target: Target for which the xml file is generated
    :param profile: Profile for which the xml file is generated
    :return: the content of the runtime.xml file as a string
    """
    base_profile = profile.base_profile
    ret = '<?xml version="1.0" ?>\n\n'
    ret += "<gprconfig>\n"
    ret += "  <configuration>\n"
    ret += "    <config><![CDATA[\n"

    # Same roots as profile_specializer.specialize(): the target's always-on
    # tree plus the profile's own components, so a profile-only component's
    # switches/externals/scripts/loaders reach runtime.xml too.
    roots = [target, *profile.components]

    target_loaders = _offered_loaders(target, runtime_loaders(target, profile))

    if len(target_loaders) > 0:
        names = [loader.name for loader in target_loaders]
        ret += f'   type Loaders is ("{'", "'.join(names)}");\n'
        ret += (
            f'   Loader : Loaders := external("LOADER", '
            f'"{_default_loader(target, names)}");\n\n'
        )

    # Add Compiler package
    ret += "   package Compiler is\n"

    app_switches = GprSwitches()
    for root in roots:
        app_switches = app_switches.merge(root.get_app_switches(target, profile))
    common_switches = app_switches.common + get_global_compiler_switches()
    c_switches = app_switches.c
    asm_switches = app_switches.asm

    if len(common_switches) > 0:
        ret += (
            f'      Common_Required_Switches := ("{'", "'.join(common_switches)}");\n'
        )
    else:
        ret += "      Common_Required_Switches := ();\n"

    if len(c_switches) > 0:
        ret += f'      C_Required_Switches := ("{'", "'.join(c_switches)}");\n'

    if len(asm_switches) > 0:
        ret += f'      Asm_Required_Switches := ("{'", "'.join(asm_switches)}");\n'

    ret += "\n"

    # The C family is C, C++ and Asm, since the C driver preprocesses Asm. The
    # asm switches then narrow it to Asm alone. Same split as the runtime's own
    # build (target_options.gpr All_C_Switches / All_Asm_Switches).
    for lang in ("Ada", "C", "C++", "Asm", "Asm2", "Asm_Cpp"):
        w = "      "
        ret += w + f'for Leading_Required_Switches ("{lang}") use\n'
        w = "         "
        ret += w + f'Compiler\'Leading_Required_Switches ("{lang}") &\n'
        ret += w + "Common_Required_Switches"
        if lang != "Ada" and len(c_switches) > 0:
            ret += " &\n" + w
            ret += "C_Required_Switches"
        if lang.startswith("Asm") and len(asm_switches) > 0:
            ret += " &\n" + w
            ret += "Asm_Required_Switches"
        ret += ";\n"
    ret += "   end Compiler;\n\n"

    # Add Linker package
    switches = [f'"{sw}"' for sw in app_switches.link]

    # gpr external variables the target declares, so linker switches can
    # reference their value (e.g. '"-L" & VSB_Dir & "/..."') instead of the
    # runtime patching generated text.
    gpr_externals = [
        pair for root in roots for pair in root.get_gpr_externals(target, profile)
    ]
    for gpr_name, env_name in gpr_externals:
        ret += f'   {gpr_name} := external ("{env_name}");\n'

    ret += "   package Linker is\n"
    indent = 6
    blank = indent * " "
    ret += blank + "for Required_Switches use Linker'Required_Switches &\n"
    ret += blank + '  ("-Wl,-L${RUNTIME_DIR(Ada)}/adalib",\n'
    indent = 9
    blank = indent * " "

    # Runtime specific linker switches
    if target.is_os_target:
        # For OS targets, runtime specific switches are defined in the
        # target packages.
        pass
    elif base_profile != "embedded":
        # For the Light and Light Tasking runtimes we have the choice of
        # either using libgcc or our Ada libgcc replacement. For the
        # later choice we do not link with any of the standard libraries.
        if target.use_certifiable_packages:
            ret += blank + '"-nostdlib",'
        else:
            ret += blank + '"-nolibc",'

        if using_llvm_compiler():
            ret += '"-nostartfiles",'

        # Add spec file for bare-metal targets to support C++
        # constructors/destructors and exception handling tables for
        # exception propagation. If the other conditions for C++ are met.
        if target.has_cplusplus_support and using_gcc_compiler():
            ret += (
                "\n" + blank + '"--specs=${RUNTIME_DIR(Ada)}/link-noexceptions.spec",'
            )

    else:
        # In the Embedded case, the runtime depends on functionalities
        # from newlib, such as memory allocation. This runtime also does
        # not support the certifiable packages option. Also, there's
        # interdependencies between libgnarl and libgnat, so we need to
        # force -lgnarl at link time, always.
        #
        # With gcc, we provide the link arguments for libc ourselves.
        # Inhibit the gcc mechanism doing so with -nolibc first. Then we
        # need to account for intricacies in dependencies, e.g. libc
        # depends on libgcc as everyone, libgcc on libc for strlen,
        # libgnat on libc for __errno or other, libc on libgnat for sbrk,
        # libgnat and libgnarl on each other...
        #
        # The LLVM linker doesn't depend on the order of archive files on
        # the command line because it remembers defined symbols in
        # addition to undefined symbols when scanning archives (see
        # https://lld.llvm.org/NewLLD.html, "Efficient archive file
        # handling"). We stills want "-nolibc" because it disables
        # automatic linking of libm, and "-nostartfiles" because it gets
        # rid of crt0.o.

        if using_gcc_compiler():
            # Exception propagation via DWARF .eh_frame keeps the FDE lookup
            # symbol so the unwinder tables survive the link. Under gcc the
            # link-zcx spec (emitted below for non-CHERI) already injects
            # these. Add them explicitly only when no spec will, so for CHERI.
            eh = (
                '"-u", "_Unwind_Find_FDE", "-Wl,--eh-frame-hdr", '
                if target.uses_eh_frame_unwinding and target.has_cheri
                else ""
            )
            ret += (
                blank
                + eh
                + '"-nolibc", '
                + '"-Wl,--start-group,'
                + "-lgnarl,-lgnat,-lc,-lgcc,-lgcc_eh,"
                + '--end-group",'
            )
        elif target.uses_eh_frame_unwinding:
            # Exception propagation via DWARF .eh_frame: keep the FDE lookup
            # symbol so the unwinder tables survive the link.
            ret += (
                blank
                + '"-Wl,--eh-frame-hdr", "-u", "_Unwind_Find_FDE", "-nolibc", '
                + '"-nostartfiles", "-lgnarl", "-lgnat", "-lc", "-lunwind",'
            )
        else:
            ret += (
                blank
                + '"-Wl,--eh-frame-hdr", "-nolibc", "-nostartfiles", '
                + '"-lgnarl", "-lgnat", "-lc", "-lunwind",'
            )

        # Add spec file for bare-metal targets to support C++
        # constructors/destructors. Exclude CHERI and LLVM because they do
        # not support C++.
        if not target.has_cheri and not using_llvm_compiler():
            ret += "\n" + blank + '"--specs=${RUNTIME_DIR(Ada)}/link-zcx.spec",'

    # Add linker paths (only needed for bare-metal runtimes)
    if not target.is_os_target:
        # Add the user script path first, so that they have precedence
        ret += "\n" + blank + '"-L${RUNTIME_DIR(ada)}/ld_user"'
        # Then the ld dir, whenever a loader names anything in it.
        if any(
            loader.linker_scripts or loader.included_scripts
            for loader in target_loaders
        ):
            ret += ",\n" + blank + '"-L${RUNTIME_DIR(ada)}/ld"'

    # Add remaining linker switches
    if len(switches) > 0:
        if not ret.endswith(",\n"):
            ret += ",\n"
        ret += blank
        ret += (",\n" + blank).join(switches)
    if ret.endswith(",\n"):
        ret = ret[:-2]
    ret += ") &\n" + blank + "Compiler.Common_Required_Switches;\n"
    indent = 6
    blank = indent * " "

    # Add LOADER specific options
    if len(target_loaders) > 0:
        ret += "\n" + blank
        ret += "case Loader is\n"
        indent += 3
        blank = indent * " "

        for loader in target_loaders:
            ret += blank
            ret += f'when "{loader.name}" =>\n'
            indent += 3
            blank = indent * " "

            # An arm links exactly what its loader names, in that order: a
            # script shared by several arms is named by each of them, since
            # -T order is part of the link.
            switches = [
                f'"-T", "{dest_name(script)}"' for script in loader.linker_scripts
            ]
            switches += [f'"{sw}"' for sw in loader.link_switches]
            if len(switches) > 0:
                ret += blank
                ret += "for Required_Switches use Linker'Required_Switches"
                ret += " &\n" + blank + "  "
                ret += f"({(',\n   ' + blank).join(switches)});\n"
            indent -= 3
            blank = indent * " "

        indent -= 3
        blank = indent * " "
        ret += f"{blank}end case;\n"

    ret += (
        "   end Linker;\n"
        "]]>\n"
        "   </config>\n"
        "  </configuration>\n"
        "</gprconfig>\n"
    )
    return ret


def _offered_loaders(
    target: AbstractTarget, loaders: tuple[Loader, ...]
) -> tuple[Loader, ...]:
    """The loaders the generated project offers: the ones the runtime declares,
    plus ``USER`` for a bare-metal runtime.

    ``USER`` belongs to bare-metal linking, not to a runtime. The end user may
    always drop their own script in ``ld_user``. So the infrastructure adds it,
    instead of every target repeating it.

    :param target: the target the runtime is being built for.
    :param loaders: what the runtime's components declared, in offering order.
    :returns: the loaders, in offering order.
    :raises ValueError: if a component declares ``USER`` itself.
    """
    if target.is_os_target:
        # An OS links with its own tooling: no user-supplied script to allow for.
        return loaders

    if any(loader.name == "USER" for loader in loaders):
        raise ValueError(
            "Target configuration error: USER loader cannot be defined by"
            " a target, it's reserved to allow users to specify their own"
            " linker script"
        )

    return loaders + (Loader("USER"),)


def _default_loader(target: AbstractTarget, names: list[str]) -> str:
    """The loader the generated project selects with no ``-XLOADER``.

    :param target: the target the runtime is being built for.
    :param names: the offered loader names, in offering order.
    :returns: ``target.default_loader``, or the first name offered.
    :raises ValueError: if ``default_loader`` names a loader nothing declared.
    """
    default = target.default_loader
    if default is None:
        return names[0]
    if default not in names:
        raise ValueError(
            f"Target {target.name}: default loader '{default}' is not offered;"
            f" the runtime offers {', '.join(names)}"
        )
    return default


def get_global_compiler_switches() -> tuple[str, ...]:
    """
    Returns a tuple of global compiler switches to be added to all
    runtimes, regardless of target and profile.
    """

    #   -fno-tree-loop-distribute-patterns (GCC flag):
    #     This optimization looks for code patterns that can be replaced
    #     with library calls, for example memset and strlen. This creates
    #     a hidden runtime dependency that is considered undesirable by many
    #     of our customers. strlen is particularly problematic, as its not
    #     provided in our light and light-tasking runtimes.
    #   For GNAT LLVM, use the similar switch -fno-builtin.
    if using_gcc_compiler():
        return ("-fno-tree-loop-distribute-patterns",)
    elif using_llvm_compiler():
        return ("-fno-builtin",)

    return ()
