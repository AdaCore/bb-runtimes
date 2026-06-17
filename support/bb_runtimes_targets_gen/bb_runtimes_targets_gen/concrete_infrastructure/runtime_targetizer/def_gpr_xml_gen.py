#
# Copyright (C) 2025-2026, AdaCore
#

from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.base_types import ProfileNameType, ScenariosConfigurationType
from rts_prebuilder.end_user_data.compiler_selector import (
    using_gcc_compiler,
    using_llvm_compiler,
)


def default_runtime_xml_generator(
    target: AbstractTarget,
    profile_name: ProfileNameType,
    scenario_config: ScenariosConfigurationType,
) -> str:
    """Generates the runtime.xml file that gives the configuration to gprbuild

    :param target: Target for which the xml file is generated
    :param profile_name: ProfileNameType for which the xml file is generated
    :param scenario_config: ScenariosConfigurationType for which the xml file is generated
    :return: the content of the runtime.xml file as a string
    """
    ret = '<?xml version="1.0" ?>\n\n'
    ret += "<gprconfig>\n"
    ret += "  <configuration>\n"
    ret += "    <config><![CDATA[\n"

    target_loaders = _process_loaders_from_target(target)

    if len(target_loaders) > 0:
        ret += f'   type Loaders is ("{'", "'.join(target_loaders)}");\n'
        ret += f'   Loader : Loaders := external("LOADER", "{target_loaders[0]}");\n\n'

    # Add Compiler package
    ret += "   package Compiler is\n"

    compiler_switches = target.compiler_switches + get_global_compiler_switches()

    if len(compiler_switches) > 0:
        ret += (
            f'      Common_Required_Switches := ("{'", "'.join(compiler_switches)}");\n'
        )
    else:
        ret += "      Common_Required_Switches := ();\n"

    if len(target.c_switches) > 0:
        ret += f'      C_Required_Switches := ("{'", "'.join(target.c_switches)}");\n'

    ret += "\n"

    for lang in ("Ada", "C", "C++", "Asm", "Asm2", "Asm_Cpp"):
        w = "      "
        ret += w + f'for Leading_Required_Switches ("{lang}") use\n'
        w = "         "
        ret += w + f'Compiler\'Leading_Required_Switches ("{lang}") &\n'
        ret += w + "Common_Required_Switches"
        if lang != "Ada" and len(target.c_switches) > 0:
            ret += " &\n" + w
            ret += "C_Required_Switches"
        ret += ";\n"
    ret += "   end Compiler;\n\n"

    # Add Linker package
    switches = []
    for sw in target.linker_switches:
        if sw.loader is None:
            if sw.loader == "":
                raise ValueError("empty string is not a valid loader, use None")
            switches.append(f'"{sw.switch}"')

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
    elif scenario_config["RTS_Profile"] != "embedded":
        # For the Light and Light Tasking runtimes we have the choice of
        # either using libgcc or our Ada libgcc replacement. For the
        # later choice we do not link with any of the standard libraries.
        if scenario_config["Certifiable_Packages"] == "yes":
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
            ret += (
                blank
                + '"-nolibc", '
                + '"-Wl,--start-group,'
                + "-lgnarl,-lgnat,-lc,-lgcc,-lgcc_eh,"
                + '--end-group",'
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
        # And then our own script(s), if any
        if len(target.linker_scripts) > 0:
            ret += ",\n" + blank + '"-L${RUNTIME_DIR(ada)}/ld"'

    # Add remaining linker switches
    if len(switches) > 0:
        if not ret.endswith(",\n"):
            ret += ",\n"
        ret += blank
        ret += (",\n" + blank).join(switches)
        blank = indent * " "
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
            ret += f'when "{loader}" =>\n'
            if loader == "USER":
                continue
            indent += 3
            blank = indent * " "

            switches = []
            for linker_script in target.linker_scripts:
                if linker_script.loaders is None or loader in linker_script.loaders:
                    switches.append(f'"-T", "{linker_script.dest_path}"')
            for sw in target.linker_switches:
                if sw.loader and sw.loader == loader:
                    switches.append(f'"{sw.switch}"')
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


def _process_loaders_from_target(target: AbstractTarget) -> tuple[str, ...]:
    """Helper to process the loaders property of a target

    TODO:3 Maybe we need to run this in target_checker.py ?
    but today only default_runtime_xml_generator() uses it. so
    it might show unknown inconsistencies (either in its logic
    or in the target definition).
    """

    # Gather loaders from ld scripts
    if len(target.loaders) > 0:
        if target.is_os_target:
            # OS targets, return as is
            return tuple(target.loaders)

        else:
            # Add user loader for bare metal targets (and ensure not already defined)
            if "USER" in target.loaders:
                raise Exception(
                    "Target configuration error: USER loader cannot be defined by"
                    " a target, it's reserved to allow users to specify their own"
                    " linker script"
                )

            return tuple(list(target.loaders) + ["USER"])

    else:  # No loaders defined by ld scripts

        if target.is_os_target:
            # OS targets are not required to have loaders
            return ()
        else:
            # Bare metal targets must have at least one loader
            # defined by their ld scripts, unless they have
            # exactly one ld script, in which case we assume
            # DEFAULT
            match len(target.linker_scripts):
                case 0:
                    # No ld script, so end user is expected to profide the linker script
                    # and the loader
                    return ("USER",)

                case 1:
                    # One ld script, so we assume DEFAULT loader
                    # (and add USER to allow user-defined scripts)
                    return ("DEFAULT", "USER")

                case _:
                    raise Exception(
                        "Target configuration error: If target defines many"
                        " linker scripts, at least one must define loaders."
                    )


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
