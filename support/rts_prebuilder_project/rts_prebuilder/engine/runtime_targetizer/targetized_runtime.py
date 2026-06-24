#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path

from typing import Tuple

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    RuntimeConfig,
    SourceFile,
)
from rts_prebuilder.base_types import (
    GnatOrGnarlType,
    ProfileNameType,
    UnresolvedPath,
    ValidBaseProfileType,
)
from rts_prebuilder.end_user_data.compiler_selector import using_gcc_compiler
from rts_prebuilder.end_user_data.logger import get_logger

from ..common.files_holder import FilesHolder
from ..common.infrastructure_interface import (
    get_all_possible_scenarios,
    get_all_sources_listing,
    get_resolver,
    resolve_and_read_file,
)
from .read_json_desc import SourcesTreeBase, get_sources_paths_from_scenario_config
from ..common.rule import complete_scenarios_from_deps
from ..common.scenarios_printer import save_scenarios_to_metadata

log = get_logger(__name__)


class TargetizedRuntime:
    """
    Class abstracting the runtime for a given Target and a given Profile

    Following instantiation, it's ready to install as a complete buildable RTS.

    Usage:
        Instantiate with:
            rts = TargetizedRuntime(...)
            see TargetizedRuntime.__init__
        Install with:
            rts.install(...)
            sett TargetizedRuntime.install
    """

    cfg: RuntimeConfig
    """The runtime configuration properties, see RuntimeConfig class"""

    name: str
    """This is the official name of the runtime (to not confuse with the
    base profile, profile name or target name)"""

    _files_holder: FilesHolder
    """The installable collection of files used by the runtime"""

    _target: AbstractTarget
    """Target instance used to build this runtime"""

    _runtime_sources_tree: SourcesTreeBase
    """Tree-like structure of the runtime sources to use for the installation"""

    _profile_name: ProfileNameType
    """Profile name for this runtime"""

    _base_profile_name: ValidBaseProfileType
    """Base profile name for this runtime"""

    _libs_tuple: Tuple[GnatOrGnarlType, ...]
    """Tuple of libraries (gnat, gnarl) used by this runtime"""

    _programming_languages: dict[GnatOrGnarlType, set[str]]
    """Dictionary of programming languages used by each lib of this runtime"""

    def __init__(
        self,
        target: AbstractTarget,
        profile: ProfileNameType,
        runtime_sources_tree: SourcesTreeBase,
    ) -> None:
        # Init the FilesHolder part with the sources from the target instance
        super().__init__()

        log.info(
            "--> Creating TargetizedRuntime for target '%s' (%s) and profile '%s'",
            target.name,
            target.__class__.__name__,
            profile,
        )

        # Init private attributes
        self._target = target
        self._runtime_sources_tree = runtime_sources_tree
        self._profile_name = profile
        self._base_profile_name = target.base_profile(profile)
        self._libs_tuple = _libs_tuple_from_base_profile_name(self._base_profile_name)

        # Check that the given runtime sources have all these needed libs
        runtime_sources_tree.check_if_libs_present(*self._libs_tuple)

        # Init programming languages tracking dict
        self._programming_languages = {lib: set() for lib in self._libs_tuple}

        # Init the public attributes
        self.files_holder = FilesHolder()
        self.name = self._target.runtime_name_generator(profile)

        # ---------------------------------------
        # INIT self.cfg
        # ---------------------------------------

        self.cfg = RuntimeConfig()
        # 1. Fill self.cfg.build_flags
        self._set_build_flags()
        # 2. Init self.cfg.config_files
        self._load_default_config_files()
        # 3. Init self.cfg.rts_vars
        # 3.a From profile and dependencies
        self._init_scenarios_from_profile_then_deps()
        # 3.b Amend the runtime config based on target specifics
        self._target.amend_rts(profile, self.cfg)

        # ---------------------------------------
        # INIT the source files held in self.files_holder
        # ---------------------------------------

        # 0. Prepare the resolver with target-specific search paths
        self._absorbe_source_search_paths_from_target()

        # 1. Add system.ads from Target
        # TODO:3 This can be merged in _fill_source_files_from_target by adding
        # the logic into AbstractTarget.__init__ or get_sources()
        self._add_system_ads_to_source_listing()

        # 2. Gather the sources from _runtime_sources_tree based on scenarios
        self._fill_source_files_from_libs_based_on_scenarios()

        # 3. Gather the sources from the Target instance itself
        self._fill_source_files_from_target()

        # ---------------------------------------
        # INIT self._programming_languages
        # ---------------------------------------
        # (Requires FilesHolder to be filled completely, to register all extensions)
        self._extract_programming_languages_from_files_holder()

        # ---------------------------------------
        log.debug("Instantiated TargetizedRuntime: %s, ready for install", self)

    def _load_default_config_files(
        self,
    ) -> None:
        """Sets the self.cfg.config_files property of the runtime config"""

        if self._target.readme_file:
            readme_content = resolve_and_read_file(self._target.readme_file)
            self.cfg.config_files.update({"README": readme_content})

        if (
            not self._target.is_os_target
            and not self._target.has_cheri
            and using_gcc_compiler()
        ):
            # Ensure that the spec file is available for bare-metal
            # targets to support C++ constructors/destructors and
            # exception handling tables when supporting exception
            # propagation. Exclude CHERI and LLVM because they do not
            # support C++.

            if self._base_profile_name in ["light", "light-tasking"]:
                spec_content = resolve_and_read_file(
                    Path("datafiles/link-noexceptions.spec")
                )
                self.cfg.config_files.update({"link-noexceptions.spec": spec_content})
            elif self._base_profile_name == "embedded":
                spec_content = resolve_and_read_file(Path("datafiles/link-zcx.spec"))
                self.cfg.config_files.update({"link-zcx.spec": spec_content})

    def _set_build_flags(self) -> None:
        """
        Sets self.cfg.build_flags property of the runtime config
        """

        # Flags for keys in target_options.gpr.in must end in _flags. This is
        # to avoid duplicating the list of flags in support/bsp_sources/installer.py.
        runtime_build_flags: dict[str, list[str]] = {
            "source_dirs": [],
            "common_flags": ["-ffunction-sections", "-fdata-sections"],
            "common_gnarl_flags": [],
            "common_debug_flags": ["-g"],
            "asm_flags": [],
            "c_flags": ["-DIN_RTS", "-Dinhibit_libc", "-DLIGHT_RUNTIME"],
            "shared_linker_flags": [],
        }

        if using_gcc_compiler():
            # GNAT-LLVM doesn't support -fcallgraph-info
            runtime_build_flags["common_flags"].append("-fcallgraph-info=su,da")

        self.cfg.build_flags = runtime_build_flags

    def _init_scenarios_from_profile_then_deps(
        self,
    ) -> None:
        """Sets the sefl.cfg.rts_vars property of the runtime config"""

        rts_generator_instance = self._target.profile_to_scenarios_generator
        self.cfg.rts_vars = rts_generator_instance.profile_to_scenarios(
            self._profile_name
        )

        # Complete scenarios config with scenarios from the dependencies
        complete_scenarios_from_deps(
            self.cfg.rts_vars,
            all_possible_scenarios=get_all_possible_scenarios(),
            all_sources=get_all_sources_listing(),
        )

    def _absorbe_source_search_paths_from_target(self) -> None:
        """Absorbe source search paths from the Target instance"""
        paths = self._target.get_sources_search_paths()

        if len(paths) == 0:
            log.info(
                "- No target-specific source search paths from target %s",
                self._target.name,
            )
        else:
            log.info(
                "- Adding source search paths for target %s: %s",
                self._target.name,
                paths,
            )

        get_resolver().add_search_paths(*paths)

    def _add_system_ads_to_source_listing(self) -> None:
        """Add system.ads file from Target to the files holder"""

        system_ads_path_str = self._target.system_ads[self._profile_name]

        # By default, system.ads files are searched for in
        # bb-runtimes/src/system.
        # For custom runtimes the value may instead carry a directory
        # component, in which case we look it up as a regular source file.
        # Path(...).name strips any directory part, so when it differs from
        # the original value the value contained a directory component.
        if Path(system_ads_path_str).name != system_ads_path_str:
            system_ads_path = Path(system_ads_path_str)
        else:
            system_ads_path = Path("system") / system_ads_path_str

        # "Cast" to UnresolvedPath to indicate that this path
        # still needs to go through the resolver
        system_ads_path = UnresolvedPath(system_ads_path)

        file = SourceFile(
            unresolved_src_path=system_ads_path,
            dest_path=Path("system.ads"),
            dest_subdir=Path("gnat"),
        )
        log.info(
            "- Adding system.ads from target %s: %s",
            self._target.__class__.__name__,
            system_ads_path,
        )
        self.files_holder.append_source_files(file)

    def _fill_source_files_from_libs_based_on_scenarios(
        self,
    ) -> None:
        """
        Uses the scenario configuration to selects the sources files to
        add to the runtime and append them to the files_holder.
        """
        # Load the scenarios associated with this runtime we're making
        runtime_scenario_dict = self.cfg.rts_vars

        # Fetch sources from the JSON descriptor based on the scenarios config
        # and from the Target instance
        for lib in self._libs_tuple:
            # lib: GnatOrGnarlType

            # Now for scenarios that have not been set by the user
            # or by the target.amend_rts() method, we set them to their
            # default values as defined in the JSON descriptor
            # i.e. the first value in the list of possible values
            for scenario, vals in self._runtime_sources_tree.scenarios(lib).items():
                default_scenario_value = vals[0]
                if scenario not in runtime_scenario_dict:
                    runtime_scenario_dict[scenario] = default_scenario_value

            # Save scenario configuration to metadata and log it for tracability
            save_scenarios_to_metadata(runtime_scenario_dict, self.name, lib)

            # Fetch sources deduced from the JSON descriptor + The scenarios config
            sources: list[str] = get_sources_paths_from_scenario_config(
                self._runtime_sources_tree.sources(lib), runtime_scenario_dict
            )

            # Instantiate SourceFile instances for each source path
            source_files_to_add: list[SourceFile] = [
                SourceFile(UnresolvedPath(Path(s)), dest_subdir=Path(lib))
                for s in sources
            ]

            log.info(
                "- Adding %d source files for lib '%s' based on scenarios",
                len(source_files_to_add),
                lib,
            )

            # Add those sources to the runtime files_holder
            self.files_holder.append_source_files(*source_files_to_add)

    def _fill_source_files_from_target(
        self,
    ) -> None:
        """
        Fills source files from the self._target instance.

        These sources are independent of the scenario configuration.
        They are extracted from the AbstractTarget.get_sources() method.
        """
        source_files_to_add: list[SourceFile] = []

        # Add target-specific source files from each lib (gnat, gnarl...)
        # based on which are in the _libs_tuple
        for lib in self._libs_tuple:
            # lib: GnatOrGnarlType
            # Get the sources from the target instance
            source_files_to_add.extend(self._target.get_sources(dest_subdir=Path(lib)))

            log.info(
                "- Adding %d source files for lib '%s' from target class '%s'",
                len(source_files_to_add),
                lib,
                self._target.__class__.__name__,
            )

        # Get the sources that are not in any specific lib (gnat or gnarl)
        no_lib_sources = self._target.get_sources(
            exclude_subdirs=[Path("gnat"), Path("gnarl")]
        )
        log.info(
            "- Adding %d source files with no specific lib from target class '%s'",
            len(no_lib_sources),
            self._target.__class__.__name__,
        )

        source_files_to_add.extend(no_lib_sources)

        # Add those sources to the runtime files_holder
        self.files_holder.append_source_files(*source_files_to_add)

    def _extract_programming_languages_from_files_holder(
        self,
    ) -> None:
        """
        Extracts the programming languages used by the given source files
        """
        for lib in self._libs_tuple:
            extensions_set = self.files_holder.get_extensions_set(Path(lib))
            self._programming_languages[lib] = set(
                map(_suffix_to_language_in_gpr, extensions_set)
            )

    def _generate_scaffolding_for_install_dir(
        self,
        output_dir: Path,
    ) -> None:
        """
        Generate the specific directories and files for a given runtime

        All args are simple inputs that are not modified.
        """

        # Add user-defined placeholder for ld scripts
        (output_dir / "ld_user").mkdir(exist_ok=True)

        # Make directory for user defined libraries
        user_libs = [f"{d}_user" for d in self._libs_tuple]
        for user_lib in user_libs:
            (Path(output_dir) / user_lib).mkdir(parents=True, exist_ok=True)

        # Install target and run-time specific configuration files
        for name, content in self.cfg.config_files.items():
            (output_dir / name).write_text(content)

        # Generate runtime.xml and write it out
        (output_dir / "runtime.xml").write_text(
            self._target.dump_runtime_xml(self._base_profile_name, self.cfg)
        )

        # Write ada_source_path and ada_object_path files
        #   Make sure the user-defined sources come first to preempt
        #   default sources when needed
        (output_dir / "ada_source_path").write_text(
            "\n".join(list(user_libs) + list(self._libs_tuple)) + "\n"
        )

        (output_dir / "ada_object_path").write_text("adalib\n")

        # And generate the project files (.gpr) used to build the rts

        #   Start with target_options.gpr
        build_flags = {
            f: '",\n        "'.join(self.cfg.build_flags[f])
            for f in self.cfg.build_flags
            if f.endswith("_flags")
        }
        target_options_content = resolve_and_read_file(
            Path("datafiles/target_options.gpr.in")
        )
        target_options_content = target_options_content.format(**build_flags)
        (output_dir / "target_options.gpr").write_text(target_options_content)

        ravenscar_gpr_template = resolve_and_read_file(
            Path("datafiles/runtime_build.gpr.in")
        )

        #   Runtime project runtime_build.gpr

        target_directive = (
            f'for Target use "{self._target.target}";'
            if not self._target.is_native
            else ""
        )

        runtime_spark_units = resolve_and_read_file(
            Path("datafiles/runtime_spark_units.lst")
        ).splitlines()

        (output_dir / "runtime_build.gpr").write_text(
            ravenscar_gpr_template.format(
                target_directive=target_directive,
                source_dirs='", "'.join(["gnat_user", "gnat"]),
                runtime_spark_units='", "'.join(runtime_spark_units),
                languages='", "'.join(sorted(self._programming_languages["gnat"])),
            )
        )

        #   Ravenscar project ravenscar_build.gpr if needed
        if "gnarl" in self._libs_tuple:
            ravenscar_gpr_template = resolve_and_read_file(
                Path("datafiles/ravenscar_build.gpr.in")
            )
            (output_dir / "ravenscar_build.gpr").write_text(
                ravenscar_gpr_template.format(
                    source_dirs='", "'.join(["gnarl_user", "gnarl"]),
                    languages='", "'.join(sorted(self._programming_languages["gnarl"])),
                )
            )

    def install(self, output_dir: Path, link: bool, overwrite: bool) -> None:
        """Installs the files_holder of the instance to the given output dir"""
        log.info(
            "Installing runtime for profile '%s' in %s"
            % (self._base_profile_name, output_dir)
        )

        # Install the sources files from the targetized_runtime
        # This also creates the directory and checks the overwrite arg
        self.files_holder.install(
            output_dir, self._target.template_config, link, overwrite
        )

        # Fill the output directory with generated files and directories
        self._generate_scaffolding_for_install_dir(
            output_dir,
        )


def _suffix_to_language_in_gpr(suffix: str) -> str:
    """
    Maps a file suffix to a gprbuild language identifier.

    :param suffix: The file suffix, including the leading dot (e.g., '.adb', '.c')
    :return: The corresponding gprbuild language identifier (e.g., 'Ada', 'C')
    """
    if suffix in [".adb", ".ads", ".ada"]:
        return "Ada"
    elif suffix in [".c", ".h"]:
        return "C"
    elif suffix in [".S"]:
        return "Asm_Cpp"
    elif suffix in [".s"]:
        return "Asm"
    else:
        return "Ada"


def _libs_tuple_from_base_profile_name(
    base_profile_name: ValidBaseProfileType,
) -> Tuple[GnatOrGnarlType, ...]:
    libs: Tuple[GnatOrGnarlType, ...]
    if "embedded" in base_profile_name or "tasking" in base_profile_name:
        libs = ("gnat", "gnarl")
    else:
        libs = ("gnat",)

    return libs
