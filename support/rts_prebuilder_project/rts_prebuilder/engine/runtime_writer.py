#
# Copyright (C) 2025-2026, AdaCore
#

import shutil
from pathlib import Path

from rts_prebuilder.abstract_infrastructure import SourceFile
from rts_prebuilder.end_user_data.logger import get_logger

from .file_pair import FilePair
from .gpr_project_gen import (
    render_ravenscar_build_gpr,
    render_runtime_build_gpr,
    render_target_options_gpr,
)
from .runtime_spec import RuntimeSpec

log = get_logger(__name__)


class RuntimeWriter:
    """
    Turns a ``RuntimeSpec`` into an installable runtime.

    It is pure mechanism. It resolves the spec's component sources into a list
    of FilePair. On install it writes those out, together with the generated
    build files: gprbuild projects, runtime.xml, path files and scaffolding
    directories. It knows nothing of targets or profiles beyond what the spec
    carries.
    """

    def __init__(self, spec: RuntimeSpec) -> None:
        self._spec = spec
        self.name = spec.name
        self._file_pairs: list[FilePair] = []
        self._programming_languages: dict[str, set[str]] = {
            lib: set() for lib in spec.libs_tuple
        }

        self._fill_source_files()
        self._extract_programming_languages()

        log.debug("Prepared runtime '%s', ready for install", self.name)

    def _append_source_files(self, *sources: SourceFile) -> None:
        """Resolve each SourceFile into a FilePair and add it to the collection."""
        for source in sources:
            file_pair = FilePair(source)
            log.debug("Inserting new filepair %s", file_pair)
            self._file_pairs.append(file_pair)

    def _fill_source_files(self) -> None:
        """
        Gather the system.ads and the spec's resolved sources into the file
        pairs. The sources are the profile's components plus the target's,
        already flattened. They are split per lib, with one extra bucket for
        the sources outside any lib.
        """
        self._append_source_files(self._spec.system_ads_source_file)

        for lib in self._spec.libs_tuple:
            source_files_to_add: list[SourceFile] = [
                s for s in self._spec.sources if s.dest_subdir == Path(lib)
            ]

            if not source_files_to_add:
                raise ValueError(
                    f"The selected components don't provide support for"
                    f" lib{lib} (runtime {self.name})"
                )

            log.info(
                "- Adding %d source files for lib '%s'",
                len(source_files_to_add),
                lib,
            )

            self._append_source_files(*source_files_to_add)

        # Sources that are not in any specific lib (gnat or gnarl)
        no_lib_sources = [
            s
            for s in self._spec.sources
            if s.dest_subdir not in (Path("gnat"), Path("gnarl"))
        ]
        log.info(
            "- Adding %d source files with no specific lib",
            len(no_lib_sources),
        )
        self._append_source_files(*no_lib_sources)

    def _extract_programming_languages(self) -> None:
        """Extract the programming languages used by each lib's source files."""
        for lib in self._spec.libs_tuple:
            extensions_set = self._extensions_in_dir(Path(lib))
            self._programming_languages[lib] = set(
                map(_suffix_to_language_in_gpr, extensions_set)
            )

    def _extensions_in_dir(self, _dir: Path) -> set[str]:
        """Union of file suffixes across the file pairs installed under _dir."""
        extensions: set[str] = set()
        for file_pair in self._file_pairs:
            if file_pair.is_in_dir(_dir):
                extensions |= file_pair.suffixes
        return extensions

    def _generate_scaffolding_for_install_dir(self, output_dir: Path) -> None:
        """
        Generate the specific directories and files for the runtime.

        All args are simple inputs that are not modified.
        """
        spec = self._spec

        # Add user-defined placeholder for ld scripts
        (output_dir / "ld_user").mkdir(parents=True, exist_ok=True)

        # Make directory for user defined libraries
        user_libs = [f"{d}_user" for d in spec.libs_tuple]
        for user_lib in user_libs:
            (Path(output_dir) / user_lib).mkdir(parents=True, exist_ok=True)

        # Generate runtime.xml and write it out
        (output_dir / "runtime.xml").write_text(spec.runtime_xml)

        # Write ada_source_path and ada_object_path files
        #   Make sure the user-defined sources come first to preempt
        #   default sources when needed
        (output_dir / "ada_source_path").write_text(
            "\n".join(list(user_libs) + list(spec.libs_tuple)) + "\n"
        )

        (output_dir / "ada_object_path").write_text("adalib\n")

        # Generate the project files (.gpr) used to build the rts. Rendering
        # lives in gpr_project_gen. Here we only hand it the resolved data and
        # write the result out.
        (output_dir / "target_options.gpr").write_text(
            render_target_options_gpr(
                spec.runtime_build_switches,
                spec.runtime_gnarl_switches,
                spec.runtime_debug_switches,
            )
        )

        (output_dir / "runtime_build.gpr").write_text(
            render_runtime_build_gpr(
                target_triple=spec.target_triple,
                is_native=spec.is_native,
                languages=self._programming_languages["gnat"],
            )
        )

        #   Ravenscar project ravenscar_build.gpr if needed
        if "gnarl" in spec.libs_tuple:
            (output_dir / "ravenscar_build.gpr").write_text(
                render_ravenscar_build_gpr(self._programming_languages["gnarl"])
            )

    def install(self, output_dir: Path, link: bool, overwrite: bool) -> None:
        """Installs the runtime's resolved file pairs to the given output dir."""
        log.info(
            "Installing runtime for profile '%s' in %s",
            self._spec.base_profile_name,
            output_dir,
        )

        if output_dir.exists():
            if not overwrite:
                raise FileExistsError(
                    f"The installation directory {output_dir} already exists, "
                    "and overwrite is set to False"
                )
            log.warning(
                "The installation directory %s already exists, and will be removed"
                " because overwrite is set to True",
                output_dir,
            )
            shutil.rmtree(output_dir)

        # Install the source files (each FilePair creates its parent dirs).
        for file_pair in self._file_pairs:
            file_pair.install(
                install_dir=output_dir,
                link=link,
                template_config=self._spec.template_config,
            )

        # Fill the output directory with generated files and directories
        self._generate_scaffolding_for_install_dir(output_dir)


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
