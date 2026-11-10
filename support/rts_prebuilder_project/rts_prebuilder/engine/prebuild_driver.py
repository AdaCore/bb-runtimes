#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path

from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.base_types import ValidBaseProfileType
from rts_prebuilder.end_user_data.logger import get_logger

from .infrastructure_interface import get_resolver
from .target_checker import validate_target
from .profile_specializer import specialize
from .runtime_writer import RuntimeWriter

log = get_logger(__name__)


class RuntimePrebuildDriver:
    """
    Responsible for generating the buildable target specific runtime.

    From a Target instance, it generates one runtime directory per profile the
    target defines. For each, it merges the profile's own components with the
    target's, applies the template configurations, and generates the build
    system files gprbuild needs.

    It can also install the runtimes in the destination through the install method.
    """

    _target: AbstractTarget
    """
    Target instance to use for the generation
    """

    created_runtimes_writers: list[RuntimeWriter]
    """
    List of prepared runtime writers, one per created profile.
    """

    def __init__(self, target: AbstractTarget) -> None:
        """
        :param target: Target instance to use for the installation
        """

        if not isinstance(target, AbstractTarget):
            raise TypeError("invalid target argument")
        validate_target(target)
        self._target = target

        self.created_runtimes_writers = []

    def create_runtimes_writers(
        self,
        filter_base_profile_names: list[ValidBaseProfileType] | None = None,
    ) -> list[RuntimeWriter]:
        """
        Prepare the runtimes for the target profiles.

        For each profile the target declares, the target is specialized for that
        profile into a RuntimeSpec, and a RuntimeWriter is prepared from it
        (ready to install).

        :param filter_base_profile_names: Subset of runtime profiles to create among those
            defined by the target.
            (example: ["light", "light-tasking"])
        :return: List of prepared RuntimeWriter instances.
        """

        self.created_runtimes_writers = []

        for profile in self._target.profiles:
            if (
                filter_base_profile_names is not None
                and profile.base_profile not in filter_base_profile_names
            ):
                continue
            spec = specialize(self._target, profile)
            self.created_runtimes_writers.append(RuntimeWriter(spec))

        if len(self.created_runtimes_writers) == 0:
            log.warning(
                "No target runtime created for target %s with profiles filter %s"
                " (its profiles are: %s)",
                self._target,
                filter_base_profile_names,
                [p.name for p in self._target.profiles],
            )

        log.info(
            "----> Created %d prebuilt runtimes for target %s",
            len(self.created_runtimes_writers),
            self._target.name,
        )

        return self.created_runtimes_writers

    def install_runtimes(
        self,
        destination: Path,
        link: bool = False,
        overwrite: bool = False,
        save_resolution_record: bool = True,
    ) -> list[Path]:
        """
        Install the previously created prebuilt runtimes to the destination directory.

        :param destination: Path to install the final runtimes to.
        :param link: If True, create symlinks instead of copying files
        :param overwrite: If True, overwrite existing runtimes in the destination
        :param save_resolution_record: If True, save resolution records
        :return: List of installed runtime paths.
        """

        if len(self.created_runtimes_writers) == 0:
            raise RuntimeError(
                "No prebuilt runtimes to install for target "
                f"{self._target.name},"
                "Make sure to call create_runtimes_writers() first, "
                "and that it created at least one runtime."
            )

        runtimes_installations: list[Path] = []

        log.info(
            "----> Installing %d prebuilt runtimes for target %s to %s",
            len(self.created_runtimes_writers),
            self._target.name,
            destination,
        )

        for runtime in self.created_runtimes_writers:
            output_dir: Path = (destination / runtime.name).resolve()

            runtime.install(output_dir, link, overwrite)

            runtimes_installations.append(output_dir)

            if save_resolution_record:
                get_resolver().write_resolution_records_to_metadata(
                    filename="prebuild_resolution_record.csv",
                    subdir=Path(runtime.name),
                )

        return runtimes_installations
