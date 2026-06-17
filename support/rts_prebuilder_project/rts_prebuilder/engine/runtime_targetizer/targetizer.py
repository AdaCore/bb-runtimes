#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path

from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.base_types import ValidBaseProfileType
from rts_prebuilder.end_user_data.logger import get_logger

from ..common.infrastructure_interface import get_resolver
from .find_json_desc import locate_json_descriptor
from .target_checker import validate_target
from .read_json_desc import SourcesTreeBase
from .targetized_runtime import TargetizedRuntime

log = get_logger(__name__)


class RuntimeTargetizer:
    """
    Responsible for generating the buildable target specific runtime.

    From an RTS source tree and its JSON descriptor and a Target instance,
    it generates the runtime directories for each profile defined in the
    target instance, applies template configurations and generates
    some build system files (for gprbuild usage).

    It can also install the runtimes in the destination through the install method.
    """

    _target: AbstractTarget
    """
    Target instance to use for the generation
    """

    _runtime_sources_tree: SourcesTreeBase
    """
    Tree-like structre of the runtime sources to use for the generation,
    as loaded based on the JSON descriptor
    """

    _created_runtimes: list[TargetizedRuntime]
    """
    List of created targetized runtimes
    """

    def __init__(
        self, target: AbstractTarget, json_descriptor: Path | None = None
    ) -> None:
        """
        :param target: Target instance to use for the installation
        :param json_descriptor: Optional JSON descriptor for runtime sources
        """

        # Perform runtime checks on the target instance
        # and use it to init the _target attribute
        if not isinstance(target, AbstractTarget):
            raise TypeError("invalid target argument")
        validate_target(target)
        self._target = target

        # Init the _runtime_sources_tree attribute
        # Auto-find JSON descriptor if not given
        if not json_descriptor:
            json_descriptor = locate_json_descriptor(self._target)

        if not json_descriptor:
            raise RuntimeError("At this point json_descriptor must be set")
        self._runtime_sources_tree = SourcesTreeBase(json_file=str(json_descriptor))

        # Initialize the list of created runtimes
        self._created_runtimes = []

    @property
    def created_runtimes(self) -> list[TargetizedRuntime]:
        """
        Access the list of created targetized runtimes.

        :return: List of created TargetizedRuntime instances.
        """
        return self._created_runtimes

    def create_targetized_runtimes(
        self,
        filter_base_profile_names: list[ValidBaseProfileType] | None = None,
    ) -> list[TargetizedRuntime]:
        """
        Create targetized runtimes for the target profiles.

        For each profile defined in the target.system_ads:
            - Create a TargetizedRuntime instance (this uses all Target class sources)
            - Generate specific files and directories in the runtime

        :param filter_base_profile_names: Subset of runtime profiles to create among those
            available in the RTS source tree.
            (example: ["light", "light-tasking"])
        :return: List of created TargetizedRuntime instances.
        """

        # Alias runtime_sources_base of the tree for easier access
        runtime_sources_base: SourcesTreeBase = self._runtime_sources_tree

        # Clear and reinitialize the created runtimes list
        self._created_runtimes = []

        for profile_name in self._target.system_ads:
            # Get base_profile_name
            base_profile_name = self._target.base_profile(profile_name)

            # If we have been given a list of profiles, skip the profiles that
            # are not in the list.
            if (
                filter_base_profile_names is not None
                and base_profile_name not in filter_base_profile_names
            ):
                continue
            # Add to the created runtimes list
            self._created_runtimes.append(
                TargetizedRuntime(self._target, profile_name, runtime_sources_base)
            )

        if len(self._created_runtimes) == 0:
            log.warning(
                "No target runtime created for target %s with profiles filter %s"
                " (its system_ads is: %s)",
                self._target,
                filter_base_profile_names,
                self._target.system_ads,
            )

        log.info(
            "----> Created %d targetized runtimes for target %s",
            len(self._created_runtimes),
            self._target.name,
        )

        # Return the list of created runtimes
        return self._created_runtimes

    def install_targetized_runtimes(
        self,
        destination: Path,
        link: bool = False,
        overwrite: bool = False,
        save_resolution_record: bool = True,
    ) -> list[Path]:
        """
        Install the previously created targetized runtimes to the destination directory.

        :param destination: Path to install the final runtimes to.
        :param link: If True, create symlinks instead of copying files
        :param overwrite: If True, overwrite existing runtimes in the destination
        :param save_resolution_record: If True, save resolution records
        :return: List of installed runtime paths.
        """

        if len(self._created_runtimes) == 0:
            raise RuntimeError(
                "No targetized runtimes to install for target "
                f"{self._target.name},"
                "Make sure to call create_targetized_runtimes() first, "
                "and that it created at least one runtime."
            )

        # Init list of runtimes installations
        runtimes_installations: list[Path] = []

        log.info(
            "----> Installing %d targetized runtimes for target %s to %s",
            len(self._created_runtimes),
            self._target.name,
            destination,
        )

        for targetized_runtime in self._created_runtimes:
            # INSTALLATION
            output_dir: Path = (destination / targetized_runtime.name).resolve()

            # Call the install handler
            targetized_runtime.install(output_dir, link, overwrite)

            # Append runtime path to the list of installed runtimes
            runtimes_installations.append(output_dir)

            if save_resolution_record:
                get_resolver().write_resolution_records_to_metadata(
                    filename="targetizer_resolution_record.csv",
                    subdir=Path(targetized_runtime.name),
                )

        # Return the list of installed runtimes
        return runtimes_installations
