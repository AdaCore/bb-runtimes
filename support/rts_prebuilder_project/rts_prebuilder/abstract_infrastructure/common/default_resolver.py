#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override

from .resolver_core import SourcePathResolver
from .resolver_steps import (
    GccDirResolutionStep,
    GnatDirLibsResolutionStep,
    GnatDirManifestResolutionStep,
    SearchPathsResolutionStep,
)


class DefaultSourcePathResolver(SourcePathResolver):
    """
    Default concrete source path resolver.

    This is just a convenience subclass of SourcePathResolver,
    if infrastructure builders do not want to define their own subclass.

    It calls 4 resolution steps in this order:
    - GnatDirManifestResolutionStep
    - GnatDirLibsResolutionStep
    - SearchPathsResolutionStep (With a method to add search paths at runtime)
    - GccDirResolutionStep
    """

    search_paths_resolver: SearchPathsResolutionStep
    """
    Instance of the search paths resolution step used by this resolver.
    This attribute is exposed to be able to add search paths later
    using the `add_search_paths` method.
    """

    def __init__(self) -> None:
        # We keep a ref on the search paths resolver to be able to add paths later
        self.search_paths_resolver = SearchPathsResolutionStep()

        super().__init__(
            steps=[
                GnatDirManifestResolutionStep(),
                GnatDirLibsResolutionStep(),
                self.search_paths_resolver,
                GccDirResolutionStep(),
            ]
        )

    @override
    def add_search_paths(self, *paths: Path, add_to_start: bool = False) -> None:
        """Add a search path to the search paths resolver step.

        see SearchPathsResolutionStep.add_search_path for details.
        """
        self.search_paths_resolver.add_search_paths(*paths)
