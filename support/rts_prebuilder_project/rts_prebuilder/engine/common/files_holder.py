#
# Copyright (C) 2025-2026, AdaCore
#

import shutil
from pathlib import Path

from rts_prebuilder.abstract_infrastructure import SourceFile
from rts_prebuilder.end_user_data.logger import get_logger

from .file_pair import FilePair, TemplateConfigType

log = get_logger(__name__)


class FilesHolder(object):
    """
    FilesHolder class represents a File Tree, represented as a list of Filepairs

    Its responsibilities are:

    - CRUD operations over the FilePair collection (For now only creation is useful
      but can be extended)
    - Installation of the output RTS tree in a given directory

    .. note::
        TODO 4: This class has been simplified to only become a list of FilePair.
        Consider if we really need this class or if we can just use a list of FilePair
    """

    _file_pairs: list[FilePair]
    """The main collection abstracted by this class."""

    def __init__(self) -> None:
        """
        Initializes an empty FilesHolder object
        """
        self._file_pairs = []

    def append_source_files(self, *sources: SourceFile) -> None:
        """
        Instantiate and append multiple FilePair to the collection
        """

        new_filepairs = [FilePair(source) for source in sources]

        for f in new_filepairs:
            log.debug("Inserting new filepair %s", str(f))
            self._file_pairs.append(f)

    def install(
        self,
        install_dir: Path,
        template_config: TemplateConfigType,
        link: bool,
        overwrite: bool = False,
    ) -> None:
        """
        Simply calls the install method of each FilePair in the collection

        :param install_dir: The root directory where to install the files
        :param template_config: The template configuration to use when installing
                                template files
        :param link: Whether to create hard links instead of copying files
        :param overwrite: Whether to overwrite the install_dir if it already exists
        """

        if install_dir.exists():
            if not overwrite:
                raise FileExistsError(
                    f"The installation directory {install_dir} already exists, "
                    "and overwrite is set to False"
                )

            log.warning(
                "The installation directory %s already exists, and will be removed"
                " because overwrite is set to True",
                install_dir,
            )
            shutil.rmtree(install_dir)

        for f in self._file_pairs:
            f.install(
                install_dir=install_dir, link=link, template_config=template_config
            )

    def get_extensions_set(self, _dir: Path | None = None) -> set[str]:
        """Returns a set of all extensions present in the FilePairs collection.

        :param dir: Optional directory to filter FilePairs by their source path.
        """
        extensions: set[str] = set()
        for file_pair in self._file_pairs:
            if _dir is None or file_pair.is_in_dir(_dir):
                extensions |= file_pair.suffixes

        return extensions
