import os

from support import is_string
from support.files_holder import FilesHolder, FilePair


class LdScript(FilePair):
    def __init__(self, dst, src, loaders):
        super(LdScript, self).__init__(dst=dst, src=src)
        if loaders is None:
            self._loaders = None
        elif is_string(loaders):
            if len(loaders) == 0:
                self._loaders = tuple()
            else:
                self._loaders = (loaders, )
        elif isinstance(loaders, list):
            self._loaders = tuple(loaders)
        elif isinstance(loaders, tuple):
            self._loaders = loaders
        else:
            assert False, "unexpected type for loader %s" % str(loaders)

    @property
    def loaders(self):
        return self._loaders

    def add_loader(self, loader):
        if self._loaders is None:
            self._loaders = (loader, )
        else:
            self._loaders += (loader,)

    @property
    def name(self):
        return self._dst


class ArchSupport(FilesHolder):
    """Handles the startup files and linker scripts"""

    def __init__(self):
        super(ArchSupport, self).__init__()
        self._ld_scripts = []
        self._ld_switches = []
        if self.parent is not None:
            self._parent = self.parent()
        else:
            self._parent = None

    @property
    def parent(self):
        return None

    @property
    def loaders(self):
        """If the bsp supports various loading mechanisms, then the list
        should be returned here."""
        if self._parent is not None:
            return self._parent.loaders
        else:
            return None

    @property
    def ld_scripts(self):
        if self._parent is not None:
            ret = self._parent.ld_scripts
        else:
            ret = []
        ret += self._ld_scripts
        return ret

    @property
    def ld_switches(self):
        if self._parent is not None:
            ret = self._parent.ld_switches
        else:
            ret = []
        ret += self._ld_switches
        return ret

    def add_gnat_source(self, source):
        self.add_source('gnat', source)

    def add_gnat_sources(self, *args):
        for arg in args:
            self.add_gnat_source(arg)

    def add_gnarl_source(self, source):
        self.add_source('gnarl', source)

    def add_gnarl_sources(self, *args):
        for arg in args:
            self.add_gnarl_source(arg)

    def add_linker_script(self, script, dst=None, loader=''):
        """Adds a new linker script to the BSP.
        """
        assert is_string(script)

        if dst is None:
            # not a pair: just copy the script without renaming it
            obj = LdScript(os.path.basename(script), script, loader)
        else:
            obj = LdScript(dst, script, loader)

        assert obj not in self.ld_scripts, \
            "duplicated ld script name %s" % str(obj)

        self._ld_scripts.append(obj)

    def add_linker_switch(self, switch, loader=None):
        """Adds additional linker switch to the BSP.

        if loader is None, then the switch is applicable whatever the current
        loader used.
        """
        self._ld_switches.append({
            'switch': switch,
            'loader': loader})

    def get_sources(self, lib):
        if self._parent is not None:
            ret = self._parent.get_sources(lib)
        else:
            ret = []
        if lib in self.dirs:
            ret += self.dirs[lib]
        return ret
