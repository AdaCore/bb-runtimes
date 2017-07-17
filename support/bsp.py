import copy
import os

from files_holder import FilesHolder


class BSP(FilesHolder):
    """Handles the startup files and linker scripts"""

    @property
    def name(self):
        raise Exception("not implemented")

    @property
    def parent(self):
        return None

    @property
    def readme_file(self):
        if self._parent is not None:
            return self._parent.readme_file
        else:
            return None

    @property
    def rel_path(self):
        if self._parent is not None:
            return self._parent.rel_path + self.name + '/'
        else:
            # Use posix path: is used to generate ada_source_path
            return '%s/' % self.name

    @property
    def loaders(self):
        """If the bsp supports various loading mechanisms, then the list
        should be returned here."""
        if self._parent is not None:
            return self._parent.loaders
        else:
            return None

    @property
    def compiler_switches(self):
        return None

    @property
    def c_switches(self):
        return None

    @property
    def add_linker_section(self):
        return True

    def has_c(self, dir):
        if dir.startswith(self.rel_path):
            d = dir.replace(self.rel_path + 'src/', '')
            return d in self.c_srcs
        else:
            return self._parent.has_c(dir)

    def has_asm(self, dir):
        if dir.startswith(self.rel_path):
            d = dir.replace(self.rel_path + 'src/', '')
            return d in self.asm_srcs
        else:
            return self._parent.has_asm(dir)

    def add_sources(self, dir, sources):
        super(BSP, self).add_sources(dir, sources)
        if 'gnarl' in dir:
            if dir not in self.gnarl_dirs:
                self.gnarl_dirs.append(dir)
        else:
            if dir not in self.source_dirs:
                self.source_dirs.append(dir)

    def add_linker_script(self, script, loader=''):
        """Adds a new linker script to the BSP.

        If loader is empty string, then the script is copied to the link folder
        but not used in the runtime.xml (considered a dependency to other
        scripts)
        If loader is None, then the script is applicable whatever the current
        loader used.
        """
        assert isinstance(script, basestring), "invalid parameter"
        base = os.path.basename(script)

        for val in self.ld_scripts:
            assert val['name'] != base, \
                "Cannot have ld scripts with the same name: %s" % base

        self.ld_scripts.insert(0, {
            'name': base,
            'pair': script,
            'path': self.rel_path + 'link',
            'loader': loader})

    def add_linker_switch(self, switch, loader=None):
        """Adds additional linker switch to the BSP.

        if loader is None, then the switch is applicable whatever the current
        loader used.
        """
        self.ld_switches.append({
            'switch': switch,
            'loader': loader})

    def __init__(self):
        super(BSP, self).__init__()
        if self.parent is None:
            self._parent = None
            self.ld_scripts = []
            self.ld_switches = []
        else:
            self._parent = self.parent()
            self.ld_scripts = copy.deepcopy(self._parent.ld_scripts)
            self.ld_switches = copy.deepcopy(self._parent.ld_switches)
        self.source_dirs = []
        self.gnarl_dirs = []

    def install_ld_scripts(self, destination, files, installed_files):
        for val in self.ld_scripts:
            rel = val['path']
            destdir = os.path.join(destination, rel)

            if not os.path.exists(destdir):
                os.makedirs(destdir)
            self._copy_pair(dst=val['name'], srcfile=val['pair'],
                            destdir=destdir,
                            installed_files=installed_files)
            files.append(rel + '/' + val['name'])

    def install_libgnat(self, dest, dirs, installed_files):
        if self._parent is not None:
            self._parent.install_libgnat(dest, dirs, installed_files)

        for d in self.source_dirs:
            rel = self._install(d, dest, installed_files)
            dirs.append(rel)

    def install_libgnarl(self, dest, dirs, installed_files):
        if self._parent is not None:
            self._parent.install_libgnarl(dest, dirs, installed_files)

        for d in self.gnarl_dirs:
            rel = self._install(d, dest, installed_files)
            dirs.append(rel)

    def _install(self, dirname, destination, installed_files):
        if dirname not in self.dirs:
            print('undefined bsp directory %s' % dirname)

        rel = self.rel_path + 'src/' + dirname

        destdir = os.path.join(destination, rel)

        if not os.path.exists(destdir):
            os.makedirs(destdir)

        for k, v in self.dirs[dirname].items():
            self._copy_pair(dst=k, srcfile=v, destdir=destdir,
                            installed_files=installed_files)

        return rel
