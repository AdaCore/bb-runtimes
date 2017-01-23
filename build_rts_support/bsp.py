import copy
import os

from config import Config
from files_holder import SharedFilesHolder


class BSP(SharedFilesHolder):
    """Handles the startup files and linker scripts"""

    @property
    def name(self):
        raise Exception("not implemented")

    @property
    def parent(self):
        return None

    @property
    def readme_file(self):
        return None

    @property
    def rel_path(self):
        if self._arch is not None:
            return self._arch.rel_path + self.name + '/'
        elif Config.create_common:
            # Use posix path: is used to generate ada_source_path
            return '../%s/%s/' % (Config.bsp_sources, self.name)
        else:
            return ''

    @property
    def loaders(self):
        """If the bsp supports various loading mechanisms, then the list
        should be returned here."""
        if self._arch is not None:
            return self._arch.loaders
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
            d = dir.replace(self.rel_path, '')
            return d in self.c_srcs
        else:
            return self._arch.has_c(dir)

    def has_asm(self, dir):
        if dir.startswith(self.rel_path):
            d = dir.replace(self.rel_path, '')
            return d in self.asm_srcs
        else:
            return self._arch.has_asm(dir)

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
        if isinstance(script, basestring):
            base = os.path.basename(script)
            pair = script
        elif isinstance(script, dict):
            assert len(script) == 1, "invalid script parameter"
            for k, v in script.items():
                base = k
                pair = v

        for script in self.ld_scripts:
            assert script['name'] != base, \
                "Cannot have ld scripts with the same name: %s" % base

        self.ld_scripts.insert(0, {
            'name': base,
            'path': self.rel_path + 'link',
            'pair': pair,
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
            self._arch = None
            self.ld_scripts = []
            self.ld_switches = []
        else:
            self._arch = self.parent()
            self.ld_scripts = copy.deepcopy(self._arch.ld_scripts)
            self.ld_switches = copy.deepcopy(self._arch.ld_switches)
        self.source_dirs = []
        self.gnarl_dirs = []

    def install_ld_scripts(self, destination, installed_files):
        for val in self.ld_scripts:
            rel = val['path']
            destdir = os.path.join(destination, rel)

            if not os.path.exists(destdir):
                os.makedirs(destdir)
            self._copy_pair(dst=val['name'], srcfile=val['pair'],
                            destdir=destdir,
                            installed_files=installed_files)

    def install_libgnat(self, dest, dirs, installed_files):
        if self._arch is not None:
            self._arch.install_libgnat(dest, dirs, installed_files)

        for d in self.source_dirs:
            rel = self.install(d, dest, installed_files)
            dirs.append(rel)

    def install_libgnarl(self, dest, dirs, installed_files):
        if self._arch is not None:
            self._arch.install_libgnarl(dest, dirs, installed_files)

        for d in self.gnarl_dirs:
            rel = self.install(d, dest, installed_files)
            dirs.append(rel)

    def install(self, dirname, destination, installed_files):
        if dirname not in self.dirs:
            print('undefined bsp directory %s' % dirname)

        rel = self.rel_path + dirname

        destdir = os.path.join(destination, rel)

        if not os.path.exists(destdir):
            os.makedirs(destdir)

        for k, v in self.dirs[dirname].items():
            self._copy_pair(dst=k, srcfile=v, destdir=destdir,
                            installed_files=installed_files)

        return rel

    def runtime_xml(self):
        ret = """<?xml version="1.0" ?>

<gprconfig>
  <configuration>
   <config>
"""
        if self.loaders is not None:
            ret += '   type Loaders is ("%s");\n' % ('", "').join(self.loaders)
            ret += '   Loader : Loaders := External("LOADER", "%s");\n' % (
                self.loaders[0])
        else:
            ret += '   LDSCRIPT := external ("LDSCRIPT",\n'
            ret += '                         "%s");\n' % (
                "${RUNTIME_DIR(ada)}/" + self.ld_scripts[0]['path'] +
                "/" + self.ld_scripts[0]['name'])

        ret += "\n"
        ret += "   package Compiler is\n"
        if self.compiler_switches is not None:
            ret += '      Common_Required_Switches := ("%s");\n' % \
                   ('", "').join(self.compiler_switches)
        else:
            ret += '      Common_Required_Switches := ();\n'

        if self.c_switches is not None:
            ret += '      C_Required_Switches := ("%s");\n' % \
                   ('", "').join(self.c_switches)

        ret += '\n'

        for lang in ('Ada', 'C', 'Asm', 'Asm2', 'Asm_Cpp'):
            w = "      "
            ret += w + 'for Leading_Required_Switches ("%s") use\n' % lang
            w = "         "
            ret += w + 'Compiler\'Leading_Required_Switches ("%s") &amp;\n' % \
                lang
            ret += w + "Common_Required_Switches"
            if lang != 'Ada' and self.c_switches is not None:
                ret += " &amp;\n" + w
                ret += "C_Required_Switches"
            ret += ";\n"
        ret += "   end Compiler;\n"

        if not self.add_linker_section:
            ret += """
   </config>
  </configuration>
</gprconfig>
"""
            return ret

        switches = []
        for val in self.ld_scripts:
            if val['loader'] is None:
                # use for all loaders
                switches.append('"-T" &amp; LDSCRIPT')
        for sw in self.ld_switches:
            if sw['loader'] is None or sw['loader'] == '':
                switches.append('"%s"' % sw['switch'])

        ret += """
   package Linker is
      for Required_Switches use Linker'Required_Switches &amp;
        ("-L${RUNTIME_DIR(ada)}/adalib", "-nolibc", "-nostartfiles\""""
        indent = 9
        blank = indent * ' '

        link_paths = []
        for val in self.ld_scripts:
            if val['path'] not in link_paths:
                link_paths.append(val['path'])
        for p in sorted(link_paths):
            ret += ',\n' + blank
            ret += '"-L${RUNTIME_DIR(ada)}/%s"' % p

        if len(switches) > 0:
            ret += ',\n'
            ret += blank
            ret += (',\n' + blank).join(switches)
            blank = indent * ' '
        ret += ') &amp;\n' + blank + "Compiler.Common_Required_Switches;\n"
        indent = 6
        blank = indent * ' '

        if self.loaders is not None:
            ret += '\n' + blank
            ret += 'case Loader is\n'
            indent += 3
            blank = indent * ' '

            for l in self.loaders:
                ret += blank
                ret += 'when "%s" =>\n' % l
                indent += 3
                blank = indent * ' '

                switches = []
                for val in self.ld_scripts:
                    if val['loader'] is None:
                        continue
                    if isinstance(val['loader'], basestring):
                        if val['loader'] == l:
                            switches.append('"-T", "%s"' % val['name'])
                    else:
                        if l in val['loader']:
                            switches.append('"-T", "%s"' % val['name'])
                for sw in self.ld_switches:
                    if isinstance(sw['loader'], basestring) \
                       and sw['loader'] == l:
                        switches.append('"%s"' % sw['switch'])
                    if isinstance(sw['loader'], list) \
                       and l in sw['loader']:
                        switches.append('"%s"' % sw['switch'])
                if len(switches) > 0:
                    ret += blank
                    ret += "for Required_Switches use Linker'Required_Switches"
                    ret += " &amp;\n" + blank + '  '
                    ret += "(%s);\n" % (',\n   ' + blank).join(switches)
                indent -= 3
                blank = indent * ' '

            indent -= 3
            blank = indent * ' '
            ret += "%send case;" % blank

        ret += """
   end Linker;
   </config>
  </configuration>
</gprconfig>
"""
        return ret
