# BSP support for Sparc/Leon
from build_rts_support import readfile
from build_rts_support.bsp import BSP
from build_rts_support.target import DFBBTarget
from build_rts_support.config import Config

import os


class LeonArch(BSP):
    @property
    def name(self):
        return "leon"

    def __init__(self):
        super(LeonArch, self).__init__()
        self.add_linker_switch('-Wl,-u_start', loader=None)
        self.add_sources('arch', [
            'leon-elf/crt0.S',
            'sparc/leon/hw_init.S',
            {'s-macres.adb': 's-macres-leon.adb',
             'sparc.h': 'sparc-bb.h'}])
        self.add_sources('gnarl', [
            {'s-bbcppr.adb': 's-bbcppr-sparc.adb',
             's-bcpith.adb': 's-bcpith-bb-sparc.adb',
             'context_switch.S': 'context_switch-bb-sparc.S',
             'trap_handler.S': 'trap_handler-bb-sparc.S',
             'interrupt_masking.S': 'interrupt_masking-bb-sparc.S',
             'floating_point.S': 'floating_point-bb-sparc.S',
             's-bbcaco.adb': 's-bbcaco-leon.adb'},
            's-bbcaco.ads',
            's-bbcppr.ads',
            's-bbinte.adb'])


class LeonTarget(DFBBTarget):
    @property
    def has_newlib(self):
        return True

    def __init__(self):
        super(LeonTarget, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self):
        super(LeonTarget, self).amend_zfp()
        self.update_pairs({'system.ads': 'system-xi-sparc.ads'})
        self.config_files['runtime.xml'] = \
            self.config_files['runtime.xml'].replace(
                '"-nolibc", ', '')

    def amend_ravenscar_sfp(self):
        super(LeonTarget, self).amend_ravenscar_sfp()
        self.update_pairs({'system.ads': 'system-xi-sparc-ravenscar.ads'})

    def amend_ravenscar_full(self):
        super(LeonTarget, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-xi-sparc-full.ads'})
        # Use leon-zcx.specs to link with -lc.
        self.config_files.update(
            {'link-zcx.spec':
             readfile(os.path.join(Config.crossdir,
                                   'leon-elf/leon-zcx.specs'))})
        self.config_files['runtime.xml'] = \
            self.config_files['runtime.xml'].replace(
                '"-nostartfiles",',
                '"--specs=${RUNTIME_DIR(ada)}/link-zcx.spec",')
