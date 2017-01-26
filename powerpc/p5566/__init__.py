from powerpc import PPCSPETarget


class P5566(PPCSPETarget):
    @property
    def name(self):
        return 'p5566'

    @property
    def compiler_switches(self):
        return ('-mfloat-gprs=single', )

    @property
    def loaders(self):
        return ('EXTRAM', 'BAM', 'FLASH')

    def __init__(self):
        super(P5566, self).__init__()
        self.add_linker_script('powerpc/p5566/bam.ld', loader='BAM')
        self.add_linker_script('powerpc/p5566/flash.ld', loader='FLASH')
        self.add_linker_script('powerpc/p5566/ram.ld', loader='EXTRAM')
        self.add_sources('crt0', [
            'powerpc/p5566/start-bam.S',
            'powerpc/p5566/start-ram.S',
            'powerpc/p5566/start-flash.S',
            'powerpc/p5566/setup.S',
            'powerpc/p5566/setup-pll.S',
            {'s-macres.adb': 's-macres-p55.adb',
             's-textio.adb': 's-textio-p55.adb'}])
        self.add_sources('gnarl', [
            's-bbsumu.adb',
            {'s-bbbopa.ads': 's-bbbopa-p55.ads',
             's-bbbosu.adb': 's-bbbosu-p55.adb',
             's-bbsuti.adb': 's-bbsuti-ppc.adb',
             's-bbpara.ads': 's-bbpara-p55.ads',
             'a-intnam.ads': 'a-intnam-xi-p55.ads'}])
